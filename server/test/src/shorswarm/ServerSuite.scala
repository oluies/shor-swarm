package shorswarm

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.util.concurrent.{CompletionStage, LinkedBlockingQueue, TimeUnit}
import scala.util.Random
import scotty.algorithm.IterativeShor
import upickle.default.{read, write}

/** The server end to end: a real Undertow on a free port, driven over a real WebSocket. */
class ServerSuite extends munit.FunSuite:
  private def freePort(): Int =
    val socket = java.net.ServerSocket(0)
    try socket.getLocalPort finally socket.close()

  private final class Browser(port: Int):
    val inbox = LinkedBlockingQueue[ToClient]()
    private val text = StringBuilder()

    val socket: WebSocket = HttpClient.newHttpClient
      .newWebSocketBuilder()
      .buildAsync(
        URI.create(s"ws://localhost:$port/ws"),
        new WebSocket.Listener:
          override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
            text.append(data)
            if last then
              inbox.put(read[ToClient](text.toString))
              text.clear()
            ws.request(1)
            null
      )
      .get(5, TimeUnit.SECONDS)

    def send(message: ToServer): Unit = socket.sendText(write(message), true).get(5, TimeUnit.SECONDS)

    def next(): ToClient = Option(inbox.poll(10, TimeUnit.SECONDS)).getOrElse(fail("no message from the server"))

    def nextJob(): ToClient.Job = Iterator.continually(next()).collectFirst { case j: ToClient.Job => j }.get

  test("two browsers factor a semiprime over the WebSocket, and every browser hears about it") {
    val port = freePort()
    val app = SwarmServer(Board(Random(11), 10, 10), port, "localhost")
    val server = app.start()

    try
      val (alice, bob) = (Browser(port), Browser(port))
      val random = Random(12)
      var factored: Option[Factored] = None
      var shots = 0

      while factored.isEmpty && shots < 400 do
        for browser <- Seq(alice, bob) do
          browser.send(ToServer.Ready)
          val job = browser.nextJob()
          browser.send(ToServer.Result(job.jobId, IterativeShor.PeriodFinder(job.a, job.n, job.t).sample(random), 1))
          shots += 1
        app.broadcast()
        factored = Iterator.continually(alice.next()).collectFirst { case ToClient.Status(s) => s }.flatMap(_.factored.headOption)

      val f = factored.getOrElse(fail(s"nothing factored in $shots shots"))
      assertEquals(f.p * f.q, f.n)

      app.broadcast()
      val seenByBob = Iterator.continually(bob.next()).collectFirst { case ToClient.Status(s) => s }.get
      assertEquals(seenByBob.peers, 2)
      assert(seenByBob.factored.exists(_.n == f.n))
    finally app.stop(server)
  }

  test("the page and the linked client are served") {
    val port = freePort()
    val app = SwarmServer(Board(Random(13), 10, 10), port, "localhost")
    val server = app.start()
    val http = HttpClient.newBuilder.followRedirects(HttpClient.Redirect.NORMAL).build

    def get(path: String) = http.send(HttpRequest.newBuilder(URI.create(s"http://localhost:$port$path")).build, HttpResponse.BodyHandlers.ofString)

    try
      val page = get("/")
      assertEquals(page.statusCode, 200)
      assert(page.body.contains("Shor swarm"))
      // A shared link with tracking parameters still reaches the page.
      assertEquals(get("/?utm_source=chat&v=2").statusCode, 200)
      val script = get("/web/main.js")
      assertEquals(script.statusCode, 200)
      assert(script.body.contains("IterativeShor") || script.body.length > 10000, "main.js is the linked client")
    finally app.stop(server)
  }
