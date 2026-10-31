package shorswarm

import java.util.concurrent.{ConcurrentHashMap, Executors, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*
import scala.util.{Random, Try}
import upickle.default.{read, write}

/** The swarm's server: the page, the WebSocket every browser joins, and the [[Board]] behind it.
  *
  * A browser sends `Ready` for each idle worker and is answered with a `Job`; it sends `Result` when
  * the shot is done. Every second the server sends each browser the [[Swarm]] it draws. The board
  * is only ever touched under its own lock, from the WebSocket actors and the broadcast thread.
  */
class SwarmServer(board: Board, override val port: Int, override val host: String) extends cask.MainRoutes:
  private val peers = ConcurrentHashMap[Long, cask.WsChannelActor]()
  private val nextPeer = AtomicLong()

  private def onBoard[A](f: Board => A): A = board.synchronized(f(board))

  // Accepts any query parameters, which Cask otherwise rejects: a shared link with ?utm_source=...
  // has to reach the page, not an error.
  @cask.get("/")
  def index(params: cask.model.QueryParams): cask.Response[String] = cask.Redirect("/web/index.html")

  @cask.staticResources("/web")
  def web(): String = "web"

  @cask.websocket("/ws")
  def ws(): cask.WebsocketResult = cask.WsHandler { channel =>
    val id = nextPeer.incrementAndGet()
    peers.put(id, channel)

    def send(message: ToClient): Unit = channel.send(cask.Ws.Text(write(message)))

    cask.WsActor {
      case cask.Ws.Text(text) =>
        Try(read[ToServer](text)).foreach {
          case ToServer.Ready                => send(onBoard(_.issue()))
          case ToServer.Result(job, y, _)    => onBoard(_.submit(job, y))
        }
      case cask.Ws.Close(_, _) | cask.Ws.ChannelClosed() => peers.remove(id)
    }
  }

  /** Sends every browser the swarm's state, once. */
  def broadcast(): Unit =
    val message = write[ToClient](ToClient.Status(onBoard(_.snapshot(peers.size))))
    peers.values.asScala.foreach(peer => Try(peer.send(cask.Ws.Text(message))))

  private val ticker: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(r =>
    val thread = Thread(r, "shor-swarm-broadcast")
    thread.setDaemon(true)
    thread
  )

  ticker.scheduleAtFixedRate(() => Try(broadcast()), 1, 1, TimeUnit.SECONDS)

  /** Starts the server without registering Cask's shutdown hook, so a test can stop it. */
  def start(): io.undertow.Undertow =
    val server = io.undertow.Undertow.builder.addHttpListener(port, host).setHandler(defaultHandler).build
    server.start()
    server

  def stop(server: io.undertow.Undertow): Unit =
    ticker.shutdownNow()
    server.stop()

  initialize()

object Server:
  /** `PORT` (8080), `HOST` (0.0.0.0), and the sizes of the semiprimes: `BITS` (16) to start, rising
    * by one per factorisation up to `MAX_BITS` (20).
    */
  def main(args: Array[String]): Unit =
    val env = sys.env
    val bits = env.get("BITS").map(_.toInt).getOrElse(16)
    val board = Board(Random(), bits, env.get("MAX_BITS").map(_.toInt).getOrElse(20).max(bits))
    val app = SwarmServer(board, env.get("PORT").map(_.toInt).getOrElse(8080), env.getOrElse("HOST", "0.0.0.0"))

    app.start()
    println(s"shor-swarm on http://${app.host}:${app.port}/, factoring ${bits}-bit semiprimes")
