package shorswarm

import org.scalajs.dom
import scala.collection.mutable
import scala.scalajs.js
import scala.util.{Random, Try}
import scotty.algorithm.IterativeShor
import upickle.default.{read, write}

/** One script for the page and its Web Workers: it runs as a worker when there is no `document`. */
object Main:
  def main(args: Array[String]): Unit =
    if js.typeOf(js.Dynamic.global.document) == "undefined" then Worker.run() else Page.run()

/** A worker: takes one shot at a time, simulates it with Scotty's `IterativeShor`, posts back `y`. */
object Worker:
  def run(): Unit =
    // The worker's global scope, reached through its `self` property: Scala.js forbids holding the
    // global scope itself as a value.
    val self = js.Dynamic.global.self
    val random = Random()

    self.onmessage = { (event: dom.MessageEvent) =>
      read[ToClient](event.data.asInstanceOf[String]) match
        case ToClient.Job(jobId, n, a, t) =>
          val start = js.Date.now()
          val y = IterativeShor.PeriodFinder(a, n, t).sample(random)
          self.postMessage(write[ToServer](ToServer.Result(jobId, y, js.Date.now() - start)))
        case _ => ()
    }

/** The page: one WebSocket to the server, a few workers, and the swarm drawn once a second. */
object Page:
  // This script's own URL, which every worker loads; only valid while the script first runs.
  private val script = dom.document.asInstanceOf[js.Dynamic].currentScript.src.asInstanceOf[String]
  private val cores = js.Dynamic.global.navigator.hardwareConcurrency.asInstanceOf[js.UndefOr[Int]].getOrElse(2)
  private val workerCount = (cores / 2).max(1).min(4)

  private val workers: Vector[dom.Worker] = Vector.fill(workerCount)(new dom.Worker(script))
  private val idle: mutable.Queue[dom.Worker] = mutable.Queue.from(workers)
  private val busy = mutable.Map.empty[dom.Worker, Long]
  private var socket: Option[dom.WebSocket] = None
  private var paused = false
  private var mine = 0L
  private var lastMillis = 0d

  private def el(id: String): dom.HTMLElement = dom.document.getElementById(id).asInstanceOf[dom.HTMLElement]

  def run(): Unit =
    workers.foreach: worker =>
      worker.onmessage = { (event: dom.MessageEvent) =>
        val result = read[ToServer](event.data.asInstanceOf[String])
        result match
          case ToServer.Result(_, _, millis) => mine += 1; lastMillis = millis
          case _                             => ()
        busy.remove(worker)
        idle.enqueue(worker)
        send(result)
        askForWork()
        showMine()
      }

    el("pause").addEventListener("click", (_: dom.Event) => togglePause())
    el("workers").textContent = s"$workerCount"
    connect()

  private def connect(): Unit =
    val scheme = if dom.window.location.protocol == "https:" then "wss" else "ws"
    val ws = new dom.WebSocket(s"$scheme://${dom.window.location.host}/ws")

    ws.onopen = (_: dom.Event) =>
      socket = Some(ws)
      el("connection").textContent = "connected"
      askForWork()
    ws.onmessage = (event: dom.MessageEvent) =>
      Try(read[ToClient](event.data.asInstanceOf[String])).foreach:
        case job: ToClient.Job =>
          pending = (pending - 1).max(0)
          // A job with no idle worker to take it (a reconnect race) is dropped; the server simply
          // hands that shot to someone else once its lease runs out.
          if idle.nonEmpty then
            val worker = idle.dequeue()
            busy(worker) = job.jobId
            worker.postMessage(write[ToClient](job))
        case ToClient.Status(swarm) => draw(swarm)
    ws.onclose = (_: dom.CloseEvent) =>
      socket = None
      el("connection").textContent = "reconnecting"
      // Shots in flight are lost with the connection; the server simply hands out others.
      busy.keys.foreach(idle.enqueue)
      busy.clear()
      pending = 0
      dom.window.setTimeout(() => connect(), 2000)

  private def send(message: ToServer): Unit =
    socket.filter(_.readyState == dom.WebSocket.OPEN).foreach(_.send(write(message)))

  /** One `Ready` per idle worker that has not already asked. */
  private def askForWork(): Unit =
    if !paused then
      val asking = idle.size - pending
      (0 until asking).foreach(_ => send(ToServer.Ready))
      pending += asking.max(0)

  // Ready messages sent and not yet answered with a job; a job answers one.
  private var pending = 0

  private def togglePause(): Unit =
    paused = !paused
    el("pause").textContent = if paused then "Resume" else "Pause"
    if !paused then askForWork()

  private def showMine(): Unit =
    el("mine").textContent = s"$mine"
    el("last").textContent = f"$lastMillis%.0f ms"

  private def draw(swarm: Swarm): Unit =
    el("n").textContent = s"${swarm.n}"
    el("bits").textContent = s"${swarm.bits}"
    el("qubits").textContent = s"${IterativeShor.width(swarm.n) + 1}"
    el("t").textContent = s"${swarm.t}"
    el("peers").textContent = s"${swarm.peers}"
    el("inflight").textContent = s"${swarm.workers}"
    el("shots").textContent = s"${swarm.shots}"
    el("runs").innerHTML = swarm.runs.reverse.map(runCard).mkString
    el("factored").innerHTML =
      if swarm.factored.isEmpty then "<li class=\"muted\">Nothing yet.</li>"
      else swarm.factored.map(f =>
        s"<li><b>${f.n} = ${f.p} &times; ${f.q}</b> <span class=\"muted\">through the period ${f.period} of a = ${f.a}, " +
          s"${plural(f.shots, "shot")}, ${f.millis / 1000} s</span></li>").mkString

  private def plural(count: Long, noun: String): String = s"$count $noun${if count == 1 then "" else "s"}"

  private def runCard(run: BaseRun): String =
    val top = run.bins.maxOption.filter(_ > 0).getOrElse(1)
    val width = 100.0 / run.bins.size
    val bars = run.bins.zipWithIndex.map: (count, i) =>
      val h = 60.0 * count / top
      f"""<rect x="${i * width}%.3f" y="${60 - h}%.3f" width="${width * 0.9}%.3f" height="$h%.3f"/>"""
    val status = run.period.map(r => s"period $r").getOrElse(if run.note.nonEmpty then run.note else "measuring")
    val note = if run.period.nonEmpty && run.note.nonEmpty then s" &middot; ${run.note}" else ""
    s"""<div class="run"><div class="run-head"><b>a = ${run.a}</b><span class="muted">${plural(run.shots, "shot")} &middot; $status$note</span></div>
       |<svg viewBox="0 0 100 60" preserveAspectRatio="none" role="img" aria-label="histogram of y / 2^t for a = ${run.a}">${bars.mkString}</svg></div>""".stripMargin
