package shorswarm

import upickle.default.*

/** What a browser sends the server. */
enum ToServer derives ReadWriter:
  /** A worker is idle and can take a shot. */
  case Ready

  /** The measured `y` of the shot `jobId`, and how long the simulation took. */
  case Result(jobId: Long, y: Long, millis: Double)

/** What the server sends a browser. */
enum ToClient derives ReadWriter:
  /** One shot: run Shor's period finding for `a` modulo `n` with `t` counting bits and report `y`. */
  case Job(jobId: Long, n: Long, a: Long, t: Int)

  /** The state of the swarm, sent to every browser about once a second. */
  case Status(swarm: Swarm)

/** One base being tried: its shots so far, binned for display, and the period once one checks out. */
final case class BaseRun(a: Long, shots: Int, bins: Seq[Int], period: Option[Long], note: String) derives ReadWriter

/** A modulus factored by the swarm, through the period of `a`. */
final case class Factored(n: Long, p: Long, q: Long, a: Long, period: Long, shots: Int, millis: Long) derives ReadWriter

/** The swarm as the page shows it. */
final case class Swarm(
    n: Long,
    bits: Int,
    t: Int,
    peers: Int,
    workers: Int,
    shots: Long,
    runs: Seq[BaseRun],
    factored: Seq[Factored]
) derives ReadWriter

object Protocol:
  /** Bins the histogram is drawn with: `y / 2^t` in `[0, 1)`, split evenly. */
  val Bins: Int = 64

  def bin(y: Long, t: Int): Int = ((y.toDouble / (1L << t)) * Bins).toInt.min(Bins - 1).max(0)
