package shorswarm

import scala.collection.mutable
import scala.util.Random
import scotty.algorithm.IterativeShor

/** The swarm's shared work: the semiprime being factored, the bases tried on it, the shots out.
  *
  * Every shot of one base is interchangeable, so a shot lost with a closed tab costs nothing: the
  * next idle worker is simply given another. Leases keep the books bounded: a browser holds at most
  * [[leasesPerPeer]], the most workers a tab runs, so one that asks for work in a loop cannot grow
  * them; they expire oldest first after [[leaseMillis]]; and a closed connection releases its own.
  * A result for a shot that is unknown, expired or for an earlier modulus is ignored, and one for a
  * base already given up is counted but no longer changes anything.
  *
  * Nothing here trusts a browser. A reported `y` only proposes a period: [[IterativeShor.period]]
  * keeps it only if `a^r = 1 (mod N)`, and [[IterativeShor.factorFromPeriod]] only if the factors it
  * gives multiply back to `N`. A browser that lies wastes shots; it cannot make the swarm claim a
  * wrong factorisation.
  *
  * Bases that share a factor with `N` are skipped rather than used, so every factorisation the swarm
  * reports comes from a period, not from the classical `gcd` shortcut.
  *
  * Not thread-safe on its own; [[Server]] calls it under a lock.
  */
final class Board(random: Random, startBits: Int, maxBits: Int, now: () => Long = () => System.currentTimeMillis()):
  // 5 bits is the smallest with a semiprime of two distinct odd primes, 21 = 3 * 7; at 4 the only
  // primes of two bits are 2 and 3, and the search for one would never end.
  require(startBits >= 5 && startBits <= maxBits, s"bits have to run from 5 up, not $startBits to $maxBits")
  // IterativeShor holds 2^(bits + 1) amplitudes; 20 bits is 32 MB a worker, a sensible browser cap.
  require(maxBits <= 20, s"$maxBits bits is more than a browser tab should hold")

  /** Shots a base gets before it is abandoned for another: about how many a period needs, with room. */
  val shotsPerBase: Int = 24

  /** How long a shot stays leased before its result is no longer expected. */
  val leaseMillis: Long = 60000

  /** Shots one browser may hold at once: the most workers a tab runs. */
  val leasesPerPeer: Int = Protocol.MaxWorkers

  private final class Run(val a: Long):
    var shots = 0
    val bins = Array.fill(Protocol.Bins)(0)
    var period: Option[Long] = None
    var note = ""

    def view: BaseRun = BaseRun(a, shots, bins.toSeq, period, note)

  private final case class Lease(peer: Long, n: Long, a: Long, t: Int, issued: Long)

  private var bits = startBits
  private var started = now()
  private var target = semiprime(bits)
  private var runs = Vector(Run(newBase()))
  // In issue order, which is time order, so expiring drops from the head instead of scanning.
  private val leases = mutable.LinkedHashMap.empty[Long, Lease]
  private val held = mutable.Map.empty[Long, Int].withDefaultValue(0)
  private var nextJob = 0L
  private var totalShots = 0L
  private val done = mutable.ArrayBuffer.empty[Factored]

  def n: Long = target._1

  def t: Int = 2 * IterativeShor.width(n)

  private def active: Run = runs.last

  /** Hands out one shot of the base being tried, unless `peer` already holds its share. */
  def issue(peer: Long): Option[ToClient.Job] =
    expire()
    Option.when(held(peer) < leasesPerPeer):
      nextJob += 1
      leases(nextJob) = Lease(peer, n, active.a, t, now())
      held(peer) += 1
      ToClient.Job(nextJob, n, active.a, t)

  /** A closed connection's shots: its leases are dropped, so its share is free again. */
  def release(peer: Long): Unit =
    leases.filterInPlace((_, lease) => lease.peer != peer)
    held.remove(peer)

  private def drop(jobId: Long): Option[Lease] =
    leases.remove(jobId).map: lease =>
      held(lease.peer) -= 1
      if held(lease.peer) <= 0 then held.remove(lease.peer)
      lease

  /** Takes a measured `y`. Returns the factorisation when this shot completes one. */
  def submit(jobId: Long, y: Long): Option[Factored] =
    drop(jobId) match
      case Some(lease) if lease.n == n && y >= 0 && y < (1L << lease.t) =>
        totalShots += 1
        // Only the base being tried learns from a shot. A late one for a base already given up, by
        // its shot limit or an unusable period, would otherwise give it up again and start another.
        if lease.a == active.a && active.period.isEmpty then record(active, y) else None
      case _ => None

  private def record(run: Run, y: Long): Option[Factored] =
    run.shots += 1
    run.bins(Protocol.bin(y, t)) += 1

    IterativeShor.period(run.a, n, y, t) match
      case Some(r) =>
        run.period = Some(r)
        IterativeShor.factorFromPeriod(run.a, n, r) match
          case Some((p, q)) =>
            val factored = Factored(n, p, q, run.a, r, runs.map(_.shots).sum, now() - started)
            done += factored
            nextTarget()
            Some(factored)
          case None =>
            // The card shows the period beside this, so the note only says why it gave no factors.
            run.note = if r % 2 == 1 then "odd" else s"a^${r / 2} = -1 (mod N)"
            runs = runs :+ Run(newBase())
            None
      case None =>
        if run.shots >= shotsPerBase then
          run.note = s"no period in $shotsPerBase shots"
          runs = runs :+ Run(newBase())
        None

  /** The swarm as the page shows it. */
  def snapshot(peers: Int): Swarm =
    expire()
    Swarm(n, bits, t, peers, leases.size, totalShots, runs.takeRight(4).map(_.view), done.takeRight(20).reverse.toSeq)

  private def expire(): Unit =
    val cutoff = now() - leaseMillis
    while leases.headOption.exists(_._2.issued < cutoff) do drop(leases.head._1)

  private def nextTarget(): Unit =
    bits = (bits + 1).min(maxBits)
    target = semiprime(bits)
    runs = Vector(Run(newBase()))
    leases.clear()
    held.clear()
    started = now()

  /** A random base coprime to `n`; one sharing a factor would factor it with no quantum step. */
  private def newBase(): Long =
    Iterator.continually(2 + random.nextLong(n - 3)).find(a => BigInt(a).gcd(n) == 1).get

  /** A product of two distinct odd primes with exactly `bits` bits. */
  private def semiprime(bits: Int): (Long, Long, Long) =
    Iterator
      .continually:
        val p = BigInt.probablePrime(bits / 2, random.self).toLong
        val q = BigInt.probablePrime(bits - bits / 2, random.self).toLong
        (p * q, p.min(q), p.max(q))
      .find((product, p, q) => p != q && p > 2 && java.lang.Long.numberOfLeadingZeros(product) == 64 - bits)
      .get
