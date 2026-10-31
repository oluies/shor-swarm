package shorswarm

import scala.util.Random
import scotty.algorithm.IterativeShor
import upickle.default.{read, write}

class BoardSuite extends munit.FunSuite:
  /** A worker on the JVM: one real IterativeShor shot for the job. */
  private def shoot(job: ToClient.Job, random: Random): Long = IterativeShor.PeriodFinder(job.a, job.n, job.t).sample(random)

  test("messages survive a round trip through JSON") {
    val messages: Seq[ToClient] = Seq(
      ToClient.Job(7, 143, 12, 16),
      ToClient.Status(Swarm(143, 8, 16, 3, 2, 40, Seq(BaseRun(12, 5, Seq.fill(Protocol.Bins)(1), Some(2), "")), Seq(Factored(143, 11, 13, 12, 2, 5, 900))))
    )
    messages.foreach(m => assertEquals(read[ToClient](write(m)), m))
    Seq[ToServer](ToServer.Ready, ToServer.Result(7, 1234, 3.5)).foreach(m => assertEquals(read[ToServer](write(m)), m))
  }

  test("the board starts on a semiprime of the requested size, with a coprime base") {
    val board = Board(Random(1), 12, 14)
    val job = board.issue(1).get

    assertEquals(java.lang.Long.numberOfLeadingZeros(job.n), 64 - 12)
    assertEquals(job.t, 2 * IterativeShor.width(job.n))
    assertEquals(BigInt(job.a).gcd(job.n), BigInt(1))
  }

  test("real shots factor one semiprime after another, each bigger, each correct") {
    val random = Random(2)
    val board = Board(Random(3), 10, 12)
    var factored = Vector.empty[Factored]
    var shots = 0

    while factored.size < 3 && shots < 2000 do
      val job = board.issue(1).get
      board.submit(job.jobId, shoot(job, random)).foreach(f => factored :+= f)
      shots += 1

    assertEquals(factored.size, 3, s"only ${factored.size} factored in $shots shots")
    factored.foreach: f =>
      assertEquals(f.p * f.q, f.n)
      assert(f.p > 1 && f.q > 1 && f.p < f.q)
      assertEquals(BigInt(f.a).modPow(f.period, f.n), BigInt(1), "the period is a period")
    assertEquals(factored.map(f => 64 - java.lang.Long.numberOfLeadingZeros(f.n)), Vector(10, 11, 12))
    assertEquals(board.snapshot(0).factored.map(_.n), factored.reverse.map(_.n))
  }

  test("a result that is unknown, out of range, repeated or for an earlier modulus is ignored") {
    val board = Board(Random(4), 10, 12)
    val job = board.issue(1).get

    assertEquals(board.submit(job.jobId + 99, 1), None)
    assertEquals(board.submit(job.jobId, 1L << job.t), None)
    assertEquals(board.snapshot(0).shots, 0L, "out of range was not counted")
    // That submission spent the lease, so the same id again is unknown.
    assertEquals(board.submit(job.jobId, 0), None)
    assertEquals(board.snapshot(0).shots, 0L)
  }

  test("leases run out, so a closed tab's shot leaves the books") {
    var clock = 0L
    val board = Board(Random(5), 10, 12, () => clock)
    val job = board.issue(1).get

    assertEquals(board.snapshot(1).workers, 1)
    clock += board.leaseMillis + 1
    assertEquals(board.snapshot(1).workers, 0)
    assertEquals(board.submit(job.jobId, 0), None, "an expired shot's result is not taken")
  }

  test("lies cannot make it claim a wrong factorisation") {
    // Every reported y is random noise. The board may still stumble on a true period, since the
    // check is classical and anything that passes it is a real period, but whatever it reports
    // has to multiply back to N.
    val noise = Random(6)
    val board = Board(Random(7), 10, 12)

    (0 until 3000).foreach: _ =>
      val job = board.issue(1).get
      board.submit(job.jobId, noise.nextLong(1L << job.t)).foreach: f =>
        assertEquals(f.p * f.q, f.n)
        assertEquals(BigInt(f.a).modPow(f.period, f.n), BigInt(1))
  }

  test("late results for an abandoned base do not start more bases") {
    // y = 0 gives no period, so a base given only zeros is abandoned after shotsPerBase shots. The
    // shots still in flight then arrive late, and must not each start another base.
    val board = Board(Random(8), 10, 12)
    val first = board.snapshot(0).runs.head.a
    // Spread over enough browsers that none exceeds its share of leases.
    val jobs = (0 until board.shotsPerBase + 6).map(i => board.issue(1 + i / board.leasesPerPeer).get)
    jobs.foreach(job => board.submit(job.jobId, 0))

    val runs = board.snapshot(0).runs
    assertEquals(runs.map(_.a).distinct.size, 2, s"bases now: ${runs.map(_.a)}")
    assertEquals(runs.head.a, first)
    assertEquals(runs.head.shots, board.shotsPerBase, "late results counted toward the abandoned base")
  }

  test("a board of 4 bits is refused, since no 4-bit semiprime of two odd primes exists") {
    intercept[IllegalArgumentException](Board(Random(9), 4, 8))
    assertEquals(Board(Random(9), 5, 8).n, 21L)
  }

  test("one browser cannot hold more than its share of leases") {
    val board = Board(Random(10), 10, 12)
    val granted = (0 until 100).flatMap(_ => board.issue(peer = 1))

    assertEquals(granted.size, board.leasesPerPeer)
    assert(board.issue(peer = 2).isDefined, "another browser still gets work")
    board.submit(granted.head.jobId, 0)
    assert(board.issue(peer = 1).isDefined, "a result frees a lease")
    board.release(peer = 1)
    assertEquals((0 until board.leasesPerPeer).flatMap(_ => board.issue(peer = 1)).size, board.leasesPerPeer, "a closed connection frees them all")
  }
