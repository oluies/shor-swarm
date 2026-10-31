# shor-swarm

Shor's algorithm, simulated one shot per browser. Everyone who opens the page joins the swarm: their
browser runs shots of Shor's period finding in Web Workers and sends back what it measured, and the
server collects the shots, recovers a period with continued fractions and factors the semiprime. Then
it moves on to a bigger one.

**This is a classical simulation, not a quantum computer.** Each shot runs Scotty's
[`IterativeShor`](https://oluies.github.io/scotty/algorithms/shor/#shor-on-l--1-qubits) on a state
vector of `2^(L + 1)` amplitudes for an `L`-bit number, so the work grows exponentially with the
number's size, and a laptop factors these numbers instantly by classical means. What the swarm shows is
how Shor's algorithm works and how its shots combine, live, across everyone on the page.

## How it works

- **Shots, not shards.** Every shot is a complete, independent run of period finding, so a browser
  that closes its tab loses nothing: the next idle worker is simply given another shot. Splitting one
  state vector across browsers instead, as Willsch et al. split theirs across 2048 GPUs
  ([arXiv:2308.05047](https://arxiv.org/abs/2308.05047)), would move nearly the whole state between
  browsers at every step and lose it with every closed tab.
- **Nothing is trusted.** A reported `y` only proposes a period: the server keeps it only if
  `a^r = 1 (mod N)`, and a factorisation only if the factors multiply back to `N`. A browser that lies
  wastes shots; it cannot make the swarm claim a wrong answer.
- **Nothing is compiled with the answer.** The semiprimes and bases are random, and bases sharing a
  factor with `N` are skipped rather than used, so every factorisation comes from a measured period,
  never from the classical `gcd` shortcut. See Smolin, Smith and Vargo,
  [arXiv:1301.7007](https://arxiv.org/abs/1301.7007), for why that matters.

## Layout

| module | what it is |
|---|---|
| `shared` | The messages, as uPickle case classes, and Scotty's `IterativeShor`, compiled for both the JVM and Scala.js |
| `server` | A [Cask](https://com-lihaoyi.github.io/cask/) app: the page, the `/ws` WebSocket, and the `Board` that hands out shots and checks the results |
| `client` | Scala.js: one script that is the page and, in Web Workers, the shots |

`IterativeShor` is compiled from a Scotty checkout rather than taken from its published jar, because
Scotty publishes no Scala.js artifact and its GitHub Packages registry needs a token even to read. The
build looks for `$SCOTTY_DIR`, then `./scotty`, then `../scotty`; CI checks out tag `v0.7.1`.

## Running it

```bash
git clone -b v0.7.1 https://github.com/oluies/scotty.git scotty
./mill server.test                   # the board and an end-to-end WebSocket run, on real shots
./mill server.run                    # http://localhost:8080/
./mill server.assembly               # out/server/assembly.dest/out.jar, page and client included
PORT=8080 BITS=16 MAX_BITS=20 java -jar out/server/assembly.dest/out.jar
```

`BITS` is the size of the first semiprime and rises by one per factorisation up to `MAX_BITS`, at most
20: a 20-bit number is a 21-qubit state, 32 MB per worker. Each tab runs up to four workers, half its
cores. To serve it publicly, put it behind TLS; the page connects with `wss:` when it was loaded over
`https:`.

## License

Apache 2.0.
