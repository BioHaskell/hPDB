#!/usr/bin/env python3
"""Run a command while holding an fcntl(F_SETLKW) lock on ~/benchmarking.lock.

bench-utils' `withBenchMutex` (System.Bench.Mutex) takes a POSIX fcntl lock on
that file. On Linux flock(2) and fcntl locks are INDEPENDENT spaces, so plain
`flock(1)` does NOT exclude a bench-utils run. `fcntl.lockf` here uses the SAME
fcntl space, so this wrapper genuinely interlocks with bench-utils benchmarks.

Usage: benchlock.py CMD [ARGS...]
"""
import fcntl
import os
import subprocess
import sys
import time

LOCK = os.path.expanduser("~/benchmarking.lock")


def main() -> int:
    if len(sys.argv) < 2:
        sys.stderr.write("usage: benchlock.py CMD [ARGS...]\n")
        return 2
    fd = os.open(LOCK, os.O_RDWR | os.O_CREAT, 0o644)
    t0 = time.time()
    # Blocking exclusive fcntl lock — the same space bench-utils uses.
    fcntl.lockf(fd, fcntl.LOCK_EX)
    waited = time.time() - t0
    if waited > 0.5:
        sys.stderr.write(f"[benchlock] acquired after {waited:.1f}s wait\n")
    try:
        return subprocess.call(sys.argv[1:])
    finally:
        fcntl.lockf(fd, fcntl.LOCK_UN)
        os.close(fd)


if __name__ == "__main__":
    sys.exit(main())
