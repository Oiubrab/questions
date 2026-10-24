#!/usr/bin/env bash
# Run every q/tests/test_*.q from the repo root; exit non-zero if any fails.
set -u
cd "$(dirname "$0")/../.." || exit 2
status=0
for t in q/tests/test_*.q; do
  if ! q "$t" < /dev/null; then
    echo "FAILED: $t"
    status=1
  fi
done
[ $status -eq 0 ] && echo "ALL TESTS PASSED"
exit $status
