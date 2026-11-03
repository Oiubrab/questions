#!/usr/bin/env bash
# Start the Cat Brain dashboards in one go: the viewer process, the KX Dashboards server and (live mode)
# a simulation publishing to it. Prints the address to open; Ctrl-C stops everything it started.
#
# Usage:
#   dashboards/start.sh [live] [--no-sim] [-- CAT_MOUSE_FLAGS...]   live viewer plus a simulation
#   dashboards/start.sh compare RUN_A RUN_B                         two finished runs side by side
#
# live: the simulation runs with the flags after "--" (default: --seed 42 --bars 20000), writes to
#   results/_dashboard_live and is slowed by SIM_DELAY_MS so the panels are watchable. --no-sim starts
#   only the servers, so you can run your own simulation with --viz-port $VIZ_PORT.
#   Example: dashboards/start.sh live -- --seed 7 --bars 20000 --world rooms
# compare: RUN_A and RUN_B are run folders holding simulation_log.csv and weights_seed*.bin
#   (e.g. results/ARM/s1); they are replayed at VIZ_RATE.
#
# Environment (defaults in brackets):
#   KX_DASHBOARDS [~/.kx/dashboards]  the Dashboards install
#   DASH_PORT [10001]  address of the Dashboards server    VIZ_PORT [6814]  the viewer process
#   VIZ_RATE [40]      replay speed for compare            SIM_DELAY_MS [5] pause after each simulation step
#   LOG_DIR [${TMPDIR:-/tmp}]  where the server logs go    OPEN [0]  1 = also open the page in Firefox
set -euo pipefail
cd "$(dirname "$0")/.."
dash="${KX_DASHBOARDS:-$HOME/.kx/dashboards}"
DASH_PORT="${DASH_PORT:-10001}"; VIZ_PORT="${VIZ_PORT:-6814}"; VIZ_RATE="${VIZ_RATE:-40}"
SIM_DELAY_MS="${SIM_DELAY_MS:-5}"; LOG_DIR="${LOG_DIR:-${TMPDIR:-/tmp}}"; OPEN="${OPEN:-0}"
LIVE_ID=5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b01      # dashboards/install.sh installs under these ids
COMPARE_ID=5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b02

mode=live
case "${1:-}" in live|compare) mode=$1; shift;; esac
sim=1; simflags=()
if [ "$mode" = compare ]; then
  [ $# -eq 2 ] || { echo "usage: $0 compare RUN_A RUN_B" >&2; exit 2; }
  run_a=$1; run_b=$2
  for r in "$run_a" "$run_b"; do [ -d "$r" ] || { echo "no such run folder: $r" >&2; exit 2; }; done
else
  [ "${1:-}" = "--no-sim" ] && { sim=0; shift; }
  [ "${1:-}" = "--" ] && shift
  simflags=("$@")
  [ ${#simflags[@]} -gt 0 ] || simflags=(--seed 42 --bars 20000)
fi
[ -d "$dash" ] || { echo "no KX Dashboards install at $dash (set KX_DASHBOARDS)" >&2; exit 2; }

for p in "$DASH_PORT" "$VIZ_PORT"; do
  if (exec 3<>"/dev/tcp/127.0.0.1/$p") 2>/dev/null; then
    echo "port $p is already in use (a dashboard may already be running; or set DASH_PORT / VIZ_PORT)" >&2; exit 2
  fi
done

dashboards/install.sh > /dev/null
pids=()
trap 'kill -9 "${pids[@]}" 2>/dev/null || true' EXIT INT TERM   # -9: a running q script ignores TERM

if [ "$mode" = compare ]; then
  env q q/programs/viz/viz_server.q -port "$VIZ_PORT" -replay "$run_a" -replay2 "$run_b" -rate "$VIZ_RATE" \
    < /dev/null > "$LOG_DIR/catbrain_viz.log" 2>&1 &
else
  env q q/programs/viz/viz_server.q -port "$VIZ_PORT" < /dev/null > "$LOG_DIR/catbrain_viz.log" 2>&1 &
fi
pids+=($!)
(cd "$dash" && exec env q dash.q -p "$DASH_PORT" -u 1 < /dev/null > "$LOG_DIR/catbrain_dash.log" 2>&1) &
dash_pid=$!; pids+=($dash_pid)

# wait until both servers answer, so the address is ready when it is printed
for p in "$VIZ_PORT" "$DASH_PORT"; do
  for _ in $(seq 1 50); do
    (exec 3<>"/dev/tcp/127.0.0.1/$p") 2>/dev/null && break
    sleep 0.2
  done
  (exec 3<>"/dev/tcp/127.0.0.1/$p") 2>/dev/null || { echo "server on port $p did not start; see $LOG_DIR/catbrain_*.log" >&2; exit 1; }
done

id=$LIVE_ID; [ "$mode" = compare ] && id=$COMPARE_ID
url="http://localhost:$DASH_PORT/#/$id"
if [ "$mode" = live ] && [ "$sim" = 1 ]; then
  env q q/programs/cat_mouse.q "${simflags[@]}" --viz-port "$VIZ_PORT" --viz-delay-ms "$SIM_DELAY_MS" \
    --output-dir results/_dashboard_live < /dev/null > "$LOG_DIR/catbrain_sim.log" 2>&1 &
  pids+=($!)
  echo "simulation: q q/programs/cat_mouse.q ${simflags[*]}   (log: $LOG_DIR/catbrain_sim.log)"
elif [ "$mode" = live ]; then
  echo "no simulation started: run one with  q q/programs/cat_mouse.q ... --viz-port $VIZ_PORT"
fi
echo "Cat Brain ($mode): $url"
echo "Ctrl-C stops the servers started here."
[ "$OPEN" = 1 ] && { firefox "$url" > /dev/null 2>&1 & }
wait "$dash_pid"
