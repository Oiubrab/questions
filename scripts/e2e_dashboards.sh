#!/usr/bin/env bash
# Start the viz server, the sim and the Dashboards gateway, run the Firefox e2e test, then stop them all.
# Needs a Dashboards install (KX_DASHBOARDS, default ~/.kx/dashboards) and global Playwright with Firefox.
# Usage: scripts/e2e_dashboards.sh [live|compare] [RUN_A RUN_B]
#   live (default): the live viewer against a running sim.
#   compare: the Cat Brain Compare dashboard replaying two finished runs (dirs with simulation_log.csv
#   and weights_seed*.bin; defaults: seed 1 of the 2026-10-29 angle- and grid-wiring arms).
set -euo pipefail
cd "$(dirname "$0")/.."
dash="${KX_DASHBOARDS:-$HOME/.kx/dashboards}"
viz_port="${VIZ_PORT:-6814}"
mode="${1:-live}"
run_a="${2:-results/ss_wireangle_20261029/s1}"; run_b="${3:-results/ss_wiregrid_20261029/s1}"

for p in 10001 "$viz_port"; do
  if (exec 3<>"/dev/tcp/127.0.0.1/$p") 2>/dev/null; then echo "port $p already in use" >&2; exit 2; fi
done

dashboards/install.sh >/dev/null
pids=()
trap 'kill -9 "${pids[@]}" 2>/dev/null || true' EXIT

if [ "$mode" = compare ]; then
  env q q/programs/viz_server.q -port "$viz_port" -replay "$run_a" -replay2 "$run_b" -rate 40 < /dev/null > /tmp/e2e_viz.log 2>&1 &
else
  env q q/programs/viz_server.q -port "$viz_port" < /dev/null > /tmp/e2e_viz.log 2>&1 &
fi
pids+=($!)
(cd "$dash" && exec env q dash.q -p 10001 -u 1 < /dev/null > /tmp/e2e_dash.log 2>&1) &
pids+=($!)
sleep 3
if [ "$mode" = compare ]; then
  NODE_PATH="${NODE_PATH:-/usr/lib/node_modules}" node dashboards/e2e/compare.test.cjs
  exit
fi
# Real sim, slowed so the panels visibly change; stopped by the trap when the test ends
env q q/programs/cat_mouse.q --seed 42 --bars 3000 --viz-port "$viz_port" --viz-delay-ms 5 \
  --output-dir results/_e2e_dashboards < /dev/null > /tmp/e2e_sim.log 2>&1 &
pids+=($!)
sleep 4

NODE_PATH="${NODE_PATH:-/usr/lib/node_modules}" node dashboards/e2e/dashboard.test.cjs
