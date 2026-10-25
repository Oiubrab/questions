#!/usr/bin/env bash
# Copy the generated dashboard and connection into a KX Dashboards install.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
dash="${KX_DASHBOARDS:-$HOME/.kx/dashboards}"
python3 "$here/build_dashboard.py"
cp "$here/catbrain.json" "$dash/data/dashboards/5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b01.json"
cp "$here/connections/catbrain.json" "$dash/data/connections/catbrain.json"
echo "installed into $dash"
