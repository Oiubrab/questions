#!/usr/bin/env python3
"""Generate the KX Dashboards definition for the cat/brain viewer.

Templates the ChartGL widget and dashboard shell from a bundled demo dashboard, so it needs the
Dashboards install (default ~/.kx/dashboards). Output: dashboards/catbrain.json and
dashboards/connections/catbrain.json. Run dashboards/install.sh to copy them into the install.
"""
import copy, json, os, sys, uuid

DASH = os.path.expanduser(os.environ.get("KX_DASHBOARDS", "~/.kx/dashboards"))
HERE = os.path.dirname(os.path.abspath(__file__))
TEMPLATE = os.path.join(DASH, "data/dashboards/e9c38322-9660-b54b-d072-cd3c9af241c1.json")
VIZ_PORT = os.environ.get("VIZ_PORT", "6814")
DASH_ID = "5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b01"
CONN = "catbrain"
POLL_SECONDS = float(os.environ.get("POLL_SECONDS", "0.25"))

LEVEL_COLOURS = ["#2b2f3a", "#f2a900", "#ff3b30"]  # energy 0 / 1 / 2
FIELD_COLOURS = ["#7a3a3a", "#7a6a1a", "#ff3b30", "#ffd500", "#3aa0ff"]  # mouse trail, cat trail, mouse, cat, gaze
TRAIL_BARS = 60


def matrix_q(fn, nrows):
    # Off-grid dummy rows keep every category present each frame so the category-to-colour mapping is stable.
    return ("([] x:-5 -5 -5f; y:-5 -5 -5f; lvl:`l0`l1`l2), "
            "select x:col+0.5, y:%s-row, lvl:`l0`l1`l2 energy from %s[]" % (nrows - 0.5, fn))


FIELD_Q = ("([] who:`mouseTrail`catTrail`mouse`cat`gaze; x:5#-5f; y:5#-5f), "
           ".viz.trailNow[%d], .viz.fieldNow[]" % TRAIL_BARS)
RATE_Q = "select bar, rate from .viz.statsNow[2000]"
KPI_Q = ".viz.kpiNow[]"
GRID_TEMPLATE = os.path.join(DASH, "data/dashboards/020c9b8f-f42f-d552-21e2-dfdfd8dbfef9.json")
ALIGN_Q = "select bar, alignAvg from .viz.statsNow[2000]"


def uid():
    return str(uuid.uuid4())


def datasource(query, key=""):
    return {
        "_pagingType": "NONE", "_autoExecute": True, "_autoExec": True, "_columns": [],
        "_dataType": "query", "_dataSource": "kdb", "_connection": CONN, "_mappings": {},
        "_maxRows": 2000, "_subscriptionType": "polling", "_subscriptionInterval": POLL_SECONDS,
        "_layout": [{"isExpanded": True, "weight": 2}, {"isExpanded": False, "weight": 1},
                    {"isExpanded": False, "weight": 1}, {"isExpanded": True, "weight": 0},
                    {"isExpanded": True, "weight": 2}],
        "_queryString": query, "_queryParams": [], "_subscriptionKey": key,
        "_hasUpdateQuery": False, "_updateQueryString": "", "_updateType": "query",
        "_pageSize": 2000, "_serverPaging": False,
    }


def chartgl(template, layer_type, data, cols, **layer_props):
    c = copy.deepcopy(template)
    c["id"] = uid()
    c["containerId"] = None
    layer = c["options"]["Layers"][0]
    layer.update({"_Type": layer_type, "Data": {"_dashboardsType": "data", "value": data},
                  "possibleXColumns": cols, "possibleYColumns": ["*"] + cols,
                  "possibleColorColumns": ["None"] + cols})
    layer.update(layer_props)
    c["options"]["Layers"] = [layer]
    return c


def set_range(c, xr, yr):
    for axes, (lo, hi) in ((c["options"]["Xaxes"], xr), (c["options"]["Yaxes"], yr)):
        axes[0]["Type"] = "Linear"
        axes[0]["Range"].update({"UseMinMax": True, "Min": str(lo), "Max": str(hi)})
    return c


def widget(component, row, col, rows, cols):
    return {"id": uid(), "layout": {"row": row, "column": col, "rowSpan": rows, "colSpan": cols},
            "component": component}


def matrix(gl, data, width, height, radius):
    c = chartgl(gl, "Bubble", data, ["x", "y", "lvl"], XAxis="x", YAxis="y", ColorData="lvl")
    layer = c["options"]["Layers"][0]
    layer["Bubbles"]["RadiusScaling"] = radius
    layer["Opacity"] = 100
    layer["Palette"] = [{"type": col} for col in LEVEL_COLOURS]
    return set_range(c, (0, width), (0, height))


def curve(gl, data, ycol, ylo, yhi):
    c = chartgl(gl, "Line", data, ["bar", ycol], XAxis="bar", YAxis=ycol)
    c["options"]["Xaxes"][0]["Type"] = "Linear"
    c["options"]["Xaxes"][0]["Range"]["UseMinMax"] = False
    c["options"]["Yaxes"][0]["Type"] = "Linear"
    c["options"]["Yaxes"][0]["Range"].update(
        {"UseMinMax": ylo is not None, "Min": str(ylo), "Max": str(yhi)})
    return c


def datagrid(data):
    """Smallest bundled Datagrid component, repointed at our data source."""
    def find(o):
        if isinstance(o, dict):
            if o.get("key") == "Datagrid":
                return o
            o = list(o.values())
        for v in (o if isinstance(o, list) else []):
            r = find(v)
            if r:
                return r
    c = copy.deepcopy(find(json.load(open(GRID_TEMPLATE))))
    c["id"] = uid()
    c["options"]["Basics"]["Data"]["value"] = data
    return c


def main():
    src = json.load(open(TEMPLATE))
    gl = src["screens"][0]["widgets"][0]["component"]["components"][0]

    energy = matrix(gl, "energy", 12, 6, 30)
    meta = matrix(gl, "meta", 5, 3, 36)
    field = chartgl(gl, "Bubble", "field", ["who", "x", "y"], XAxis="x", YAxis="y", ColorData="who")
    fl = field["options"]["Layers"][0]
    fl["Bubbles"]["RadiusScaling"] = 7
    fl["Palette"] = [{"type": col} for col in FIELD_COLOURS]
    set_range(field, (0, 100), (0, 100))
    rate = curve(gl, "rate", "rate", None, None)
    align = curve(gl, "align", "alignAvg", -1, 1)

    screen = copy.deepcopy(src["screens"][0])
    screen.update({"id": uid(), "name": "Screen 1", "isDefault": True, "widgets": [
        widget(energy, 0, 0, 14, 22), widget(field, 0, 22, 14, 14),
        widget(datagrid("kpi"), 14, 0, 10, 9), widget(meta, 14, 9, 10, 9),
        widget(rate, 14, 18, 10, 9), widget(align, 14, 27, 10, 9)]})

    out = {k: v for k, v in src.items() if k not in ("screens", "data", "viewState", "screenDetails")}
    out.update({
        "id": DASH_ID, "name": "Cat Brain", "hash": uid(), "screens": [screen],
        "screenDetails": [{"label": "Screen 1", "value": screen["id"]}],
        "viewState": {".settings": {}}, "popups": [], "tags": [],
        "data": {"energy": datasource(matrix_q(".viz.energyNow", 6)),
                 "meta": datasource(matrix_q(".viz.metaNow", 3)),
                 "field": datasource(FIELD_Q), "rate": datasource(RATE_Q), "align": datasource(ALIGN_Q),
                 "kpi": datasource(KPI_Q)},
    })
    json.dump(out, open(os.path.join(HERE, "catbrain.json"), "w"), indent=1)

    os.makedirs(os.path.join(HERE, "connections"), exist_ok=True)
    conn = {"name": CONN, "dbtype": "q", "tls": "off", "host": "localhost", "port": VIZ_PORT,
            "user": "", "pass": "", "database": ""}
    json.dump(conn, open(os.path.join(HERE, "connections", CONN + ".json"), "w"), indent=2)
    print("wrote dashboards/catbrain.json")


if __name__ == "__main__":
    sys.exit(main())
