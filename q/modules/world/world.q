/ Obstacle worlds (.world) — OBSTACLES_PLAN.md
/ Walls are axis-aligned rectangles inside the field, stored as columns (x0;y0;x1;y1). They block
/ movement (cat and mouse) and, unless --see-through, sight. The field wraps (the default world;
/ walls aren't allowed with --walled-world), so a segment that crosses an edge is tested against the walls' copies in the 8
/ neighbouring tiles: tile builds those copies once per layout.
/ Loaded by cat_mouse.q (only active with --world) and the tests.

\d .world

/ Named layouts on the 100x100 field. Walls are 4 units thick, so a catch (path within 2 units of
/ the mouse) can't happen through a wall.
rects:{[r] `float$flip r}                     / list of (x0 y0 x1 y1) rows -> columns
NONE: 4#enlist 0#0f
LAYOUTS: `none`pillars`wall`rooms!(
  NONE;
  / four 12x12 blocks
  rects (20 20 32 32; 68 20 80 32; 20 68 32 80; 68 68 80 80);
  / one wall across the field at y 48-52, with doors at x 20-30 and 70-80
  rects (0 48 20 52; 30 48 70 52; 80 48 100 52);
  / two vertical walls (x 23-27, 73-77) and two horizontal (y 23-27, 73-77). On a torus that makes
  / 4 rooms; every wall segment between crossings has a 12-unit door (the edge doors wrap: 94-106)
  rects (23 6 27 44; 23 56 27 94; 73 6 77 44; 73 56 77 94;
         6 23 44 27; 56 23 94 27; 6 73 44 77; 56 73 94 77))

/ k random blocks with sides 6-20, inside the field. Uses the global RNG: callers reseed around it.
randomLayout:{[k;f] w: 6+k?14f; h: 6+k?14f; x0: (f-w)*k?1f; y0: (f-h)*k?1f;
  (x0; y0; x0+w; y0+h)}

/ The 9 tile offsets of a wrapped field, and the walls copied into all of them
offs:{[f] f*(-1 -1;-1 0;-1 1;0 -1;0 0;0 1;1 -1;1 0;1 1f)}
tile:{[R;f] o: offs f; (raze R[0]+/:o[;0]; raze R[1]+/:o[;1]; raze R[2]+/:o[;0]; raze R[3]+/:o[;1])}

/ Smallest t in [0,1) at which the segment p -> p+d enters a wall of T (tiled walls); 0w if never.
/ Slab method per axis: a zero component means the segment is inside that slab for every t, or
/ never. Strict inequalities: sliding along a wall face or touching a corner isn't a hit.
slab:{[a0;a1;pa;da]
  $[0f=da; [ins: (pa>a0)&pa<a1; (?[ins;-0w;0w]; ?[ins;0w;-0w])];
    [t0: (a0-pa)%da; t1: (a1-pa)%da; (t0&t1; t0|t1)]]}
hitT:{[T;p;d]
  if[0=count T 0; :0w];
  x: slab[T 0;T 2;p 0;d 0]; y: slab[T 1;T 3;p 1;d 1];
  tin: 0f|x[0]|y[0]; tout: 1f&x[1]&y[1];
  h: tin<tout;
  $[any h; min tin where h; 0w]}

/ Move p by d, stopping 0.05 units short of the first wall. Returns (new position, not yet wrapped;
/ blocked flag). Stopping short keeps the cat outside the wall, so its next move is tested afresh.
move:{[T;p;d]
  t: hitT[T;p;d];
  $[t>=1f; (p+d; 0b); (p+d*0f|t-0.05%.vision.norm d; 1b)]}

/ Whiskers (touch, W.4): for each of the 8 directions in angle order (45d degrees from +x towards
/ +y, the order of the vision slices and of angle wiring), is a wall within len units?
whiskers:{[T;p;len] {[T;p;len;a] 1f>hitT[T;p;len*(cos a;sin a)]}[T;p;len] each (til 8)*.vision.PI%4}

/ ---- Scripted baseline policies (W.0) ----
/ Move directions as .dirs indices; DIRU their unit vectors (x right, y down), matching cat_mouse.q
ANGLE_ORDER: 4 7 6 5 3 0 1 2i                 / angle index -> .dirs index (as .relearn)
DIRU: {x%sqrt sum x*x} each flip `float$(-1 0 1 -1 1 -1 0 1;-1 -1 -1 0 0 1 1 1)
/ The move direction closest to displacement v
bestDir:{[v] `int$first idesc DIRU mmu v}
/ One bar's choice. pol: `random`greedy`reactive`persist; vis: the mouse is visible; s: its vision
/ slice (angle index, -1 hidden); v: displacement to the nearest mouse copy (greedy sees through
/ walls); last: the last direction chosen while visible (persist); u2 / u8: random draws in 0..1 / 0..7.
/ reactive and persist head along one of the two directions bounding the visible slice (u2 picks),
/ and when the mouse is hidden reactive moves at random, persist keeps its last heading.
policyDir:{[pol;vis;s;v;ld;u2;u8]
  $[pol=`random; u8;
    pol=`greedy; bestDir v;
    vis & s>=0i; ANGLE_ORDER (s+u2) mod 8i;
    (pol=`persist) & not null ld; ld;
    u8]}

/ Geodesic distance field (W.0 planner). free: g x g boolean grid (cell [i;j] = x index i, y index
/ j; cell size f%g) of cells whose centre is clear of walls. Returns the shortest 8-connected path
/ length (field units) from the goal to every cell on the wrapped field, around walls (0w where
/ unreachable or blocked), by relaxing over the 8 wrapped neighbour shifts until nothing changes.
freeGrid:{[T;f;g;m] h: f%g; c: h*0.5+til g; {[T;m;c;x] {[T;m;x;y] not inWall[T;(x;y);m]}[T;m;x] each c}[T;m;c] each c}
SHIFTS: (-1 -1;-1 0;-1 1;0 -1;0 1;1 -1;1 0;1 1)
distField:{[free;goal;f]
  g: count free; h: f%g;
  gi: `long$floor (goal mod f)%h;
  free: .[free;gi;:;1b];
  cost: h*?[2=sum each abs SHIFTS; sqrt 2f; 1f];
  D: .[(g;g)#0w;gi;:;0f];
  relax:{[free;cost;D] n: min {[D;s;c] c+s[1] rotate/: s[0] rotate D}[D]'[SHIFTS;cost];
    {[fr;x] ?[fr;x;0w]}'[free;D&n]}[free;cost];
  relax/[D]}
/ Value of field D at position p (wrapped)
fieldAt:{[D;p;f] g: count D; i: `long$floor (p mod f)%f%g; D[i 0;i 1]}
/ Planner: of the 8 moves of length len, the one that isn't blocked and lands closest to the mouse
/ along paths around walls; a move whose path catches the mouse (within 2 units) wins outright.
plannerDir:{[T;D;p;v;len;f]
  ms: len*DIRU;
  if[count c: where 2f>{[v;m] .vision.pathDist[0 0f;m;v]}[v] each ms; :`int$first c];
  val: {[T;D;p;f;m] $[1f>hitT[T;p;m]; 0w; fieldAt[D;p+m;f]]}[T;D;p;f] each ms;
  `int$first iasc val}

/ Can p see q (q unwrapped: p plus the nearest-copy displacement)? Sight is the straight segment.
visible:{[T;p;q] 1f<=hitT[T;p;q-p]}

/ Is p inside (or within margin m of) any wall? Positions are wrapped, so the untiled walls suffice
/ except near the edges, where the tiled copies catch walls that wrap round.
inWall:{[T;p;m] any (p[0]>T[0]-m)&(p[0]<T[2]+m)&(p[1]>T[1]-m)&p[1]<T[3]+m}

/ A position clear of walls by margin m: g[] proposes candidates (wrapped); retry until one is clear
clearPos:{[T;m;g] p: g[]; n: 0; while[inWall[T;p;m]; p: g[]; n+:1; if[n>10000; '"world: no clear position"]]; p}

\d .
