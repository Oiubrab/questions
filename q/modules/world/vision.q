/ Vision simulation module
/ Tracks cat and mouse positions on a toroidal field.
/ Computes 8-slice angular vision from cat's perspective.
/ Mirrors vision_simulation_module.f90

\d .vision

PI: acos -1.0   / 3.14159...

/ Vector helpers. Use these instead of writing a*b+c*d inline: q evaluates
/ right to left, so dx*dx+dy*dy means dx*(dx+(dy*dy)) (REVIVAL_PLAN.md F1).
norm:{sqrt sum x*x}       / Euclidean length of a vector
dot:{sum x*y}             / dot product of two equal-length vectors
alignment:{[move;target]
  moveLen: norm move; targetLen: norm target;
  if[(moveLen=0f)|targetLen=0f; :0n];
  (dot[move;target]) % (moveLen*targetLen)}

/ Initialize mouse at random position in field
initMouse:{[fieldSize] fieldSize * 2?1.0}   / returns (x;y)

/ Move mouse by random walk (step_size units in random direction)
/ Returns new (x;y), wrapped toroidally
moveMouse:{[pos;fieldSize;stepSize]
  angle: (rand 1.0) * 2.0 * PI;
  newX: (pos[0] + stepSize * cos angle) mod fieldSize;
  newY: (pos[1] + stepSize * sin angle) mod fieldSize;
  / Clamp in case mod behaves oddly with negatives
  newX: $[newX<0.0; newX+fieldSize; newX];
  newY: $[newY<0.0; newY+fieldSize; newY];
  (newX;newY)}

/ Compute the active vision slice index (0-indexed, 0 to numSlices-1) for
/ the direction from cat to mouse. Returns -1i if positions are identical.
/ Mirrors update_vision_input in vision_simulation_module.f90
visionSlice:{[catPos;mousePos;numSlices]
  dx: mousePos[0] - catPos[0];
  dy: mousePos[1] - catPos[1];
  if[(0.0001>abs dx)&0.0001>abs dy; :-1i];
  angle: atan[dy%dx];                   / atan returns (-PI/2, PI/2)
  / Full atan2 using sign of dx
  angle: $[dx<0.0;
           $[dy>=0.0; angle+PI; angle-PI];
           angle];
  / Normalize to [0, 2*PI)
  if[angle<0.0; angle+:2.0*PI];
  sliceAngle: 2.0*PI % numSlices;
  / floor, not `int$ (which rounds: angle near 2*PI would give slice numSlices)
  / mod: if float error makes angle exactly 2*PI, wrap to slice 0 (same direction)
  `int$ (floor angle % sliceAngle) mod numSlices}   / 0-indexed slice

/ Build an inputter vector of length numSlices (all 0i except active slice = 1i)
/ Corresponds to MEDIUM state in the original (value=1i)
buildInputter:{[catPos;mousePos;numSlices]
  inp: `int$numSlices#0i;
  s: visionSlice[catPos;mousePos;numSlices];
  if[s>=0i; inp[s]:1i];                  / MEDIUM = 1i
  inp}

/ Calculate toroidally-correct distance between two positions
toroidalDist:{[p1;p2;fieldSize]
  dx: p2[0]-p1[0]; dy: p2[1]-p1[1];
  if[abs[dx]>fieldSize%2.0; dx: dx - (signum dx)*fieldSize];
  if[abs[dy]>fieldSize%2.0; dy: dy - (signum dy)*fieldSize];
  norm (dx;dy)}

/ Shortest displacement from p1 to the nearest copy of p2 on a wrapped (toroidal) field.
/ Each component lies in [-fieldSize/2, fieldSize/2]; norm torDelta = toroidalDist.
torDelta:{[p1;p2;fieldSize] d: p2-p1; d - fieldSize*floor 0.5+d%fieldSize}
/ Wrap a position into [0, fieldSize) on both axes (q's mod is floored, so negatives wrap too)
wrapPos:{[p;fieldSize] p mod fieldSize}

/ Line-segment to point distance: check if cat's movement path crossed near mouse.
/ Returns closest distance (< 2.0 → catch).
/ cat moved from oldPos to newPos; mouse is at mousePos.
pathDist:{[oldPos;newPos;mousePos]
  path: newPos-oldPos;
  pLen: norm path;
  if[pLen<0.0001; :norm mousePos-oldPos];
  / Project mouse onto path segment, clamped to the segment
  t: 0.0 | 1.0 & dot[mousePos-oldPos;path] % pLen*pLen;
  norm mousePos-oldPos+t*path}

\d .
