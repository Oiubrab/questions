/ Vision simulation module
/ Tracks cat and mouse positions on a toroidal field.
/ Computes 8-slice angular vision from cat's perspective.
/ Mirrors vision_simulation_module.f90

\d .vision

PI: acos -1.0   / 3.14159...

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
  if[(abs dx < 0.0001)&(abs dy < 0.0001); :-1i];
  angle: atan[dy%dx];                   / atan returns (-PI/2, PI/2)
  / Full atan2 using sign of dx
  angle: $[dx<0.0;
           $[dy>=0.0; angle+PI; angle-PI];
           angle];
  / Normalize to [0, 2*PI)
  if[angle<0.0; angle+:2.0*PI];
  sliceAngle: 2.0*PI % numSlices;
  `int$ angle % sliceAngle}               / 0-indexed slice

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
  sqrt[dx*dx + dy*dy]}

/ Line-segment to point distance: check if cat's movement path crossed near mouse.
/ Returns closest distance (< 2.0 → catch).
/ cat moved from oldPos to newPos; mouse is at mousePos.
pathDist:{[oldPos;newPos;mousePos]
  pdx: newPos[0]-oldPos[0];
  pdy: newPos[1]-oldPos[1];
  pLen: sqrt[pdx*pdx + pdy*pdy];
  if[pLen<0.0001; :sqrt[(mousePos[0]-oldPos[0]) xexp 2 + (mousePos[1]-oldPos[1]) xexp 2]];
  / Project mouse onto path segment
  t: ((mousePos[0]-oldPos[0])*pdx + (mousePos[1]-oldPos[1])*pdy) % (pLen*pLen);
  t: 0.0 | t & 1.0;
  cx: oldPos[0] + t*pdx;
  cy: oldPos[1] + t*pdy;
  sqrt[(mousePos[0]-cx) xexp 2 + (mousePos[1]-cy) xexp 2]}

\d .
