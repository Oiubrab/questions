/ Direction constants module
/ 8 directions (1-indexed to match Fortran):
/   1=NW(-1,-1)  2=N(-1,0)  3=NE(-1,+1)
/   4=W(0,-1)    5=E(0,+1)
/   6=SW(+1,-1)  7=S(+1,0)  8=SE(+1,+1)
/ Mirrors direction constants in brain_module.f90

\d .dirs

/ Row and column deltas for directions 1-8 (access as dr[dir-1], dc[dir-1])
dr: `int$-1 -1 -1 0 0 1 1 1
dc: `int$-1 0 1 -1 1 -1 0 1

/ Opposite directions (1-indexed values, access as opp[dir-1])
/ direction_opposites(8) = [8,7,6,5,4,3,2,1]
opp: `int$8 7 6 5 4 3 2 1

/ Vertical bias: favours downward propagation (for vision signals entering top row)
/ direction_bias(8) = [0.5,0.5,0.5,1.0,1.0,1.5,1.8,1.5]
vBias: 0.5 0.5 0.5 1.0 1.0 1.5 1.8 1.5

/ Horizontal bias: favours leftward propagation (for meta-brain signals entering right column)
/ horizontal_bias(8) = [1.5,1.0,0.5,0.5,0.5,1.0,1.5,1.8]
hBias: 1.5 1.0 0.5 0.5 0.5 1.0 1.5 1.8

\d .
