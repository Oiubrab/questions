/ Trinary constants and shift
/ Run from repo root: q q/tests/test_trinary.q

\l q/load.q
\l q/tests/harness.q

-1 "test_trinary";

.t.eq["constants";(.trinary.LOW;.trinary.MEDIUM;.trinary.HIGH);0 1 2i];
.t.eq["shift up";.trinary.shift[0i;1];1i];
.t.eq["shift up clamps at HIGH";.trinary.shift[2i;1];2i];
.t.eq["shift down";.trinary.shift[2i;-1];1i];
.t.eq["shift down clamps at LOW";.trinary.shift[0i;-1];0i];
.t.eq["shift vector";.trinary.shift[0 1 2i;1];1 2 2i];
.t.eq["initGrid shape";count each .trinary.initGrid[6;12];6#12];
.t.eq["initGrid all LOW";distinct raze .trinary.initGrid[6;12];enlist 0i];

.t.done[];
