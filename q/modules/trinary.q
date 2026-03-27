/ Trinary neuron state module
/ Three states: LOW=0i (inactive), MEDIUM=1i (active), HIGH=2i (saturated)
/ Mirrors trinary_module.f90

\d .trinary

LOW:0i
MEDIUM:1i
HIGH:2i

/ Shift a neuron value by d (+1 or -1), clamped to [LOW,HIGH]
shift:{[v;d] HIGH & LOW | v + `int$d}

/ Initialize a rows x cols grid of trinary neurons, all LOW
initGrid:{[r;c] r # enlist c # LOW}

\d .
