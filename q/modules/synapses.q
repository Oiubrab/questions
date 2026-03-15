/ Synapse module
/ 4D synapse array: rows x cols x 8 x 8
/ syns[r][c] is an 8x8 integer matrix where syns[r][c][inDir-1][outDir-1] = weight
/ Mirrors synapses_module.f90

\d .syn

MAX_STRENGTH: 2000000i
MIN_STRENGTH: 25i
MAX_INIT: 1000i

/ Initialize 4D synapse array (rows x cols x 8 x 8 integer)
/ Returns nested list: rows lists, each containing cols 8x8 int matrices
init:{[nR;nC]
  / Projection f[;nC] fixes nC so inner lambda can generate fresh random matrices per row
  f:{[x;n] {`int$(MIN_STRENGTH + (MAX_INIT - MIN_STRENGTH) * 8 8#64?1.0)} each til n};
  f[;nC] each til nR}

/ Initialize synapse usage tracker (nR x nC x 8 x 8 boolean)
initSU:{[nR;nC]
  nR # enlist nC # enlist 8 8#0b}

/ Initialize 5D synapse history buffer (histSize x nR x nC x 8 x 8 boolean)
initHist:{[histSize;nR;nC]
  histSize # enlist nR # enlist nC # enlist 8 8#0b}

/ Reset synapse usage to all false
resetSU:{[su] {{8 8#0b} each til count x} each su}

/ Decay one 8x8 synapse matrix (multiplier 0.98-0.995 per element)
/ decay_value(): 0.98 + 0.015*rand gives 0.5-2% loss
decayCell:{[sc]
  noise: 8 8 # 0.98 + 0.015*(64?1.0);
  MIN_STRENGTH | `int$(`float$sc) * noise}

/ Apply decay to entire synapse array
applyDecay:{[syns]
  {decayCell each x} each syns}

/ Module-level helpers for reinforce/punish — defined at namespace scope to avoid
/ kdb-x v5 closure limitation (inner lambdas cannot capture outer local variables).

/ Reinforce one 8x8 matrix sc where usage mask uc is set (caps at MAX_STRENGTH)
reinforceCell:{[mult;sc;uc]
  noise: 8 8 # 0.9 + 0.2*(64?1.0);
  newSc: MAX_STRENGTH & `int$(`float$sc) * mult * noise;
  sc + uc * (newSc - sc)}

/ Punish one 8x8 matrix sc where usage mask uc is set (floors at MIN_STRENGTH)
punishCell:{[mult;sc;uc]
  noise: 8 8 # 0.9 + 0.2*(64?1.0);
  newSc: MIN_STRENGTH | `int$(`float$sc) * mult * noise;
  sc + uc * (newSc - sc)}

/ Apply adaptive reinforcement to synapses marked as used
/ Multiplier = 1.2 / (0.95^numSteps), scaled by random noise 0.9-1.1
/ Mirrors apply_adaptive_reinforcement in synapses_module.f90
applyReinforce:{[syns;su;numSteps]
  mult: `float$1.2 % 0.95 xexp numSteps;
  / Pass mult via projection to avoid kdb-x v5 closure limitation
  {[mult;x;y] reinforceCell[mult]'[x;y]}[mult]'[syns;su]}

/ Apply adaptive punishment (weakens used synapses)
/ Multiplier = 0.8 * (0.95^numSteps), scaled by random noise 0.9-1.1
/ Mirrors apply_adaptive_punishment in synapses_module.f90
applyPunish:{[syns;su;numSteps]
  mult: `float$0.8 * 0.95 xexp numSteps;
  {[mult;x;y] punishCell[mult]'[x;y]}[mult]'[syns;su]}

\d .
