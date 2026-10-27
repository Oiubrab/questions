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

/ Sparse usage is tagged so empty index lists remain distinguishable from dense masks.
emptySparseUsage:{[](1b;`int$())}
isSparseUsage:{[usage]
  if[2<>count usage; :0b];
  if[-1h<>type first usage; :0b];
  first usage}
sparseUsage:{[indices](1b;`int$distinct indices)}
addSparseUsage:{[usage;idx] (1b;asc distinct (usage 1),enlist `int$idx)}
usageIndices:{[usage] `int$distinct (usage 1)}
indexOf:{[r;c;inDir;outDir;nC]
  cellIndex:((r*nC)+c);
  dirIndex:((inDir*8i)+outDir);
  `int$((cellIndex*64i)+dirIndex)}

/ Update only indices in tagged sparse usage; dense APIs remain available to the lab.
applySparseReinforce:{[syns;usage;numSteps]
  flat: raze raze raze syns;
  idx: usageIndices usage;
  old: flat[idx];
  mult: `float$1.2%0.95 xexp numSteps;
  noise: 0.9 + 0.2*(count idx)?1.0;
  flat:@[flat;idx;:;.syn.MAX_STRENGTH & `int$`float$old*mult*noise];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}
applySparseReinforceN:{[syns;usage;numSteps;calls]
  if[calls<=0i; :syns];
  flat: raze raze raze syns;
  idx: usageIndices usage;
  old: flat[idx];
  mult: (`float$1.2%0.95 xexp numSteps) xexp calls;
  noise: 0.9 + 0.2*(calls*count idx)?1.0;
  noiseShape:`int$(calls;count idx);
  noise:prd each flip noiseShape#noise;
  updated: .syn.MAX_STRENGTH & `int$`float$old*mult*noise;
  flat:@[flat;idx;:;updated];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}
applySparsePunish:{[syns;usage;numSteps]
  flat: raze raze raze syns;
  idx: usageIndices usage;
  old: flat[idx];
  mult: `float$0.8*0.95 xexp numSteps;
  noise: 0.9 + 0.2*(count idx)?1.0;
  flat:@[flat;idx;:;.syn.MIN_STRENGTH | `int$`float$old*mult*noise];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}
applySparsePunishN:{[syns;usage;numSteps;calls]
  if[calls<=0i; :syns];
  flat: raze raze raze syns;
  idx: usageIndices usage;
  old: flat[idx];
  mult: (`float$0.8*0.95 xexp numSteps) xexp calls;
  noise: 0.9 + 0.2*(calls*count idx)?1.0;
  noiseShape:`int$(calls;count idx);
  noise:prd each flip noiseShape#noise;
  updated: .syn.MIN_STRENGTH | `int$`float$old*mult*noise;
  flat:@[flat;idx;:;updated];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}
/ G.3(b) bounded log-space rewards: the same update as applySparseReinforceN/PunishN, with the
/ same RNG draws, but each synapse's total log-change is multiplied by lam. So lam=0.25 makes a
/ reward a quarter as large in log space. lam=1 calls the original update (bit-identical).
applySparseReinforceNL:{[syns;usage;numSteps;calls;lam]
  if[1f=lam; :applySparseReinforceN[syns;usage;numSteps;calls]];
  if[calls<=0i; :syns];
  flat: raze raze raze syns;
  idx: usageIndices usage;
  old: flat[idx];
  mult: (`float$1.2%0.95 xexp numSteps) xexp calls;
  noise: 0.9 + 0.2*(calls*count idx)?1.0;
  noise:prd each flip (`int$(calls;count idx))#noise;
  updated: .syn.MAX_STRENGTH & `int$`float$old*(mult*noise) xexp lam;
  flat:@[flat;idx;:;updated];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}
applySparsePunishNL:{[syns;usage;numSteps;calls;lam]
  if[1f=lam; :applySparsePunishN[syns;usage;numSteps;calls]];
  if[calls<=0i; :syns];
  flat: raze raze raze syns;
  idx: usageIndices usage;
  old: flat[idx];
  mult: (`float$0.8*0.95 xexp numSteps) xexp calls;
  noise: 0.9 + 0.2*(calls*count idx)?1.0;
  noise:prd each flip (`int$(calls;count idx))#noise;
  updated: .syn.MIN_STRENGTH | `int$`float$old*(mult*noise) xexp lam;
  flat:@[flat;idx;:;updated];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}
/ Deterministic multiplier m scaled in log space by lam (lam=1 returns m unchanged)
scaleMult:{[m;lam] $[1f=lam; m; m xexp lam]}

/ G.R2 policy-gradient update in log space (REINFORCE for the softmax route choice at T=1).
/ Route choice is p_s(o) = bias_o*W_s,o / sum bias*W_s for a directed state s = (cell, inDir),
/ i.e. a softmax over theta = ln W, so d ln p_s(c) / d theta_s,o = 1[o=c] - p_s(o).
/ For every used state s (flat index s*8+c records route c taken from s):
/   dtheta_s,o = eta * delta * (n_s,o - n_s * p_s(o))
/ where n_s,o = 1 if route o of s was used this bar and n_s = routes used from s. All 8 routes of
/ a used state move: chosen ones by (1-p), siblings by -p. So the row's total log-change is zero
/ (before clamping) and updates shrink as a route becomes certain. Unused states are untouched.
/ No RNG is used.
applySparsePG:{[syns;usage;bias;eta;delta]
  idx: usageIndices usage;
  if[(0=count idx)|0f=delta; :syns];
  flat: raze raze raze syns;
  st: distinct idx div 8i;                      / used directed states
  rowsIdx: (8i*st) +\: til 8i;                  / states x 8 flat indices
  W: `float$flat rowsIdx;
  bw: W *\: bias;
  p: bw % sum each bw;
  used: `float$rowsIdx in idx;                  / n_s,o
  dth: eta*delta*used - p*sum each used;
  new: .syn.MIN_STRENGTH | .syn.MAX_STRENGTH & `int$W*exp dth;
  flat: @[flat; raze rowsIdx; :; raze new];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}

applySparseScale:{[syns;usage;mult]
  flat: raze raze raze syns;
  idx: usageIndices usage;
  flat:@[flat;idx;:;.syn.MAX_STRENGTH & `int$`float$flat[idx]*mult];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}
applySparseLookbackScale:{[syns;usages;mult]
  allIdx:raze usageIndices each usages;
  grouped:group allIdx;
  idx:key grouped;
  nUsed:count each value grouped;
  flat:raze raze raze syns;
  scaled:.syn.MAX_STRENGTH & `int$`float$flat[idx]*mult xexp nUsed;
  flat:@[flat;idx;:;scaled];
  shape:`int$(count syns;count first syns;8i;8i);
  shape#flat}

/ Initialize 5D synapse history buffer (histSize x nR x nC x 8 x 8 boolean)
initHist:{[histSize;nR;nC]
  histSize # enlist nR # enlist nC # enlist 8 8#0b}

initSparseHist:{[histSize] histSize#enlist emptySparseUsage[]}
denseHistToSparse:{[hist]
  sparseUsage each {where raze raze raze x} each hist}
sparseHistToDense:{[hist;nR;nC]
  {[usage;nR;nC]
    flat:(nR*nC*64)#0b;
    idx:usageIndices usage;
    flat:@[flat;idx;:;(count idx)#1b];
    shape:`int$(nR;nC;8i;8i);
    shape#flat
  }'[hist;count[hist]#nR;count[hist]#nC]}

/ History buffer slot holding bar b, seen from current bar barN with write head hi
/ (hi is the NEXT slot to be written, so bar barN-1 is at hi-1). Relies on q's
/ floored mod: -3 mod 100 is 97.
histSlot:{[hi;barN;b;histSize] `int$(hi-(barN-b)) mod histSize}

/ Bars to look back over for strategic reinforcement: bar-1, bar-2, ..., bar-scope,
/ keeping only bars >= 1 that are still in the history buffer (bars-ago < histSize)
lookbackBars:{[bar;scope;histSize]
  b: `int$(bar-1i) - til scope;
  b where (b>=1i) & (bar-b)<histSize}

/ Reset synapse usage to all false
resetSU:{[su] {{8 8#0b} each til count x} each su}

/ Decay one 8x8 synapse matrix (multiplier 0.98-0.995 per element)
/ decay_value(): 0.98 + 0.015*rand gives 0.5-2% loss
decayCell:{[sc]
  noise: 8 8 # 0.98 + 0.015*(64?1.0);
  MIN_STRENGTH | `int$(`float$sc) * noise}

/ Apply decay to entire synapse array
applyDecay:{[syns]
  flat: raze raze raze syns;
  noise: 0.98 + 0.015*(count flat)?1.0;
  updated: MIN_STRENGTH | `int$`float$flat*noise;
  shape: `int$(count syns;count first syns;8i;8i);
  shape#updated}

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
  flat: raze raze raze syns;
  used: raze raze raze su;
  noise: 0.9 + 0.2*(count flat)?1.0;
  updated: MAX_STRENGTH & `int$`float$flat*mult*noise;
  shape: `int$(count syns;count first syns;8i;8i);
  shape#(flat + used*(updated-flat))}

/ Apply adaptive punishment (weakens used synapses)
/ Multiplier = 0.8 * (0.95^numSteps), scaled by random noise 0.9-1.1
/ Mirrors apply_adaptive_punishment in synapses_module.f90
applyPunish:{[syns;su;numSteps]
  mult: `float$0.8 * 0.95 xexp numSteps;
  flat: raze raze raze syns;
  used: raze raze raze su;
  noise: 0.9 + 0.2*(count flat)?1.0;
  updated: MIN_STRENGTH | `int$`float$flat*mult*noise;
  shape: `int$(count syns;count first syns;8i;8i);
  shape#(flat + used*(updated-flat))}

\d .
