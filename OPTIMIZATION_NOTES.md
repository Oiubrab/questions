# Performance Notes

## Q/kdb-x Implementation

The q port replaces the Fortran/OpenMP implementation. The key architectural shift is from **parallel imperative loops** to **functional fold**.

### Propagation as Functional Fold

The core performance design in q:

```q
/ f/[accumulator; activeNeuronList] threads (br;syns;su;inc0;inc1;out) through each neuron
/ Sequential but energy-conserving — no critical sections needed
acc: f/[(br;syns;su;inc0;inc1;out); activeNeuronList]
```

**Why fold instead of `each`:**
- `each` discards return values — synapse updates would be lost
- Fold threads mutable state through the list sequentially
- Energy conservation is maintained without OpenMP critical sections
- Correct credit assignment: each neuron sees the state left by the previous one

### Avoiding Repeated Allocation

In the Fortran version, module-level buffer caching eliminated repeated heap allocations. In q, nested lists are structural and reused by reference — no special caching needed. The main cost is the fold iteration itself.

### Array Access Patterns

4D synapse access in q:
```q
/ syns[r][c][inDir][outDir] — nested list access
sc: syns[r][c];          / 8x8 int matrix
w:  sc[inDir][outDir];   / scalar weight
```

The nested list layout (rows × cols × 8 × 8) mirrors the Fortran array layout and gives good locality for per-neuron processing.

### Precomputed Constants

Direction constants, biases, and opposites are computed once at load time in `.dirs`:
```q
dr: -1 -1 -1 0i 0i 1i 1i 1i    / row deltas (8-element int vector)
dc: -1i 0i 1i -1i 1i -1i 0i 1i / col deltas
vBias: 0.5 0.5 0.5 1.0 1.0 1.5 1.8 1.5
```

Vector operations on these (e.g. `vBias * mult`) are SIMD-friendly in q's array engine.

### Synapse Decay (Vectorized)

Decay applies a random multiplier (0.98–0.995) element-wise across each 8×8 matrix:
```q
decayCell:{[sc]
  noise: 8 8 # 0.98 + 0.015*(64?1.0);
  MIN_STRENGTH | `int$(`float$sc) * noise}
applyDecay:{[syns] {decayCell each x} each syns}
```

The 8×8 matrix multiply is vectorized by q's array engine. The `MIN_STRENGTH |` clamp prevents underflow without branching.

## Fortran Baseline (Historical Reference)

The prior Fortran implementation achieved:
- **Single-threaded**: 50.7 seconds (wall clock, 20,000 bars)
- **Multi-threaded (OpenMP, ~15 cores)**: 27.8 seconds — 1.82× speedup
- Learning quality maintained: 1,107 catches, 85%+ directional accuracy

The OpenMP approach used:
- `collapse(4)` on decay loops
- Critical sections for synapse write conflicts
- SIMD vectorization via `-O3 -ftree-vectorize`

The q fold approach trades raw throughput for correctness guarantees (no race conditions possible). For the current brain size (6×12 = 72 neurons, 4,608 synapses), q performance is adequate.

## Scaling Considerations

If brain size is increased significantly:
1. **Fold bottleneck**: Active neuron count scales with `rows × cols`; consider batching inactive neurons
2. **Synapse decay**: `{decayCell each x} each syns` maps well to q's vector engine; scales quadratically with `rows × cols`
3. **History buffer**: `(100; rows; cols; 8; 8)` boolean — memory scales linearly with brain size
4. **Meta-brain**: Keep at 3×5 — larger sizes cause energy accumulation bugs (prior experiments)

## kdb-x Simulation Log Advantage

The `simLog` kdb-x table approach significantly outperforms the previous CSV file approach:
- Appends are O(1) (in-memory columnar append)
- Real-time queries with no parsing overhead
- Full q query language available during simulation
- Weight saving uses native q binary serialization (`weightPath set ...`)
