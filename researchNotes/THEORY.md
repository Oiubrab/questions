# Theory — the brain as a system of probabilities

This document sets out mathematical principles for the brain in this project: how units of energy move through a probability matrix, how use reshapes that matrix, and what follows. The aim is to predict how a configuration will behave *before* running it.

Each claim carries a status:

| Tag | Meaning |
|---|---|
| **Derived** | follows from the rules as coded; no experiment needed |
| **Verified** | derived *and* checked numerically; the numbers come from `q q/programs/theory_checks.q` or from the experiments in [REVIVAL_PLAN.md](../plans/inProgress/REVIVAL_PLAN.md) §6 |
| **Consistent** | explains existing results after the fact (weaker than a prediction made beforehand) |
| **Prediction** | not yet tested; each has a test proposed in §11 |
| **Conjecture** | a proposed principle that needs more work to make precise |

Status as of 2026-10-24: first version.

---

## 1. The model, written down

**The state.**
- A grid of cells $i$, each holding $n_i \in \{0,1,2\}$ units of energy (LOW, MEDIUM, HIGH).
- An occupied cell carries an *incoming-direction label* $d_i \in \{1..8\}$, the direction its energy came from. A HIGH cell can carry a second label $d'_i$.
- Each cell has a weight matrix $W_i(d,o) \ge 25$: one weight for each (incoming $d$, outgoing $o$) pair, 64 per cell.

**The move rule** (`.brain.stepNeuron`). In each step, every cell that is occupied at the start of the step moves **one** unit. It chooses direction $o$ with probability

$$
p_i(o \mid d) \;=\; \frac{m_i(o)\,\big[\,b_o\,W_i(d,o)\,\big]^{\alpha}}{\sum_{o'} m_i(o')\,\big[\,b_{o'}\,W_i(d,o')\,\big]^{\alpha}}, \qquad \alpha = 1/T,
$$

where:
- $b_o$ is the direction bias (`vBias`);
- $T$ is the temperature: the cat uses $T=1$, and the lab can vary it;
- $m_i(o)\in\{0,1\}$ masks out targets that are off-grid or already full.

A HIGH cell with two labels uses the average of the two weight rows. The unit arrives at the target carrying the label $\bar o$, the opposite of $o$, so its *next* choice depends on the direction it just moved. **Each unit therefore performs a second-order (direction-remembering) random walk.**

**Learning (Hebbian).** A successful move adds $h$ to the weight used ($h = 1000$, capped at $2{\times}10^6$). Every 60 steps (5 bars), every weight is multiplied by $\delta \sim U[0.98, 0.995]$, with a floor of 25.

**Conservation (Derived).** A unit leaves its source only if it lands somewhere, so $\sum_i n_i$ plus the outputter total is invariant during propagation. Energy enters and leaves only at the boundaries: the input row, the meta cross-flow, the outputter and the drain.

**Throughput limit (Derived).** Each occupied cell moves *one* unit per step, even when HIGH. So a cell's maximum outflow is 1 unit/step, whatever it holds.

---

## 2. One unit at low density: the brain is a Markov chain

When energy is sparse, units rarely meet, the masks $m_i$ only exclude off-grid targets, and each unit walks independently. The walk is then a Markov chain on **directed states** $(i, d)$: 6×12 cells × 8 labels = 576 states, plus 8 absorbing output slots.

Write $Q$ for the 576×576 matrix of brain→brain moves and $R$ for the 576×8 matrix of brain→output moves. Then:

- **Distribution after $t$ steps:** $x_t = x_0 Q^t$ (Derived).
- **Probability of exiting at slot $a$ within one bar of $K = 12$ steps,** given entry at the vision cell for slice $s$:
$$A_{s,a} = \sum_{t=0}^{K-1} \big(e_s Q^t R\big)_a$$
  This **8×8 matrix $A$ is the cat's policy**: the probability of each action given each slice (Derived).
- **Mean time until exit,** with $N = (I-Q)^{-1}$ the fundamental matrix: $\tau = N\mathbf 1$ (Derived).

This is the "probability matrix" the system is built on. Learning changes $Q$; behaviour is $A$.

### 2.1 Drift and diffusion (Derived)
With exchangeable weights (any fresh brain), the interior step probabilities are $b_o/\sum b$:
- **Downward drift:** $v = \sum_o p_o\,\Delta r_o = 3.3/8.3 = $ **0.398 rows/step**.
- **Spread:** sideways variance **0.723 per step**, vertical variance **0.601 per step**.

### 2.2 The linear theory predicts the real simulation (Verified)
Fraction of energy still in the brain after $t$ steps. Setup: fresh weights, walls plus the bottom sink, uniform random start; simulations average 300 runs (low density) and 60 runs (half full).

| $t$ | linear theory $\sum x_t$ | real code, low density (3% fill) | real code, half full |
|---|---|---|---|
| 1 | 0.930 | 0.928 | 0.943 |
| 5 | 0.690 | 0.677 | 0.740 |
| 12 | 0.316 | 0.327 | 0.419 |
| 24 | 0.065 | 0.075 | 0.081 |

At low density the linear chain matches the actual code to within ~1 percentage point. At half fill the brain empties more slowly (0.42 vs 0.32 at $t=12$). That gap is the cost of crowding: blocked targets, plus the one-unit-per-cell throughput limit (§4).

### 2.3 Residence time and the bar length (Verified)
- A unit entering at the top row stays **$\bar\tau = 15.4$ steps** on average in a fresh brain.
- A bar is 12 steps, so the vision signal reaches the output within the same bar only **44%** of the time. The "12 steps is enough to cross 6 rows" assumption in BAR_STRUCTURE.md holds only on average for the drift alone: 6 rows ÷ 0.4 rows/step ≈ 15 steps, which is already more than 12.
- Training raises the within-bar exit probability to 0.75–0.99 (§3.2), so learning *shortens* the brain's response time.
- **Design rule (Derived):** for a reliable response within a bar, choose $K \gtrsim \bar\tau + 2\sigma_\tau$. Alternatively, end the bar on the first exit.

### 2.4 Sideways blur limits an untrained brain (Derived, Verified)
- After $\bar\tau \approx 15$ steps, the sideways spread is $\sigma \approx \sqrt{0.723 \times 15} \approx 3.3$ columns. A unit entering at column $s$ exits somewhere within about ±3 slots.
- So the untrained policy is a **blurred diagonal** (theory_checks §3). Slice 0 exits at slots 0/1/2 with probability 17/8/7%; slice 7 exits at slots 7/6/5 with 14/9/7%.
- Its mutual information is only **0.30 bits**, out of a possible 3.

---

## 3. Behaviour can be predicted from the weights alone

### 3.1 The wiring prior (Verified, with a Prediction)
- The current output slots map to directions as 0 NW, 1 N, 2 NE, 3 W, 4 E, 5 SW, 6 S, 7 SE. Vision slices run in angle order, starting from the +x axis.
- Combined with the blurred-diagonal policy, an untrained cat leans **away** from the mouse: predicted towards-rate 0.36, $E[\cos] = -0.22$.
- If instead slot $s$ were wired to the direction at angle $45s°$ (E, SE, S, SW, W, NW, N, NE), the *same* untrained weights would give towards-rate **0.66** and $E[\cos] = +0.24$.
- **Prediction P4:** with that wiring, learning starts from a helpful prior instead of a harmful one. Acquisition should be faster, and early catch rates higher.
- This is an architectural choice, not a learned one. Whether a built-in prior fits the project's goals is the user's call.

### 3.2 The policy matrix predicts trained behaviour (Verified)
For each of the 16 brains trained in Phase 3, I computed $A$ from its saved weights alone (linear chain, $T=1$, no simulation), then compared it with what that brain actually did over 10,000 further bars (arm A):

| Policy statistic, from weights | correlates with | r (n=16) |
|---|---|---|
| predicted towards-rate | measured towards-rate | **0.82** |
| predicted $E[\cos]$ (alignment of the chosen move) | catches | **0.80** |
| mutual information $I(S;A)$ | catches | 0.41 |

- **Evaluating a brain analytically takes milliseconds,** versus minutes of simulation, and explains about two-thirds of the variance in hunting success across brains.
- Mutual information predicts less well than alignment. A brain can be *decisive* without being *right*.
- **Practical consequence:** $A$ is a cheap fitness function, useful for screening configurations, for checking learning progress, and as a well-defined reward signal for a higher layer (§9).

---

## 4. Crowding: the flow of many units

Above low density, units block each other. Each cell holds at most 2 units and moves at most 1 per step. That makes the system a member of the **exclusion-process** family: ASEP/TASEP are the textbook cases, with capacity 1. Here the capacity is 2 and the motion is direction-dependent.

**Mean-field current** (Derived under an independence assumption, not yet verified). Suppose cells are independent and each unit position is occupied with probability $p$ (pressure), as in the lab's starting state. Then

$$J(p) \;\approx\; v\,\underbrace{[1-(1-p)^2]}_{\text{cell occupied}}\;\underbrace{(1-p^2)}_{\text{target not full}},$$

which peaks at **$p \approx 0.5$ with $J \approx 0.56\,v$**, much as TASEP's $\rho(1-\rho)$ peaks at half filling.
- **Prediction P1:** a sealed, wrapped brain with downward bias carries its largest circulating current at pressure ≈ 0.5, and less above or below.
- Above that density the brain approaches the jammed states seen in F19.

---

## 5. A driven brain: Little's law and the pressure valves

With inflow $\lambda$ (units per step) and mean residence $\bar\tau$, the time-averaged energy in the brain is $\bar E = \lambda\,\bar\tau$ (Little's law; Derived). Pressure is $p = \bar E/(2N)$, with $N = 72$ cells.

- **The drain threshold $p = 0.1$** means $\bar E = 14.4$ units, so it needs $\lambda \approx 14.4/15.4 \approx 0.94$ units/step, about **11 units per bar**.
- **The actual inflow:**
  - vision: 1 unit per bar;
  - meta cross-flow: at most 10 per bar (5 cells × 2), usually far less.

**Consistent:** this explains why measured pressure peaks around 0.056 (about 8 units) and why the valves never engage (F19, CROSS_FLOW_ARCHITECTURE.md). They were designed for an inflow roughly 10× larger than the brain receives.

**Design rule:** to make a valve engage at inflow $\lambda$, set its threshold below $\lambda\bar\tau/(2N)$.

---

## 6. Learning as a reinforced urn

Focus on one directed state $(i,d)$. Its 8 outgoing weights are an urn: each time a unit passes through, one route is chosen with probability $\propto (b_o W_o)^{\alpha}$, and that route gets $+h$. This is a **generalised Pólya urn**, and the urn literature tells us what happens:

| Exponent $\alpha = 1/T$ | Bias | Long-run outcome |
|---|---|---|
| $\alpha = 1$ ($T=1$, the cat) | flat | **Coexistence.** Route shares converge to a *random* limit (Dirichlet), set by early chance: path dependence without lock-in. |
| $\alpha = 1$ | unequal | The highest-bias route wins, but only polynomially: $W_o/W_{\max} \sim n^{(b_o-b_{\max})/b_{\max}}$ after $n$ uses (Derived from $d\ln W_o/dn = h\,b_o/S$, with $S \approx h\,b_{\max}n$). |
| $\alpha > 1$ ($T<1$) | any | **Monopoly.** One route eventually takes every draw, almost surely, after a finite time ("balls-in-bins with feedback" results). |
| $\alpha < 1$ ($T>1$) | flat | **Equalisation.** Shares converge to equal; usage erases structure. |
| $\alpha = \infty$ ($T=0$) | any | **Frozen from the start:** the strongest route is always taken. |

**Consistent with lab sweep S.3** (wrap, random order):

| | T=1, hebb on | T=1, hebb off | T=0.3, hebb on | T=0.3, hebb off |
|---|---|---|---|---|
| persistence (flat bias) | 0.031 | 0.026 | **0.737** | 0.044 |
| persistence (down bias) | 0.028 | 0.011 | **0.678** | 0.064 |

- **At $\alpha = 1$, usage barely matters** (coexistence). At $\alpha \approx 3.3$, usage creates channels (monopoly). Without reinforcement ($h=0$) there's no urn, so no lock-in at any temperature.
- **So the temperature decides whether "probabilities that strengthen with use" produce lasting structure. The threshold is $\alpha = 1$.** This is the formal version of the design intent in REVIVAL_PLAN.md §2.

### 6.0 The route choice is a softmax policy (Derived; tested 2026-10-25)
In log-weights $\theta=\ln W$, the choice rule is $p(o) = e^{\alpha(\theta_o+\ln b_o)}/\sum_{o'}e^{\alpha(\theta_{o'}+\ln b_{o'})}$, a softmax, with $\partial\ln p(c)/\partial\theta_o = \alpha(\mathbb 1[o=c]-p(o))$. So REINFORCE (Williams 1992) gives the principled update $\Delta\theta_{s,o}=\eta(R-\bar R)\alpha(\mathbb 1[o=c_s]-p_s(o))$ for each traversed directed state $s$, and the simulation's `su` path record is exactly the set of $s$ it needs. Each directed state is a stochastic learning automaton, and the brain is a team of them sharing one reward.

**Tested** (REVIVAL_PLAN.md §6, G.R1/G.R2): implemented as `--pg-update` (± `--rpe`). It **did not beat** the tuned heuristic rule. It peaks at η≈1–2, statistically tied, while some seeds collapse, and the baseline had no effect. A plausible, untested reason: directed states are **shared across vision slices**, and lowering sibling routes (the −p term) interferes with other slices' paths. That's a known weakness of policy gradient on shared representations, and a reason the "only touch what was used" heuristic is robust here.

### 6.1 Known models this is closest to
- **Ant colony optimisation.** Choice $\propto \tau^{\alpha}\eta^{\beta}$ (pheromone × heuristic), deposit on use, evaporation. Here $W$ plays the pheromone $\tau$ and $b$ the heuristic $\eta$, but **both are raised to the same power $1/T$**. ACO treats $\alpha$ and $\beta$ as separate knobs. *Suggestion:* apply the temperature to $W$ only, so exploitation of learned routes and the strength of innate drift can be tuned independently.
- **Physarum (slime-mould) transport networks** (Tero et al.). Tube conductance grows with the flux through it and decays otherwise. With flux exponent $\mu > 1$ the network collapses to a single path; with $\mu < 1$ it stays a mesh. The threshold structure mirrors $\alpha = 1$ here, and the lab's "channels" are a discrete, stochastic, many-particle analogue.
- **Edge-reinforced random walks** (Coppersmith–Diaconis): one walker, with an edge's weight growing each time the walker crosses it.

**Possibly novel (Conjecture; needs a literature check before any claim):** the combination here is
- many walkers,
- capacity-2 exclusion,
- direction-conditioned (second-order) reinforcement,
- multiplicative decay,
- tunable superlinearity.

I don't know of a standard model with all five. The second-order part means the reinforced object is a *turn*, (incoming → outgoing), rather than an edge. That's what lets lanes keep their heading.

---

## 7. Decay sets the memory, and the reinforcement number

**Memory time (Derived).**
- Mean decay per application is $\bar\delta = 0.9875$, applied every 60 steps. So weights forget with an e-folding time of
$$\tau_d = \frac{60}{-\ln\bar\delta} \approx 4{,}770 \text{ steps } (\approx 400 \text{ bars}).$$
- An unused route takes $\tau_d \ln(1000/25) \approx 17{,}600$ steps to fall from 1000 to the floor.

**Equilibrium weight (Derived).** Take a route used $u$ times per step. Its weight obeys $dW/dt \approx h u - W/\tau_d$, so
$$W^* = h\,u\,\tau_d \approx 4.8{\times}10^6\,u .$$
Any route used more than 0.42 times per step sits at the cap, and capped routes tie with each other, so the choice among them falls back to the bias alone.

**The reinforcement number (Conjecture, proposed as a control parameter).**
$$\mathcal R = \frac{h\,u\,\tau_d}{\bar W_0}$$
is the ratio of experience-driven weight to innate weight at equilibrium, with $\bar W_0 \approx 512$ the mean initial weight.
- $\mathcal R \gg 1$: experience dominates, and structure locks.
- $\mathcal R \ll 1$: innate wiring dominates.
- Currently $\mathcal R \approx 9{,}300\,u$, so any route used more than about once per 10,000 steps becomes experience-dominated.

**Consequences:**
- **Lab runs (Consistent).** Lab runs of 3,000 steps are shorter than $\tau_d$, so structures *cannot* reorganise within a run. That matches S.3: route turnover sat at the noise floor.
- **Trained brains (Verified).** 20,000 bars is 240,000 steps, about $50\tau_d$, so trained brains should split into two groups: forgotten routes at the floor, and used routes far above it. Measured over 16 trained brains: **96% of synapses ≤ 100, 2.8% ≥ 10⁶, only 1% in between**. A trained brain is a sparse skeleton of about 130 strong routes.
- **Prediction P3 (S.3b).** Reorganisation, i.e. structures that break down and re-form, needs $\tau_d$ comparable to or shorter than the run, **and** $\mathcal R$ near 1–10 for off-channel routes. Lower $h$ and decay more often: decaying every 5 steps gives $\tau_d \approx 400$.
- **Prediction P2.** Lock-in at $\alpha > 1$ takes longer the closer $\alpha$ is to 1. So with finite memory, channels should appear only once $\alpha$ is clearly above 1: a transition somewhere between $T \approx 0.5$ and $0.9$. That transition should move towards $T = 1$ as $\tau_d$ grows.

---

## 8. The cat's rewards in log space

Rewards multiply weights, so it's natural to track $\ln W$ (Derived from the code):

| Event | Effect on $\ln W$ of each synapse used |
|---|---|
| one reinforce call, ×$1.2/0.95^{12} = 2.22$ | **+0.80** |
| one punish call, ×$0.8\cdot0.95^{12} = 0.432$ | **−0.84** |
| one good move: $k = \text{round}(1 + 3\,\text{dotP})$ reinforce calls | a typical 5-unit move gives $k \approx 15$, so **+12**; a 10-unit diagonal gives up to $k = 43$, so +34 |
| strategic lookback: ×$1.1^{m}$ per lookback bar the synapse was used in | +0.095·m per bar; over 80 bars up to +7.6 (m=1) or +15.2 (m=2) |
| decay, every 60 steps | −0.0126 |
| range from floor to cap | $\ln(2{\times}10^6/25) = 11.3$ |

- **One rewarded move takes a synapse from anywhere to the cap** (Derived): +12 is more than the whole 11.3 range.
- **Undoing it takes about 9–10 punish calls,** i.e. 3–10 away-moves.
- **The learning rule is effectively binary:** a route is either "used and rewarded" or "forgotten". The bimodal weight distribution in §7 confirms this. Graded probability learning, which is the point of a probability-matrix brain, is lost at the cat level.
- **Suggestion:** make each reward a bounded increment in log space, e.g. $\ln W \mathrel{+}= \eta\cdot\text{reward}$ with $\eta \ll 11.3$. Then many experiences shape a weight, not just the first one.
- ~~**Prediction P7:** with bounded log-rewards, the bimodality disappears and policies become graded.~~ **Refuted (2026-10-25, REVIVAL_PLAN.md §6, G.3b).** Scaling every primary reward's log-step by λ made weights *more* concentrated, not graded: weights between 100 and 10⁶ fell from 3.1% (λ=1) to 0.5% (λ=0.25) and 0.3% (λ=0.1), and strong weights (≥10⁶) fell from 2.7% to 1.9% and 1.4%. Hunting improved strongly anyway (alignment +0.149 at λ=0.25). A likely reason, not yet tested: the reward was never the only force on a weight. Hebbian +1000 per use (§7, $W^*=hu\tau_d$) still saturates routes the cat uses, and gentler rewards let fewer wrong routes get promoted, which leaves a sparser, more accurate skeleton. So the benefit comes from *fewer wrong routes*, not from graded weights.

---

## 9. Hierarchy: structure on structure (Conjecture)

The design intent (REVIVAL_PLAN.md §2) is that a higher system sets how much reward a lower one gets, and the higher system has its own, more basic reward. In this theory's terms:

- **Each layer $\ell$ has its own parameters:** an exponent $\alpha_\ell$, a memory $\tau_{d,\ell}$ and a reinforcement number $\mathcal R_\ell$.
- **Setting the lower layer's reward sets its effective $h_\ell$,** and therefore **$\mathcal R_\ell$**. So *the job of layer $\ell+1$ is to control how strongly and how lastingly layer $\ell$ lays down structure.*
- **Proposed principle:** a useful controller keeps the layer below near the boundary between lock-in and forgetting ($\mathcal R$ near 1–10, $\alpha$ just above 1). There, structures persist long enough to be useful but can still reorganise when the reward changes. This parallels the "edge of chaos" idea in reservoir computing.
- **Timescale condition (Derived):** a layer can only learn from consequences that arrive within its own memory. The meta-brain's reward is the catch rate over 200 bars, and its memory is $\tau_d \approx 400$ bars, so the condition holds. But the meta-brain's own reward, `do[rateCounter; ×2.22]`, is up to 200 multiplications by 2.22 (+160 in log space). That saturates it instantly, the same binary problem as §8, which is consistent with the null meta-brain ablation in Phase 3.
- **The policy matrix $A$ (§3) offers a natural "basic reward" for higher layers:** $E[\cos]$ predicts success (r = 0.80) and is computable from weights alone.

---

## 10. Summary of principles

1. **The brain is an absorbing Markov chain on (cell, incoming-direction) states.** Its policy is the chain's within-bar absorption matrix, which is computable from the weights.
2. **Drift, diffusion and residence time** follow from the bias vector alone for any untrained brain. The current numbers: drift 0.40 rows/step, sideways variance 0.72 per step, residence 15.4 steps, longer than the 12-step bar.
3. **Crowding** turns it into a capacity-2 exclusion process. Throughput peaks near half filling (mean-field).
4. **Little's law** links inflow, residence and pressure, and shows the valves are out of reach at current inflow.
5. **Use-strengthened choice is a generalised Pólya urn.** Lasting structure requires $\alpha = 1/T > 1$ together with $h > 0$.
6. **Decay sets the memory $\tau_d$.** Together with $h$ it defines a reinforcement number $\mathcal R$ that sets whether structure locks, forgets, or reorganises.
7. **The cat's multiplicative rewards act as switches, not gradients.**
8. **A higher layer can be understood as controlling the lower layer's $\mathcal R$.**

## 11. Predictions to test

| # | Prediction | Test | Plan task |
|---|---|---|---|
| P1 | Circulating current in a sealed, wrapped, downward-biased brain peaks near pressure 0.5 | lab sweep over `--fill`, measuring net downward flux | S.5 (new metric) |
| P2 | The channel transition lies between T ≈ 0.5 and 0.9, and moves towards T = 1 as $\tau_d$ grows | lab sweep T × decay interval | S.3b |
| P3 | Turnover rises above the noise floor once $\tau_d$ is below the run length and off-channel $\mathcal R$ ≈ 1–10 | lab sweep decay × hebb | S.3b |
| P4 | Angle-ordered output wiring makes an untrained cat lean towards the mouse (towards 0.66 vs 0.36) and speeds acquisition | policy matrix + paired training runs | M.4 |
| P5 | The weights-only $E[\cos]$ predicts catches for *other* conditions too (meta-only, new seeds) | rerun §3.2 on arms B–D | M.3 |
| P6 | Pressure reaches 0.1 only when inflow ≳ 11 units per bar | open-system lab with controlled injection | S.5 |
| P7 | Bounded log-space rewards remove weight bimodality and give graded policies | weight histogram + policy MI after training | **Refuted** (G.3b): weights became sparser, while hunting improved |

## 12. Reproducing the numbers

```bash
q q/programs/theory_checks.q < /dev/null     # sections 1-3 always; 4-5 need results/p3_* (Phase 3)
```
