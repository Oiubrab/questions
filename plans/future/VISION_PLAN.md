# Plan: seeing the whole world without drowning in it

**Status (2026-11-03):** written, not started. Waiting on the user's decisions in §7. Nothing here is built yet.

**Why it exists.** The user (2026-11-03): "a real cat … doesn't just see the mouse, the cat sees the entire world. And in this case, if we just put, say, like four walls around the environment … all of the elements of the input vector would be lit up, and it would get no information about position of anything that it was seeking. … can we set up a proper environment first and then can we set up a way to input that environment into the vision that doesn't completely overwhelm the vision brain with useless or with non-differentiating information."

**Relation to other plans.** The [obstacles plan](../inProgress/OBSTACLES_PLAN.md) (paused) measures walled worlds with **invisible walls**: they block sight and movement but send nothing to the eye, so the cat only learns about them by touch (3 units' range) and by being blocked. That plan's §6 shows the cat getting trapped against walls. This plan lets the cat see walls without losing sight of the mouse. Its results become the reference ("V0") for this plan. The [3D plan](3D_PLAN.md) would inherit whatever encoding wins here.

**How to use this document:** the same rules as the obstacles plan. Tasks are checkboxes (`[ ]` not started, `[~]` partly done, `[x]` done and verified). Tick a task only after its acceptance check passes. Every session appends to the Progress Log (§9). Decisions in §7 belong to the user. Claims need ≥ 16 paired seeds. User-stated principles go into the default, not behind a flag.

---

## Architecture

### Today (the default since 2026-11-02)

The primary side: the senses, the central brain and the move.

```
WORLD  100×100, wrapped: cat, mouse, dog (--distractor), walls (--world)
       walls block sight and movement, but the eye never sees them
   │                                  │                                      ▲
   │ sight: the mouse's slice         │ touch: wall within 3 units → 1 unit  │ move
   │ → 1 unit (dog: 1 unit, label 6)  │ last move blocked that way → 2 units │ (slot value × 5 units)
   ▼                                  ▼                                      │
┌────────────────────┐      ┌────────────────────┐                           │
│ SIGHT BRAIN  3×12  │      │ TOUCH BRAIN  3×12  │                           │
│ in: top cols 2–9   │      │ in: top cols 2–9   │                           │
└─────────┬──────────┘      └─────────┬──────────┘                           │
          │ bottom cols 2–9, label 7  │ bottom cols 2–9, label 6             │
          └─────────────┬─────────────┘                                      │
                        │ every step; same column = same direction;          │
                        │ a unit that finds its cell full is lost            │
                        ▼ top cols 2–9                                       │
              ┌────────────────────┐                                         │
              │ CENTRAL BRAIN 3×12 │◄── right col, rows 1–2: reward brain    │
              └─────────┬──────────┘    out[0] and out[1] from the last bar  │
                        ▼ bottom cols 2–9                                    │
              OUTPUTTER  8 slots ── the fullest slot is the direction ───────┘
                                    (angle wiring; ties → the lowest slot)
```

The reward (meta) brain: its input is how full the cat is, and it is motivated to be full.

```
satiety: a leaky food store (+1 per catch, 200-bar decay)
   │ one unit a bar into cell 0 (starving) … cell 4 (full)
   ▼ top cols 0–4
┌────────────────────┐
│ REWARD BRAIN  3×5  │  rewarded for being full, punished for starving
└─────────┬──────────┘
          ├── out[0] → scope: reinforce the routes of the last 20 / 40 / 80 bars ┐ strategic
          ├── out[1] → magnitude ×1 / ×2                                         ┘ reinforcement
          └── all 5 outputs → the central brain's right column, next bar
              (only out[0] and out[1] land: a 3-row central brain has only rows 1–2 there)
```

The timing within one bar, and the learning that follows:

```
one bar = 12 steps; each step:  sight ─► touch ─► transfer into central ─► central ─► reward brain ×2
then the cat moves, and learning acts on the stacked primary state
(sight rows 0–2, touch rows 3–5, central rows 6–8):
  · usage: +250 on every synapse used, whatever the reward
  · direct reward: cosine of the move against the direction to the mouse (none while it's hidden)
  · strategic reinforcement: scope × magnitude from the reward brain, gated by the recent catch rate
  · catch boost; decay every 5 bars
```

`--single-brain` replaces sight, touch and central with the original 6×12 brain (vision into its top row, the reward brain into its right column, rows 1–5). `--efference` and `--spontaneous` add internal input; both are off by default.

### What this plan adds (★ = new; nothing is built yet)

```
WORLD  ★ objects with a kind, shape, size, motion and colour; ★ box layout (four walls)
   │
   ▼
★ RENDERER   32 rays from each of ★ two eyes (b apart, across the heading);
             each ray returns the first thing it hits:
             colour · distance · moved since last bar
             (and its kind, used only for scoring, never sent to the brain)
   │
   ▼
★ ENCODER    the view → input cells, 0/1/2 units each: 8 slices, or ★ 8 slices × K colour channels
             contrast, motion, depth, small-object, colour, salience cap (V2–V7),
             depth measured by binocular disparity or motion parallax (V8, V9)
             naive presence (V1) and kind labels (V6L) are the controls
   │
   ├───────────────────────────┐                                  touch: unchanged
   ▼ form / "what"             ▼ ★ motion / "where" (an option)        │
┌──────────────────┐   ┌──────────────────┐   ┌──────────────────┐     │
│ SIGHT BRAIN 3×12 │   │ ★ 2nd SIGHT 3×12 │   │ TOUCH BRAIN 3×12 │◄────┘
└────────┬─────────┘   └────────┬─────────┘   └────────┬─────────┘
         └──────────────────────┼──────────────────────┘
                                ▼ top cols 2–9, told apart by label
                     ┌────────────────────┐
                     │ CENTRAL BRAIN 3×12 │◄── reward brain (unchanged)
                     └──┬──────────────┬──┘
   bottom cols 0, 1, 10, 11            bottom cols 2–9
                ▼                      ▼
        ★ REST OUTLET              OUTPUTTER  8 move slots
        energy leaves without      (decision 2c: the cat stays
        moving the cat; still      still when rest wins)
        counted (conservation)
```

- The second sight brain is routing option (c) in §5. Option (a) instead keeps one sight brain and tells the channels apart by label; option (b) uses a wider 3×20 sight brain.
- The rest outlet is §7 decision 2. Its sight-brain variant (d) would sit at the sight brain's bottom corners, filtering input before it reaches the central brain.
- With colour (V6), the sight brain is wider: 3×28 for 3 colours (layout (a), §7 decision 8). Alternatively there is one colour brain per channel, drawn like the second sight brain.
- Binocular route (b) (§7 decision 6) would replace the form/motion split with left-eye and right-eye sight brains, or would feed both eyes' images into one sight brain under separate labels. Route (a) does the comparison in the encoder and leaves the brains as drawn.

---

## 0. Goal

The cat sees everything in its world (walls, mouse, dog, landmarks), and its input still tells it **where the mouse is**, plus **where the walls are**, as separable signals.

Measured, per world and encoding:
- **Information about the mouse**, in bits per bar: the mutual information between the input vector and the mouse's slice while it's visible. Today's input carries up to 3 bits (1 of 8 slices). A naive "anything in this slice" input carries about 0 bits in a walled box.
- **Load**: input units per bar and the share of bars with ≥ 6 of 8 cells lit ("overwhelmed").
- **Information about distance:** the mutual information between the input and the distance band of the nearest surface. This is the near/far contrast; binocular vision (V8) and motion parallax (V9) are meant to raise it.
- **Behaviour** after learning: catches per 1,000 bars, alignment while visible, and how often a move is blocked. Compare with today's invisible-wall input on the same seeds.

North-star link: heading towards the mouse must survive richer input, which is the open-world test. Seeing walls should stop the wall trapping that blocks every hidden-mouse result so far. It doesn't give the cat memory; that's the substrate problem (obstacles plan I.1/I.2; 3D plan stage A).

## 1. The problem, precisely

Three properties of the current system combine:

1. **Presence coding.** A slice injects one unit if something is in it. In any enclosed world, every ray hits something, so every slice is always lit. The input then varies with nothing, and carries no information.
2. **Every input unit becomes a move vote.** Energy is conserved and leaves a brain only through its bottom-row outputs. In the central brain, those outputs are the 8 move directions. A brain can't ignore an input; it can only route it to *some* move. Wall units would push the cat in whatever directions they drain to. The move is the slot with the most energy (ties go to the lowest slot), so 8 lit inputs turn the move into noise. That's worse than no information.
3. **Capacity.** A cell holds at most 2 units (HIGH). The sight brain has 8 input cells, and a unit that reaches a full cell in the central brain's top row is lost. Flooding saturates the input row, so the mouse's unit can't even get in.

So two problems need solving: **encoding** (the input must carry differences, not presence) and **disposal** (the brain needs a way to not act on input).

## 2. How animals solve it (the design source)

- **Retinas report contrast, not light.** Centre-surround cells (lateral inhibition) respond weakly to a uniform field and strongly to edges and small spots. A uniform wall filling the view sends little.
- **Transient cells report change.** Cats are strongly motion-sensitive, and a still mouse is hard for them to see. A static scene seen by a still eye fades.
- **Feature detectors before the brain.** "What the frog's eye tells the frog's brain" (Lettvin et al., 1959): frog retinal cells fire for small, dark, moving, convex things (bugs) and ignore large static ones. The recognition is done by the eye's wiring, from physical properties (size, motion, contrast), not from labels.
- **Separate pathways.** A fast "where/motion" pathway and a slower "what/form" pathway (magno/parvo, dorsal/ventral). That fits the modular brain the user already chose.
- **Attention.** Only a small part of the visual field gets full processing at a time (saccades).
- **Colour is contrast between surfaces.**
  - Receptor types (cones) with overlapping sensitivities each report how much of their band comes back. A colour is the pattern across them, so it separates things that are equally bright.
  - Cats have two cone types and see colour weakly, people have three, and mantis shrimp have a dozen or more. Pit vipers add a heat sense.
  - A simulation can have as many channels as it likes.
- **Depth comes from parallax.**
  - **Binocular disparity:** each eye sees a near object at a slightly different angle, and the brain compares the two images. In mammals the comparison happens in the visual cortex, not in the eye. Depth precision falls with the square of the distance, so stereo is a near-range sense in every animal; cats use it to judge a pounce.
  - **Motion parallax:** as the animal moves, near things sweep across the view faster than far ones. It works with one eye.
  - Near things matter more, and depth jumps are where contrast is.

## 3. Design intent that applies (revival plan §2)

- The core mechanism doesn't change: energy moves by usage-weighted probabilistic choice, and it's conserved (point 6). Any outlet added in §5 stays inside the conservation accounting.
- Unconditional usage reinforcement stays (point 7).
- A brain always receives input (point 8). The encodings below can leave the sight brain empty in a blank scene, as it is today whenever the mouse is hidden. Internal input (obstacles plan W.1b) serves point 8, not the eye.
- Modular brain: sense brains feed the central brain (obstacles plan §7 decision 4). New senses or pathways become new sense brains.

---

## 4. Stage E: a proper environment

- [ ] **E.1 Object model** (`.world`). Everything in the world is an object with physical properties:
  - kind (`wall mouse dog landmark`);
  - shape: rectangle for walls, disc for animals;
  - size (mouse radius 1, dog 2);
  - whether it moves;
  - colour: K channel intensities (V6, §5), so that kinds differ, and walls can differ among themselves.

  Today's mouse and dog become disc objects; walls keep the existing rectangle columns. *Accept:* existing layouts give byte-identical runs (golden digest unchanged); unit tests in `test_world.q`.
- [ ] **E.2 Layouts:**
  - `box`: four walls along the field edges, the user's example. It lives in the wrapped world, so no new geometry code is needed; nothing crosses an edge.
  - `box+pillars` and `box+rooms`.
  - Optional: `landmarks`, which gives some walls a different colour so the cat could orient by them.

  *Accept:* clear spawns and line-of-sight tests pass for each layout.
- [ ] **E.3 Renderer (the retinal image).** Ray casting from the cat. R rays (default 32, i.e. 4 per vision slice) each return the first object hit within range, as a per-bar view table: `ray angle kind colour dist moved`. `kind` is for scoring only. Here `moved` means the object moved in the world since the last bar. Every encoding in Stage V is a function of this table, so encodings are interchangeable and testable without a brain. It reuses `.world.hitT` (slab test) plus a ray–disc test. *Accept:* unit tests:
  - from the centre of `box`, every ray hits a wall at the expected distance;
  - a mouse in front of a wall occludes it;
  - wrapped copies are seen across edges;
  - the cost is ≤ 0.3 ms a bar per eye (measured). The brain costs about 1.2 ms a bar, so the renderer must not dominate.
- [ ] **E.4 Mouse motion schedule** (only if motion is kept, §7 decision 3). Today the mouse jumps 2 units every 10 bars, so a motion cue would see it on 1 bar in 10. Add a small step every bar at the same average speed (0.2 units a bar). This changes the default world, so it needs a new golden digest.
- [ ] **E.5 Viewer:** draw the rays and what each one hits in the dashboard, so an encoding can be checked by eye. Optional.

## 5. Stage V: encodings, and a way to ignore input

### Encodings

Each encoding maps the view table to the 8 input cells: a value of 0, 1 or 2 units per cell, plus a label. They're listed from the reference to the most selective.

| | Encoding | What lights a cell | Four-walls box, cat in the middle | Mouse in front of a wall |
|---|---|---|---|---|
| V0 | today | the mouse is in the slice | nothing (walls invisible) | lit |
| V1 | naive presence (negative control) | anything is in the slice | **all 8 lit**, the user's case | lit, indistinguishable from the walls |
| V2 | depth | nearest surface: 2 units if near, 1 if mid-range, 0 if far | nothing; near a wall, those slices light | lit only if near |
| V3 | contrast (lateral inhibition) | the slice's view differs from its neighbours' (depth or colour jump; per colour channel with V6) | nothing (uniform); corners and doors light | lit: a small depth jump |
| V4 | motion (transient) | something in the slice moved in the world since last bar | nothing | lit when it moves (needs E.4) |
| V5 | small-object ("bug") detector | the object fills less than one slice | nothing | lit |
| V6 | colour (K receptor channels per slice) | each slice has K cells, one per colour channel; a cell lights with the strength of its channel in what the slice sees | wall-colour cells lit all round, the rest dark | the mouse-colour cells light in its slice, wall-colour cells in the others |
| V6L | kind label (control: the world names it) | mouse and wall enter the same 8 cells with different labels, as the dog does now (label 6) | all lit, wall-labelled | lit, mouse-labelled |
| V7 | salience cap (attention) | any of the above, but only the k most salient cells (k = 1 or 2) | as the base encoding | as the base, capped |
| V8 | binocular (two eyes) | depth measured by the cat from the disparity between two eyes' images: (a) the encoder compares them, or (b) both images enter the brain, which must compare them | in the centre nothing is near; near a wall, the near slices light | lit as near or far, against a far wall: a depth jump |
| V9 | motion parallax (one eye) | depth from how far a thing's direction shifts as the cat moves (the move is known: the cat made it) | as V8 | as V8 |

Notes:
- V2–V6 do the recognition the way an eye does it, from physical cues. V6L hands the brain the answer, so it's the upper-bound control.
- V3 and V4 are the two strongest candidates on paper: V3 removes uniform walls, and V4 removes everything static. They can be combined (V3+V4).
- Corners and doors lighting under V3 is useful, not noise: they're landmarks, which a hidden-mouse search will eventually need.
- Labels are the substrate's only built-in "colour" channel, but a label is also the direction a unit came from. That sets its next move, so using labels as colours bends the walk. V6L measures how much that matters. Colour (V6) avoids the problem by giving each channel its own input cells.
- V2 takes distance straight from the renderer, so the world measures depth for the cat. V8 and V9 let the cat measure it itself. V2 becomes their upper bound, and V3 (contrast) can run on measured depth instead of given depth.

### Binocular vision (V8; added 2026-11-03)

> **The user (2026-11-03):** "binocular vision. Because then you could use parallax to judge the distances of things which might help in terms of differentiation between near and far objects. Which, would give some contrast in the world as well."

**Geometry.**
- Two eyes sit b units apart, across the cat's heading.
- **The cat has no heading today:** its vision slices are compass directions. The simplest heading is the last move direction (§7 decision 7). The eyes still see all the way round, in compass slices.
- An object along the line through both eyes gives no disparity; one straight ahead or behind gives the most.
- The disparity of an object at distance d is 2·atan(b/2d):

| Eye separation b | d = 5 | d = 10 | d = 20 | d = 50 | Seen in different slices by the two eyes, with 8 slices (45°) | …with 32 rays (11.25°) |
|---|---|---|---|---|---|---|
| 2 (the catch radius) | 22.6° | 11.4° | 5.7° | 2.3° | within 2.4 units | within 10 units |
| 4 | 43.6° | 22.6° | 11.4° | 4.6° | within 4.8 units | within 20 units |

So **at today's 8 slices, stereo sees almost nothing beyond the catch radius**. It needs the 32-ray render: the cat moves 5 or 10 units a bar, so a 10–20-unit stereo range covers the next one or two moves. That makes binocular vision depend on §7 decision 4. The encoder route (a) works from the 32-ray render whatever the input width. The brain route (b) needs the wider input, because each eye's image must reach the brain at full resolution.

**Two ways in** (§7 decision 6):
- **(a) The encoder compares the eyes.** It finds each object's ray in both images and turns the disparity into a near/far value in the 8 input cells. This is V2 with depth measured, not given. Cheap, and certain to carry information.
- **(b) The brain compares them.** Both images enter the brain, either as left-eye and right-eye sight brains or as one sight brain with each eye's units under its own label, and the brain has to discover disparity itself. That is how mammals do it.
  - The substrate already has a coincidence detector: a cell holding two units that arrived from two directions is HIGH with two labels, and its next move uses the average of both labels' weight rows (THEORY.md).
  - So units from the two eyes that meet in a cell behave differently from either one alone.
  - Whether learning can turn that into a depth readout is an open substrate question. V.B3 tests it before any cat run.

**Tasks:**
- [ ] **V.B1 Two-eye renderer** (extends E.3): render from both eye positions, with b as a parameter. *Accept:* unit tests that an object at a known distance appears at the expected ray offset (2·atan(b/2d)) between the eyes, and zero offset along the line through the eyes.
- [ ] **V.B2 Screen** (with S.1). I(input; distance band of the nearest surface), and I(input; mouse slice), for:
  - V2 (given depth);
  - V8a; V8b (both eyes' images taken together);
  - V9;

  each at 8 and 32 rays, with b = 2 and 4. *Accept:* V8a recovers most of V2's distance information within its range, or the shortfall is reported.
- [ ] **V.B3 Coincidence test in the substrate lab** (`substrate.q`, extended if needed): inject a unit at two top-row cells together versus one at a time, and measure whether the output distribution differs. Then check whether reinforcement can separate "near" pairs from "far" pairs. If it can't, route (b) can't work without substrate changes (3D plan stage A), and (a) is the route.

### Colour vision (V6; defined 2026-11-03)

> **The user (2026-11-03):** "another way that we can discern contrast … is with color. … maybe those rays don't go into a single element in the input vector. They go into three, or maybe five. … We can go way past nature here. … we color each of our objects differently. Like the mouse is one color, and the wall is another color, and obstacles are another color, and so … certain elements of in each kind of subset of input neurons will fire based off of whether or not it's getting back a certain color."

**The model.**
- **Every object has a colour:** a vector of K intensities, one per channel, each 0, 1 or 2 (the units a receptor cell can send).
  - K = 3 by default. K = 5 or more goes past nature.
  - A non-visual channel is possible too: "heat", in which animals glow and walls don't.
- **Each ray returns the colour of the first thing it hits.**
- **Each slice gets K input cells, one per channel, instead of one.** A channel's cell gets the strongest return of that channel among the slice's 4 rays (0, 1 or 2 units). A variant counts instead how many of the 4 rays return the channel, capped at 2. That also carries size: a mouse covering one ray sends 1 unit, a wall filling the slice sends 2. S.1 screens both.
- So the eye has 8 × K input cells: 24 for 3 colours, 40 for 5.

**How it differs from a kind label (V6L).**
- Colour is a property the object has, not its name; the cat has to learn which colours mean "mouse".
- It arrives on cells of its own, so it doesn't bend the walk, and labels keep meaning direction.
- How much recognition it asks of the cat depends on the palette, hence the ladder below.

**Palette ladder** (one difficulty per rung, like the obstacles rungs):

| Rung | Palette | What the cat must learn |
|---|---|---|
| C1 | one-hot: each kind (mouse, wall, obstacle, dog) lights only its own channel (K ≥ the number of kinds) | which channel to follow (a sanity rung: a kind label on separate cells) |
| C2 | overlapping: e.g. mouse (2,0,1), wall (2,2,0), obstacle (0,2,1) | a combination: no single channel identifies the mouse |
| C3 | camouflage: one obstacle's colour one unit off the mouse's | a fine distinction |
| C4 | variation: colours dim with distance (one unit lost beyond a range), and walls differ slightly from each other (landmarks) | recognition that holds up under change |

**Colour doesn't stop flooding by itself.**
- In the box, the wall's channel cells are lit in every slice, every bar. Colour makes the wall input separable from the mouse input, so the brain *can* treat them differently, but those units still have to go somewhere.
- They can go to the rest outlet (decision 2). Or contrast within each channel (V3 per channel) can remove them: a uniform wall colour all round sends nothing, while a colour edge, such as the mouse against a wall, sends a spike.
- **Colour + per-channel contrast + the rest outlet** is the combination expected to win.

**Input layout and routing** (§7 decision 8):
- **(a) One wide sight brain, slice-interleaved:** slice 0's K cells side by side, then slice 1's, and so on.
  - A slice's colours are neighbours, so their units can meet in one cell, which a combination (C2) needs.
  - It is 3 × (8K + 4), i.e. 3×28 for 3 colours.
  - Its 8 outputs to the central brain stay at one per slice, under the slice's middle channel. Its other bottom cells could be the sight brain's rest outlet (decision 2d).
- **(b) The same width, channel-major:** all 8 slices of channel 0, then of channel 1, and so on. A channel's directions are neighbours, but a slice's colours are far apart.
- **(c) One colour brain per channel,** each like today's sight brain (3×12), all feeding the central brain.
  - It reuses everything and fits the modular design.
  - But combinations across channels can only form in the central brain, whose top row loses units that find their cell full.
  - Each colour brain needs its own incoming label there.
- **(d) Opponent channels in the encoder,** as retinas do: brightness, channel 0 − channel 1, channel 2 − (channel 0 + channel 1). Fewer, less redundant inputs. It can be combined with (a)–(c).
- **The constraint:** between bars the brains are stacked into one primary state with one column count (`.mod.stack`, `.syn` usage indices). A wider sight brain means either padding the touch and central brains to the same width, which changes them, or generalising the stack to brains of different widths (usage offsets per brain, not per row). Generalising is the cleaner route.

**Tasks:**
- [ ] **V.C1 Colour model:** K and the palette are parameters; build palettes C1–C4; each ray returns its colour. *Accept:* unit tests that a ray hitting each kind of object returns its colour; with colour off, runs are byte-identical (golden digest unchanged).
- [ ] **V.C2 Encoder:** 8 × K cells, both pooling rules, layouts (a) and (b), and the opponent variant (d). *Accept:* unit tests on hand-built views.
- [ ] **V.C3 Brains:** generalise `.mod.stack` / `.mod.sub` and the usage indexing to brains of different widths; then build the wide sight brain (a/b) or colour brains (c). *Accept:* the cross-brain conservation tests pass at the new widths; `--single-brain` and the colour-off default stay byte-identical; weights files round-trip.
- [ ] **V.C4 Screen, then learn.**
  - Screen (S.1/S.2): for each palette × layout, the information about the mouse's slice, and a scripted reader that is told the mouse's colour (the ceiling).
  - Learning (L.2–L.4): does the cat learn which colour to chase? Measure:
    - alignment while visible in the box, against V0 (only the mouse visible, i.e. perfect recognition handed over);
    - the share of wall-colour energy that ends in the rest outlet.
  - *Accept:* an encoding that matches V0 in the box while also seeing the walls means the cat recognised the mouse itself.

### Routing channels into the brain (decide after Stage S)

There are three ways to feed several channels (say a wall channel and a mouse channel) into the brain:
- (a) one sight brain, with the channels told apart by label;
- (b) a wider sight brain, e.g. 16 input columns (8 per channel) on a 3×20 grid;
- (c) one sense brain per pathway ("motion/where" and "form/what"), each feeding the central brain as touch does. Each extra 3×12 brain costs about 0.3 ms a bar.

### Disposal: an outlet that doesn't move the cat

- [ ] **V.D** Today all energy ends as a move vote (§1 point 2). The proposal is to make the central brain's bottom-row corner cells (columns 0, 1, 10 and 11, currently not outputs) a **rest outlet**. Energy there leaves the brain but moves nothing.
  - The brain could then learn to dump input that doesn't matter.
  - It also gets a learnable "stay still", and waiting in ambush is how real cats hunt.
  - Conservation is unchanged, because the rest outlet is counted in the outputter.
  - The move rule needs one decision: either the rest outlet only absorbs, or the cat stays still when rest outweighs every move slot.
  - Theory follows: the outlet adds absorbing states to the chain. `.theory.chainMod` and `policy_ceiling.q` need the extra columns.
  - A sight-stage variant (corner outlets in the sight brain, which filter before the central brain) is the alternative.

  This changes the brain, so it's §7 decision 2. If adopted it becomes the default (updated golden digest, revival §2 updated), measured against today's behaviour.

## 6. Stages S and L: screen cheaply, then learn

**Speed matters** (obstacles plan §7 decision 5), so every encoding is screened without a brain first, which takes seconds per seed. Only the finalists get 16-seed learning runs.

- [ ] **S.1 Load and information screen** (`q/programs/vision_screen.q`, new).
  - For each encoding × world (open, box, box+pillars, rooms, pillars), run a 20k-bar trajectory with no brain: the cat moves at random, or with the `reactive` policy. Report:
    - load: units per bar and the overwhelmed share;
    - I(input; mouse slice), visible bars only, in bits;
    - I(input; nearest-wall direction), so wall information is counted as useful when it's separable from the mouse.
  - 16 seeds each.
  - *Accept:* V1 in `box` scores ≈ 0 bits (confirms §1); V0 matches its known 3-bit maximum when visible.
- [ ] **S.2 Scripted reader.** A policy that sees only the encoded input: head for the most salient cell, prefer mouse-like cells when the encoding separates them, and move at random when nothing is lit. Report catches per 1,000 bars per encoding and world. This is the ceiling for a memoryless cat on that input: if the scripted reader can't catch with an encoding, the brain won't either.
- [ ] **S.3 Pick 2–3 finalists** from S.1/S.2. Record the table in §8 and ask the user before Stage L.
- [ ] **L.1 Open world: no harm.** Run each finalist against V0, 16 paired seeds, 20k bars, comparing alignment and catches. *Accept:* not significantly worse than V0.
- [ ] **L.2 Box world** (the user's case): catches per 1,000 bars, alignment while visible, and blocked-move share. Compare each finalist with V0 (walls invisible) and V1 (naive), each with and without the rest outlet if V.D is adopted.
- [ ] **L.3 Pillars and rooms:** does seeing walls cut wall trapping (the obstacles §6 failure)? Measure the blocked share, catches and time since last seen.
- [ ] **L.4 Recognition probe.** In trained brains, do mouse-driven and wall-driven input take different routes? Measure:
  - the share of each channel's energy that reaches the rest outlet versus the move slots;
  - whether the move still follows the mouse slice when walls are in view.

  Tools: `--log-state`, the lesion map and `structure.q`.
- [ ] **L.5 Adopt.** The winning encoding (and the outlet, if it won) becomes the default: update the golden digest, the revival §2 and BAR_STRUCTURE.md, re-run the obstacles plan's walled-world arms on the new default, and update CLAUDE.md.

## 7. Decisions for the user

1. **Who does the recognition?**
   - (a) The cat's own wiring, from physical cues only: depth, contrast, motion, size and colour (V2–V6, V8, V9; the frog's-eye route).
   - (b) The world names each thing (V6L, kind by label).
   - (c) Both: physical cues are the candidates, and V6L is measured only as the upper-bound control.

   **Recommendation: (c).** It keeps "recognition" something the cat's own wiring does, and still tells us how much is lost by not being told. Colour (V6) is the physical cue closest to what V6L hands over, so the gap between those two measures what it costs the cat to learn its own recognition.
2. **A way to ignore input: the rest outlet (V.D)?**
   - (a) No outlet; rely on a sparse encoding.
   - (b) A rest outlet in the central brain's bottom corners: it absorbs only.
   - (c) As (b), and the cat stays still when rest wins.
   - (d) The outlet sits in the sight brain instead (filtering before the central brain).

   **Recommendation: (c).** Without some outlet, every unit the eye sends is a move. Staying still is a real hunting behaviour, and conservation is untouched. Measure it against (a) before adopting.
3. **Mouse motion every bar** (E.4)? It's needed only if a motion cue (V4) is a finalist, and it changes the default world. **Recommendation:** decide after S.1, which can measure V4 under both schedules.
4. **Retina resolution.**
   - (a) Render 32 rays but encode down to the existing 8 input cells (the sight brain unchanged).
   - (b) 16 input cells and a wider sight brain.

   **Recommendation: (a) first.** Go to (b) only if S.1 shows 8 cells are the bottleneck, i.e. a mouse and a wall edge often fall in the same slice.
5. **Channel routing** (§5: labels, a wider sight brain, or a second sight brain). **Recommendation:** decide after S.3 with the numbers. A second sense brain (motion/where beside form/what) fits the modular design best if the channels must stay apart.
6. **Binocular depth: who compares the two eyes** (V8, §5)?
   - (a) The encoder.
   - (b) The brain (left-eye and right-eye sight brains, or one sight brain with a label per eye).
   - (c) Neither: use motion parallax (V9) only, with one eye.
   - (d) Not now.

   **Recommendation: screen all three (V.B2).** Make (b) the main line if the coincidence test (V.B3) works, since it keeps depth something the cat's own wiring computes. Otherwise use (a), keeping (a) and V2 as upper bounds either way. Binocular vision needs the 32-ray render, which ties it to decision 4.
7. **A heading for the eyes** (needed by V8; V9 doesn't need it).
   - (a) The last move direction. The eyes sit across it and still see all round in compass slices.
   - (b) A fixed axis. The simplest, but blind to depth along that axis.
   - (c) A fully cat-centred view, where slices are relative to the heading and the outputs mean turn/forward. That's a large change to the brain's input and output wiring; it belongs in its own plan.

   **Recommendation: (a).**
8. **Colour (V6, §5).**
   - **How many channels?** 3, or 5 (past nature), and whether to add a heat channel.
   - **Pooling:** the strongest return per channel, or the count of rays (which also carries size).
   - **Layout and routing:**
     - (a) one wide, slice-interleaved sight brain;
     - (b) the same width, channel-major;
     - (c) one colour brain per channel;
     - (d) opponent channels (combinable with any of these).

   **Recommendation:**
   - **3 channels to start.** That's enough for C1 with mouse, wall and obstacle, and for C2 combinations. Go to 5 when the dog and landmarks need channels of their own.
   - **Screen both pooling rules.**
   - **Layout (a)** as the main line: it keeps a slice's colours adjacent, so combinations can form inside one brain.
   - **Start the ladder at C2 for results,** with C1 as the sanity check.
   - **Keep heat as a later rung:** like C1, it makes the mouse trivially identifiable.

## 8. Results

None yet.

## 9. Risks

- **The outlet becomes a do-nothing attractor.** Under the usage reinforcement, the brain could dump everything there and sit still. Watch the share of bars with no move, and compare with the starving-satiety punishment, which should push against it.
- **Contrast and motion leave the sight brain empty most of the time** (in a blank scene, or when the mouse is still). That's the same as today when the mouse is hidden, but it makes memory even more necessary.
- **Labels as colour bend the walk** (§5 notes). That's why colour (V6) uses separate cells. If V6L beats V6 only because of the bend, prefer separate cells or pathways (decisions 5c, 8).
- **A one-hot palette is a kind label by another name.** If each kind lights only its own channel (C1), colour just hands over the answer. Recognition is only tested from C2 (overlapping colours) on, so C1 counts as a sanity rung, not a result.
- **Width.** 8 slices × K colours × 2 eyes grows fast: 48 input cells for 3 colours with two eyes compared in the brain. The stacked state also needs equal widths, or a generalised stack (V.C3).
- **Renderer cost.** 32 rays × walls per bar per eye must stay small next to the brain (E.3 sets a 0.3 ms limit per eye).
- **Stereo is short-range at this resolution.** At 8 slices it reaches only about 2.4 units with b = 2 (§5). If the brain route (V8b) is kept at 8 input cells per eye, it will look like failure when it's only a resolution limit. V.B2 measures that before any learning run.
- **The obstacles plan's walled-world results** were all measured with invisible walls. Once this plan's winner is the default, those arms need re-running (L.5).

## 10. Progress Log

### 2026-11-03 — plan written
- From the user's question about the cat seeing the whole world: presence coding in an enclosed world lights every input. Two more facts make that worse than no information: every input unit becomes a move vote (energy leaves only through the move outputs), and the input row saturates at 2 units a cell.
- Proposed: an object model and a ray-cast renderer (Stage E); eight encodings from naive to attention-capped, with physical-cue recognition as the main line and kind labels as the control (Stage V); a rest outlet so the brain can ignore input; a brain-free screen before any learning runs (Stage S, cheap, because of the speed problem); and 16-seed learning tests of the finalists (Stage L).
- Nothing built. Waiting on §7.

### 2026-11-03 — architecture diagrams; binocular vision added
- Added the Architecture section: today's primary side, the reward brain, one bar, and what this plan adds (★).
- User suggestion: binocular vision, so that parallax tells near from far and gives the world contrast. Added as V8, with motion parallax (V9) as its one-eye alternative; tasks V.B1–V.B3; §7 decisions 6 (who compares the eyes) and 7 (a heading for the eyes).
- Finding from the geometry: stereo depends on resolution. With eyes 2 units apart, the two eyes see an object in different slices only within 2.4 units at 8 slices, and within 10 units at 32 rays (20 units with eyes 4 apart). So binocular vision is tied to decision 4.
- The substrate already has a candidate coincidence detector for the two eyes (a HIGH cell with two labels averages their weight rows). V.B3 tests it in the lab first.

### 2026-11-03 — colour vision defined
- User request: rays return a colour; each object kind gets its own colour; each slice feeds K input cells (3, or 5 "way past nature"), one per colour channel. Defined as V6 (§5, "Colour vision"); the old kind-label encoding is now the control V6L.
- Added a palette ladder C1–C4 (one-hot, overlapping, camouflage, variation): only C2 onwards tests recognition, since a one-hot palette is a kind label on separate cells.
- Colour separates the wall input from the mouse input but doesn't stop the wall channel flooding. Per-channel contrast and the rest outlet are still needed; colour + contrast + outlet is the expected winner.
- Found a code constraint: the stacked primary state uses one column count for every brain (`.mod.stack`, usage indices), so a wider colour sight brain needs a generalised stack (V.C3).
- New tasks V.C1–V.C4 and §7 decision 8.
