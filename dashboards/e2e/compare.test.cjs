// End-to-end check of the Cat Brain Compare dashboard (two replayed runs side by side, with route
// maps) in Firefox. Run via dashboards/e2e/run_e2e.sh compare.
const { firefox } = require('playwright');
const assert = require('assert');

const BASE = process.env.DASH_URL || 'http://localhost:10001';
const ID = '5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b02';
const shot = (p, clip) => p.screenshot({ clip });
const count = (t, k) => t.split(k).length - 1;

(async () => {
  const browser = await firefox.launch();
  const page = await browser.newPage({ viewport: { width: 1500, height: 1000 } });
  const errors = [];
  page.on('console', async (m) => {
    const t = m.text();
    if (m.type() === 'error' && !t.includes('downloadable font')) errors.push(t.slice(0, 200));
  });
  let failed = 0;
  const check = async (name, fn) => {
    try { await fn(); console.log('ok   ', name); }
    catch (e) { failed++; console.log('FAIL ', name, '-', e.message.split('\n')[0]); }
  };

  await page.goto(`${BASE}/#/${ID}`);
  await page.waitForTimeout(5000);
  const text = async () => page.locator('body').innerText();

  await check('title is Cat Brain Compare', async () => assert.match(await page.title(), /Cat Brain Compare/));
  // Panel labels ("Run A: route sharpness ...") are axis titles drawn on the canvas, so they aren't
  // page text; check them on the saved screenshot (E2E_SHOT) by eye.
  await check('two route maps with all four legend entries', async () => {
    const t = await text();
    for (const k of ['r0', 'r1', 'r2', 'dir']) assert.ok(count(t, k) >= 2, `${k} appears ${count(t, k)} times`);
  });
  await check('two fields with trails, mouse, cat and gaze', async () => {
    const t = await text();
    for (const k of ['mouseTrail', 'catTrail', 'gaze']) assert.ok(count(t, k) >= 2, `${k} appears ${count(t, k)} times`);
  });
  await check('no loading overlay left', async () => assert.ok(!/Loading\s+\d+s/.test(await text())));
  await check('no subscriber errors', async () => assert.deepStrictEqual(errors, []));

  // Both replays advance together, so both sides' fields and curves should change within a few seconds
  const panels = {
    'run A field': { x: 10, y: 460, width: 365, height: 480 },
    'run A catch rate': { x: 385, y: 460, width: 365, height: 240 },
    'run B field': { x: 759, y: 460, width: 365, height: 480 },
    'run B catch rate': { x: 1134, y: 460, width: 355, height: 240 },
  };
  const before = {};
  for (const [n, c] of Object.entries(panels)) before[n] = await shot(page, c);
  await page.waitForTimeout(8000);
  for (const [n, c] of Object.entries(panels)) {
    const after = await shot(page, c);
    await check(`${n} updates during replay`, async () => assert.ok(!before[n].equals(after)));
  }
  await page.screenshot({ path: process.env.E2E_SHOT || '/tmp/catbrain_compare_e2e.png' });
  await browser.close();
  console.log(failed ? `${failed} check(s) failed` : 'all checks passed');
  process.exit(failed ? 1 : 0);
})();
