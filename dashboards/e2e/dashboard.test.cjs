// End-to-end check of the Cat Brain dashboard in Firefox. Run via scripts/e2e_dashboards.sh.
const { firefox } = require('playwright');
const assert = require('assert');

const BASE = process.env.DASH_URL || 'http://localhost:10001';
const ID = '5c0f0e5a-ca7b-4ab1-9d11-0a7b0c4a7b01';
const shot = (p, clip) => p.screenshot({ clip });

(async () => {
  const browser = await firefox.launch();
  const page = await browser.newPage({ viewport: { width: 1500, height: 900 } });
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

  await check('title is Cat Brain', async () => assert.match(await page.title(), /Cat Brain/));
  await check('energy matrix legend shows the three levels', async () => {
    const t = await page.locator('body').innerText();
    for (const k of ['l0', 'l1', 'l2']) assert.ok(t.includes(k), `missing ${k}`);
  });
  await check('field legend shows trails, mouse, cat and gaze', async () => {
    const t = await page.locator('body').innerText();
    for (const k of ['mouseTrail', 'catTrail', 'mouse', 'cat', 'gaze']) assert.ok(t.includes(k), `missing ${k}`);
  });
  await check('stats grid shows catches and brain energy', async () => {
    const t = await page.locator('body').innerText();
    for (const k of ['Catches', 'Brain energy', 'Bar']) assert.ok(t.includes(k), `missing ${k}`);
  });
  await check('no loading overlay left', async () => {
    assert.ok(!/Loading\s+\d+s/.test(await page.locator('body').innerText()));
  });
  await check('no subscriber errors', async () => assert.deepStrictEqual(errors, []));

  // The sim runs in the background (--viz-delay-ms), so every panel should change within a few seconds.
  const panels = {
    'energy matrix': { x: 10, y: 40, width: 900, height: 495 },
    'cat/mouse field': { x: 915, y: 40, width: 575, height: 495 },
    'stats grid': { x: 10, y: 540, width: 365, height: 350 },
    'catch-rate curve': { x: 740, y: 540, width: 365, height: 350 },
    'alignment curve': { x: 1110, y: 540, width: 380, height: 350 },
  };
  const before = {};
  for (const [n, c] of Object.entries(panels)) before[n] = await shot(page, c);
  await page.waitForTimeout(8000);
  for (const [n, c] of Object.entries(panels)) {
    const after = await shot(page, c);
    await check(`${n} updates live`, async () => assert.ok(!before[n].equals(after)));
  }
  // The meta-brain is idle until catches arrive, so its matrix is only checked for presence (two legends: primary and meta).
  await check('meta matrix is drawn', async () => {
    const t = await page.locator('body').innerText();
    assert.strictEqual(t.split('l0').length - 1, 2);
  });
  await page.screenshot({ path: process.env.E2E_SHOT || '/tmp/catbrain_e2e.png' });

  await browser.close();
  console.log(failed ? `${failed} check(s) failed` : 'all checks passed');
  process.exit(failed ? 1 : 0);
})();
