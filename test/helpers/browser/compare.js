// Differential rendering: the same elements under tw's sheet and Tailwind's,
// compared on what the browser computes. A property that computes the same is
// equivalent however the two sheets spell it; one that differs is observable.
// Exit codes: 0 no differences, 1 differences on stdout, 2 no usable browser.
const fs = require('fs');

let chromium;
try {
  ({ chromium } = require('playwright'));
} catch (e) {
  console.error(e.message);
  process.exit(2);
}

const [, , elementsFile, twCss, tailwindCss] = process.argv;
const elements = fs.readFileSync(elementsFile, 'utf8').split('\n').filter((l) => l.trim());

// Custom properties are absent from getComputedStyle's enumeration, so collect
// the names the sheets declare and ask for each by hand.
const customNames = (css) => new Set(css.match(/--[A-Za-z0-9_-]+/g) || []);

const page = (css, classes) =>
  `<!doctype html><meta charset="utf-8"><style>${css}</style>` +
  `<body>${classes.map((c, i) => `<div id="e${i}" class="${c}"></div>`).join('')}</body>`;

async function computed(browser, css, names) {
  const p = await browser.newPage({ viewport: { width: 1280, height: 800 } });
  await p.setContent(page(css, elements));
  const out = await p.evaluate((ns) => {
    const res = {};
    document.querySelectorAll('div[id^=e]').forEach((el) => {
      const cs = getComputedStyle(el);
      const o = {};
      for (const prop of cs) o[prop] = cs.getPropertyValue(prop);
      for (const n of ns) {
        const v = cs.getPropertyValue(n);
        if (v !== '') o[n] = v;
      }
      res[el.id] = o;
    });
    return res;
  }, names);
  await p.close();
  return out;
}

(async () => {
  const a = fs.readFileSync(twCss, 'utf8');
  const b = fs.readFileSync(tailwindCss, 'utf8');
  const names = [...new Set([...customNames(a), ...customNames(b)])];
  let browser;
  try {
    browser = await chromium.launch();
  } catch (e) {
    console.error(e.message);
    process.exit(2);
  }
  const ta = await computed(browser, a, names);
  const tb = await computed(browser, b, names);
  await browser.close();

  // A custom property is a token stream: the browser hands back the author's
  // own whitespace and quoting, so two sheets that minify differently differ on
  // spelling alone. Compare them with both dropped - whatever reads the
  // variable shows a real difference in its own property.
  const norm = (k, v) =>
    k.startsWith('--') && v !== undefined ? v.replace(/["'\s]+/g, '') : v;

  const lines = [];
  for (const [id, pa] of Object.entries(ta)) {
    const pb = tb[id];
    for (const k of new Set([...Object.keys(pa), ...Object.keys(pb)])) {
      // A custom property one sheet declares and the other prunes renders the
      // same unless something reads it, and whatever reads it differs instead.
      if (k.startsWith('--') && (pa[k] === undefined || pb[k] === undefined)) continue;
      if (norm(k, pa[k]) !== norm(k, pb[k]))
        lines.push(`${elements[Number(id.slice(1))]}: ${k}: ${pa[k]} (tw) vs ${pb[k]} (tailwind)`);
    }
  }
  if (lines.length) {
    console.log(lines.join('\n'));
    process.exit(1);
  }
  process.exit(0);
})();
