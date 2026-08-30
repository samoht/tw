// Differential rendering: the same elements under tw's sheet and Tailwind's,
// compared on what the browser computes. A property that computes the same is
// equivalent however the two sheets spell it; one that differs is observable.
// Exit codes: 0 no differences, 1 differences on stdout, 2 no usable browser,
// 3 the page did not carry the classes asked for, reason on stdout.
const fs = require('fs');

let chromium;
try {
  ({ chromium } = require('playwright'));
} catch (e) {
  console.error(e.message);
  process.exit(2);
}

const [, , elementsFile, twCss, tailwindCss] = process.argv;

// An entry is a class list, optionally followed by a tab and the markup to put
// inside the element carrying it. A bare element only exercises the rules that
// match the class itself; a plugin like @tailwindcss/typography spends most of
// its sheet on descendants, which fire on real children or not at all.
const entries = fs
  .readFileSync(elementsFile, 'utf8')
  .split('\n')
  .filter((l) => l.trim())
  .map((l) => {
    const tab = l.indexOf('\t');
    return tab < 0
      ? { classes: l.trim(), inner: '' }
      : { classes: l.slice(0, tab).trim(), inner: l.slice(tab + 1) };
  });

// Custom properties are absent from getComputedStyle's enumeration, so collect
// the names the sheets declare and ask for each by hand.
const customNames = (css) => new Set(css.match(/--[A-Za-z0-9_-]+/g) || []);

// Arbitrary values carry quotes and angle brackets of their own
// (content-["x"], bg-[url("/img/x.png")]), so the class must be escaped as
// attribute text or it truncates the attribute it sits in.
const attr = (s) =>
  s
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');

const page = (css) =>
  `<!doctype html><meta charset="utf-8"><style>${css}</style><body>` +
  entries
    .map((e, i) => `<div id="e${i}" class="${attr(e.classes)}">${e.inner}</div>`)
    .join('') +
  `</body>`;

// What each element is meant to carry, whitespace normalised the way classList
// reports it.
const wanted = entries.map((e) => e.classes.split(/\s+/).join(' '));

// An element that lost part of its class attribute is styled by neither sheet,
// and the two then agree on the same bare element - a pass that says nothing.
// Report the first element whose classes are not the ones asked for.
const unusable = (got) => {
  if (got.length !== wanted.length)
    return `built ${got.length} elements for ${wanted.length} entries`;
  for (let i = 0; i < wanted.length; i++)
    if (got[i] !== wanted[i])
      return `element e${i} carries [${got[i]}], expected [${wanted[i]}]`;
  return null;
};

// The states a variant can put a rule behind that a still page never enters.
// Forced through CDP rather than by moving a mouse: one pass sets the state on
// every element at once, it reaches :focus without the element being focusable
// for real, and it is what DevTools' own "force element state" does. A state
// the protocol will not force is simply absent from this list rather than
// silently mis-measured.
// Measured one by one against a rule the state alone selects, because the
// protocol accepts a name whether or not the browser then honours it:
// [visited] is accepted and changes nothing (a privacy restriction), so it is
// left out rather than sitting here reporting agreement it never tested.
const forcedStates = [
  'hover',
  'focus',
  'focus-visible',
  'focus-within',
  'active',
  'checked',
  'disabled',
];

// Force [state] on every recorded element, so a rule behind hover: or focus:
// is one the browser has actually applied rather than one that merely sits in
// the sheet. Without this the state half of the variant set is in both sheets
// and matched by neither, and the two agree for the wrong reason.
async function force(p, state) {
  const cdp = await p.context().newCDPSession(p);
  await cdp.send('DOM.enable');
  await cdp.send('CSS.enable');
  const { root } = await cdp.send('DOM.getDocument');
  const { nodeIds } = await cdp.send('DOM.querySelectorAll', {
    nodeId: root.nodeId,
    selector: 'body > div[id^=e], body > div[id^=e] *',
  });
  for (const nodeId of nodeIds)
    await cdp.send('CSS.forcePseudoState', { nodeId, forcedPseudoClasses: [state] });
}

async function computed(browser, css, names, state) {
  const p = await browser.newPage({ viewport: { width: 1280, height: 800 } });
  await p.setContent(page(css));
  if (state) await force(p, state);
  const out = await p.evaluate((ns) => {
    // Where a descendant sits inside its element, as tag names with an index
    // wherever siblings share a tag: enough to find it again in the markup.
    const path = (el, root) => {
      const parts = [];
      for (let n = el; n && n !== root; n = n.parentElement) {
        const tag = n.tagName.toLowerCase();
        const sibs = [...n.parentElement.children].filter(
          (c) => c.tagName === n.tagName
        );
        parts.unshift(sibs.length > 1 ? `${tag}(${sibs.indexOf(n) + 1})` : tag);
      }
      return parts.join(' > ');
    };
    const style = (el, pseudo) => {
      const cs = getComputedStyle(el, pseudo || null);
      const o = {};
      for (const prop of cs) o[prop] = cs.getPropertyValue(prop);
      for (const n of ns) {
        const v = cs.getPropertyValue(n);
        if (v !== '') o[n] = v;
      }
      return o;
    };
    // The element itself and the pseudo-elements a sheet can reach without the
    // page being interacted with. Prose puts its list bullets on ::marker and
    // the before:/after: variants exist to write ::before and ::after, none of
    // which the element's own computed style shows.
    const pseudos = ['', '::before', '::after', '::marker'];
    const res = { nodes: [], classes: [] };
    const record = (id, el, p) => {
      for (const ps of pseudos)
        res.nodes.push({ id, path: p + ps, style: style(el, ps) });
    };
    document.querySelectorAll('body > div[id^=e]').forEach((el) => {
      res.classes.push([...el.classList].join(' '));
      record(el.id, el, '');
      el.querySelectorAll('*').forEach((d) => record(el.id, d, path(d, el)));
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
  // One pass with the page as it loads, then one per forced state. A rule
  // behind hover: or focus: is in both sheets and matched by neither until the
  // browser is actually in that state, so the still page agrees for the wrong
  // reason.
  const passes = [];
  for (const state of [null, ...forcedStates])
    passes.push({
      state,
      ra: await computed(browser, a, names, state),
      rb: await computed(browser, b, names, state),
    });
  await browser.close();

  const lines = [];
  for (const { state, ra, rb } of passes) {
    const where = state ? ` [:${state}]` : '';
    compare(ra, rb, where, lines);
  }
  report(lines);
})();

// Pair the two node lists and record every property that differs.
function compare(ra, rb, where, lines) {
  const broken = unusable(ra.classes) || unusable(rb.classes);
  if (broken) {
    console.log(`markup does not carry the intended classes: ${broken}`);
    process.exit(3);
  }
  // Both pages are built from the same entries, so the two node lists are the
  // same tree walked in the same order. A mismatch means one page parsed the
  // markup differently, and pairing them up by index would compare unrelated
  // elements.
  if (ra.nodes.length !== rb.nodes.length) {
    console.log(
      `markup parsed to ${ra.nodes.length} nodes under tw and ` +
        `${rb.nodes.length} under tailwind`
    );
    process.exit(3);
  }

  const label = (n) => {
    const cls = entries[Number(n.id.slice(1))].classes;
    const base = !n.path
      ? cls
      : n.path.startsWith('::')
        ? `${cls} ${n.path}`
        : `${cls} :: ${n.path}`;
    return base + where;
  };

  // A custom property is a token stream: the browser hands back the author's
  // own whitespace and quoting, so two sheets that minify differently differ on
  // spelling alone. Compare them with both dropped - whatever reads the
  // variable shows a real difference in its own property.
  const norm = (k, v) =>
    k.startsWith('--') && v !== undefined ? v.replace(/["'\s]+/g, '') : v;

  for (let i = 0; i < ra.nodes.length; i++) {
    const na = ra.nodes[i];
    const nb = rb.nodes[i];
    if (na.id !== nb.id || na.path !== nb.path) {
      console.log(
        `markup walked differently: [${label(na)}] under tw, ` +
          `[${label(nb)}] under tailwind`
      );
      process.exit(3);
    }
    const pa = na.style;
    const pb = nb.style;
    for (const k of new Set([...Object.keys(pa), ...Object.keys(pb)])) {
      // A custom property one sheet declares and the other prunes renders the
      // same unless something reads it, and whatever reads it differs instead.
      if (k.startsWith('--') && (pa[k] === undefined || pb[k] === undefined)) continue;
      if (norm(k, pa[k]) !== norm(k, pb[k]))
        lines.push(`${label(na)}: ${k}: ${pa[k]} (tw) vs ${pb[k]} (tailwind)`);
    }
  }
}

function report(lines) {
  if (lines.length) {
    // Every difference fails the run; a whole descendant tree off by one rule
    // prints thousands of lines, so the report stops at a readable prefix.
    const shown = lines.slice(0, 200);
    console.log(shown.join('\n'));
    if (lines.length > shown.length)
      console.log(`... and ${lines.length - shown.length} more differences`);
    process.exit(1);
  }
  process.exit(0);
}
