#!/usr/bin/env node
/**
 * Compare property/token order across Tailwind CSS layers.
 *
 * - Reads a CSS file (defaults to node_modules/tailwindcss/index.css)
 * - Extracts and orders:
 *   - theme layer: CSS custom properties (design tokens) declared under @layer theme
 *   - properties layer: @property at-rules (if present) under @layer properties
 *   - utilities layer: first-seen order of standard CSS properties (excludes --custom-props)
 * - Prints a concise report and optionally a JSON dump.
 *
 * Usage:
 *   node scripts/compare_tailwind_property_order.js [--css <path>] [--json <out.json>] [--full]
 */

const fs = require('fs');
const path = require('path');

function readFile(file) {
  try {
    return fs.readFileSync(file, 'utf8');
  } catch (e) {
    console.error(`Error reading ${file}: ${e.message}`);
    process.exit(1);
  }
}

function sliceBalancedBlock(css, startIdxOfOpenBrace) {
  if (startIdxOfOpenBrace === -1) return null;
  let depth = 0;
  for (let j = startIdxOfOpenBrace; j < css.length; j++) {
    const ch = css[j];
    if (ch === '{') depth++;
    else if (ch === '}') {
      depth--;
      if (depth === 0) {
        return css.slice(startIdxOfOpenBrace + 1, j);
      }
    }
  }
  return null;
}

function findLayerBlock(css, layerName) {
  const marker = `@layer ${layerName}`;
  let i = css.indexOf(marker);
  if (i !== -1) {
    const braceStart = css.indexOf('{', i);
    return sliceBalancedBlock(css, braceStart);
  }
  // Tailwind v4 ships theme as @theme default { ... } without @layer in theme.css
  if (layerName === 'theme') {
    const themeMarker = '@theme';
    const idx = css.indexOf(themeMarker);
    if (idx !== -1) {
      const braceStart = css.indexOf('{', idx);
      return sliceBalancedBlock(css, braceStart);
    }
  }
  return null;
}

function extractThemeTokens(themeCss) {
  if (!themeCss) return [];
  // Grab custom property declarations in order: --name: value;
  const re = /--[a-zA-Z0-9_-]+\s*:/g;
  const tokens = [];
  let m;
  while ((m = re.exec(themeCss)) !== null) {
    const raw = m[0];
    const name = raw.slice(0, raw.indexOf(':')).trim();
    tokens.push(name);
  }
  return tokens;
}

function extractPropertiesLayer(propertiesCss) {
  if (!propertiesCss) return [];
  // Collect @property names in order: @property <name> { ... }
  const re = /@property\s+([a-zA-Z0-9_-]+)/g;
  const props = [];
  let m;
  while ((m = re.exec(propertiesCss)) !== null) {
    props.push(m[1]);
  }
  return props;
}

function extractUtilityPropertyOrder(utilitiesCss) {
  if (!utilitiesCss) return [];
  const seen = new Set();
  const order = [];

  // A very simple tokenizer scanning declarations like: prop: value;
  // We skip custom props (starting with --) and at-rules content.
  // This is intentionally lightweight; good enough for comparing ordering.
  const re = /([a-zA-Z-]+)\s*:\s*[^;{}]+;/g;
  let m;
  while ((m = re.exec(utilitiesCss)) !== null) {
    const prop = m[1];
    if (prop.startsWith('--')) continue;
    if (!seen.has(prop)) {
      seen.add(prop);
      order.push(prop);
    }
  }
  return order;
}

function parseArgs(argv) {
  const args = { css: null, json: null, full: false };
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    if (a === '--css') args.css = argv[++i];
    else if (a === '--json') args.json = argv[++i];
    else if (a === '--full') args.full = true;
    else {
      console.error(`Unknown arg: ${a}`);
      process.exit(1);
    }
  }
  return args;
}

function main() {
  const args = parseArgs(process.argv);
  const defaultCss = path.join('node_modules', 'tailwindcss', 'index.css');
  const cssPath = args.css || defaultCss;
  const css = readFile(cssPath);

  const themeCss = findLayerBlock(css, 'theme');
  const propertiesCss = findLayerBlock(css, 'properties'); // may be null in Tailwind v4
  const utilitiesCss = findLayerBlock(css, 'utilities');

  const themeTokens = extractThemeTokens(themeCss);
  const propertiesAtRules = extractPropertiesLayer(propertiesCss);
  const utilityProps = extractUtilityPropertyOrder(utilitiesCss);

  const report = {
    source: path.resolve(cssPath),
    layers: {
      theme: { count: themeTokens.length, tokens: themeTokens },
      properties: { count: propertiesAtRules.length, atRules: propertiesAtRules },
      utilities: { count: utilityProps.length, properties: utilityProps },
    },
  };

  // Pretty print concise summary
  console.log(`Source CSS: ${report.source}`);
  console.log('');

  // Theme
  console.log('Layer: theme');
  console.log(`- tokens: ${themeTokens.length}`);
  if (args.full) {
    console.log(themeTokens.join('\n'));
  } else {
    console.log(
      `- first 10: ${themeTokens.slice(0, 10).join(', ')}`
    );
    console.log(
      `- last 5: ${themeTokens.slice(-5).join(', ')}`
    );
  }
  console.log('');

  // Properties layer
  console.log('Layer: properties');
  if (!propertiesCss) {
    console.log('- not present in this CSS (Tailwind v4 package does not emit a distinct properties layer)');
  } else {
    console.log(`- @property at-rules: ${propertiesAtRules.length}`);
    if (args.full) console.log(propertiesAtRules.join('\n'));
  }
  console.log('');

  // Utilities
  console.log('Layer: utilities');
  console.log(`- distinct CSS properties (first-seen order): ${utilityProps.length}`);
  if (args.full) {
    console.log(utilityProps.join('\n'));
  } else {
    console.log(`- first 20: ${utilityProps.slice(0, 20).join(', ')}`);
  }
  console.log('');

  // Simple comparison hint
  console.log('Comparison:');
  if (propertiesAtRules.length === 0) {
    console.log('- properties layer: N/A');
  } else {
    console.log(`- properties layer has ${propertiesAtRules.length} @property rules`);
  }
  console.log(`- theme token count vs utilities properties: ${themeTokens.length} tokens vs ${utilityProps.length} properties`);

  if (args.json) {
    fs.writeFileSync(args.json, JSON.stringify(report, null, 2));
    console.log(`\nWrote JSON report to ${path.resolve(args.json)}`);
  }
}

main();
