#!/usr/bin/env node
// specimen-site — typeset any PureScript package as a single-page specimen book.
//
//   node cli/specimen-site.mjs <package-name | workspace-dir> [options]
//
//   --out, -o <dir>     output directory (default: ./site/<package>)
//   --title <string>    book title (default: package name)
//   --deck <string>     book deck line (default: computed)
//   --mark <glyph>      formal mark at the waxseal's foot (default: λ)
//   --include-tests     include test/ modules from a local workspace
//
// A registry name (e.g. `aff`) is resolved by `spago fetch` in a throwaway
// workspace; a directory is scanned for its packages' src trees. Every module
// is rendered at build time through Specimen's pure pipeline (glyphify →
// extractBlocks → renderDocument) — the output is a static site, no runtime
// PureScript, servable from file:// or any static host.
//
// Identity kit, all derived from the package itself:
//   · hero banner: recursive Minard beeswarm (module = circle-pack of its
//     declarations, swarmed along import-graph layers) that morphs on scroll
//     into the left-margin bubble nav
//   · waxseal: B-ink circle-pack seal of the namespace tree, in the colophon
//     and as the favicon
//   · computed colophon: modules / declarations / lines / version / date

import { readFileSync, readdirSync, statSync, writeFileSync, mkdirSync, existsSync, cpSync } from 'node:fs';
import { join, dirname, resolve, basename } from 'node:path';
import { fileURLToPath } from 'node:url';
import { execSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { hierarchy, pack } from 'd3-hierarchy';
import { forceSimulation, forceX, forceY, forceCollide } from 'd3-force';

const HERE = dirname(fileURLToPath(import.meta.url));
const SPECIMEN = resolve(HERE, '..');
const OUTPUT = join(SPECIMEN, 'output');
const PRELUDE_BOOK = resolve(SPECIMEN, '..', 'the-prelude', 'public');

const { glyphify } = await import(OUTPUT + '/Specimen.Preprocess/index.js');
const { extractBlocks } = await import(OUTPUT + '/Specimen.Block/index.js');
const { renderDocument } = await import(OUTPUT + '/Specimen.Render/index.js');
const M = await import(OUTPUT + '/Data.Map.Internal/index.js');

// ── args ──────────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
if (argv.length === 0 || argv[0] === '--help') {
  console.error('usage: specimen-site <package-name | workspace-dir> [-o dir] [--title T] [--deck D] [--mark G] [--include-tests]');
  process.exit(argv.length === 0 ? 1 : 0);
}
const target = argv[0];
const opt = (name, short) => {
  const i = argv.findIndex(a => a === name || (short && a === short));
  return i >= 0 ? argv[i + 1] : undefined;
};
const flag = name => argv.includes(name);
const includeTests = flag('--include-tests');
const markGlyph = opt('--mark') ?? 'λ';

// ── resolve sources ───────────────────────────────────────────────────────
const findPurs = (dir, acc = []) => {
  for (const e of readdirSync(dir)) {
    const p = join(dir, e);
    if (statSync(p).isDirectory()) {
      if (['.spago', 'output', 'node_modules', '.git'].includes(e)) continue;
      if (!includeTests && e === 'test') continue;
      findPurs(p, acc);
    } else if (e.endsWith('.purs')) acc.push(p);
  }
  return acc;
};

function resolveTarget(t) {
  if (existsSync(t) && statSync(t).isDirectory()) {
    const dir = resolve(t);
    return { name: basename(dir), version: 'local', files: findPurs(dir), local: true };
  }
  if (!/^[a-z][a-z0-9-]*$/.test(t)) {
    console.error(`"${t}" is neither a directory nor a plausible registry package name`);
    process.exit(1);
  }
  // Throwaway workspace; spago fetch vendors the sources under .spago/p/.
  const ws = join(tmpdir(), 'specimen-site-fetch', t);
  const pkgDir = join(ws, '.spago', 'p');
  if (!existsSync(pkgDir) || !readdirSync(pkgDir).some(d => d.startsWith(t + '-'))) {
    mkdirSync(join(ws, 'src'), { recursive: true });
    writeFileSync(join(ws, 'spago.yaml'),
      `workspace: {}\npackage:\n  name: specimen-site-fetch\n  dependencies:\n    - ${t}\n`);
    writeFileSync(join(ws, 'src', 'Main.purs'), 'module Main where\n');
    console.error(`fetching ${t} from the registry…`);
    execSync('spago fetch --quiet', { cwd: ws, stdio: ['ignore', 'inherit', 'inherit'] });
  }
  const vendored = readdirSync(pkgDir).find(d => d.startsWith(t + '-') &&
    /^\d/.test(d.slice(t.length + 1)));
  if (!vendored) { console.error(`spago fetch ran but ${t} not found under ${pkgDir}`); process.exit(1); }
  const version = vendored.slice(t.length + 1);
  return { name: t, version, files: findPurs(join(pkgDir, vendored, 'src')), local: false };
}

const pkg = resolveTarget(target);
if (pkg.files.length === 0) { console.error('no .purs files found'); process.exit(1); }

const title = opt('--title') ?? pkg.name;
const outDir = resolve(opt('--out', '-o') ?? join('site', pkg.name));

// ── harvest ───────────────────────────────────────────────────────────────
const mods = pkg.files.map(p => {
  const src = readFileSync(p, 'utf8');
  const lines = src.split('\n');
  const nameM = src.match(/^module\s+([\w.]+)/m);
  if (!nameM) return null;
  const name = nameM[1];
  // top-level anchors for the mini-packs: col-0 code lines, sig+equations merged
  const anchors = [];
  lines.forEach((l, i) => {
    if (/^[a-zA-Z(]/.test(l) && !/^(module|import)\b/.test(l)) {
      const id = (l.match(/^\(?([\w']+)/) || [, l.slice(0, 8)])[1];
      anchors.push({ id, line: i });
    }
  });
  const decls = [];
  for (const a of anchors) {
    const last = decls[decls.length - 1];
    if (last && last.id === a.id) continue;
    decls.push({ id: a.id, line: a.line });
  }
  decls.forEach((d, i) => {
    const end = i + 1 < decls.length ? decls[i + 1].line : lines.length;
    d.span = Math.max(1, end - d.line);
  });
  const imports = [...src.matchAll(/^import\s+([\w.]+)/gm)].map(m => m[1]);
  const loc = lines.filter(l => l.trim() !== '').length;
  return { name, src, loc, decls, imports };
}).filter(Boolean);

const known = new Set(mods.map(m => m.name));
// in-scope edges only; edges into a bare re-export module named `Prelude`
// are dropped (they invert the layering — the umbrella belongs at the top)
mods.forEach(m => {
  m.imports = m.imports.filter(i => known.has(i) && i !== m.name && i !== 'Prelude');
});

// ── topological layers (longest path from the leaves) ────────────────────
const byName = new Map(mods.map(m => [m.name, m]));
const level = new Map();
const depth = m => {
  if (level.has(m.name)) return level.get(m.name);
  level.set(m.name, 0); // cycle guard
  const d = m.imports.length ? 1 + Math.max(...m.imports.map(i => depth(byName.get(i)))) : 0;
  level.set(m.name, d);
  return d;
};
mods.forEach(depth);
// umbrella module named exactly like a re-export target sits last anyway via layers
const maxLevel = Math.max(...level.values());
mods.forEach(m => { m.level = level.get(m.name); });
mods.sort((a, b) => a.level - b.level || a.name.localeCompare(b.name));

// accents: hue spread 0–300°, same grammar as the-prelude
const accentFor = i => `hsl(${mods.length <= 1 ? 0 : Math.round(i * 300 / (mods.length - 1))}, 58%, 43%)`;
mods.forEach((m, i) => { m.accent = accentFor(i); });

// ── mini-packs + two layouts ──────────────────────────────────────────────
const KA = mods.length < 12 ? 5.0 : 3.4;   // small books get bigger plates
for (const m of mods) {
  m.rA = Math.max(6, KA * Math.sqrt(m.loc));
  m.rB = Math.max(3.5, 1.15 * Math.sqrt(m.loc));
  const h = hierarchy({ children: m.decls }).sum(d => d.span ?? 0).sort((a, b) => b.value - a.value);
  pack().size([2 * m.rA, 2 * m.rA]).padding(1.4)(h);
  m.pack = h.leaves().map(l => ({
    dx: +(l.x - m.rA).toFixed(1), dy: +(l.y - m.rA).toFixed(1), r: +l.r.toFixed(1) }));
}

// layout A: horizontal banner, normalised 0..1
{
  // small books don't get the full spread — clamp the band and centre it,
  // so a 3-module package reads as a group, not three lonely islands
  const AW = 1600, AH = 560;
  const span = Math.min(AW - 180, Math.max(420, maxLevel * 300, Math.sqrt(mods.length) * 340));
  const AM = (AW - span) / 2, ADX = maxLevel ? span / maxLevel : 0;
  const nodes = mods.map(m => ({ m, x: AM + m.level * ADX, y: AH / 2 + Math.sin(m.name.length * 7.3) * 150 }));
  forceSimulation(nodes)
    .force('x', forceX(n => AM + n.m.level * ADX).strength(0.7))
    .force('y', forceY(AH / 2).strength(0.055))
    .force('c', forceCollide(n => n.m.rA + 7).iterations(4))
    .stop().tick(600);
  for (const n of nodes) { n.m.ax = +(n.x / AW).toFixed(4); n.m.ay = +(n.y / AH).toFixed(4); }
}

// layout B: vertical rail, px within a 176px (11rem) column
{
  const BW = 176, BH = 900, BM = 40, BDY = maxLevel ? (BH - 2 * BM) / maxLevel : 0;
  const nodes = mods.map(m => ({ m, x: BW / 2 + Math.sin(m.name.length * 3.1) * 30, y: BM + m.level * BDY }));
  forceSimulation(nodes)
    .force('y', forceY(n => BM + n.m.level * BDY).strength(0.85))
    .force('x', forceX(BW / 2).strength(0.08))
    .force('c', forceCollide(n => n.m.rB + 2.5).iterations(4))
    .stop().tick(600);
  for (const n of nodes) { n.m.bx = +n.x.toFixed(1); n.m.by = +n.y.toFixed(1); }
}

// ── render articles ───────────────────────────────────────────────────────
const slug = s => s.replace(/\./g, '-');
const sourceLabel = pkg.version === 'local' ? pkg.name : `${pkg.name} v${pkg.version}`;
let braw = 0, declTotal = 0, locTotal = 0;
const articles = mods.map(m => {
  const blocks = extractBlocks(glyphify(m.src));
  for (const b of blocks) {
    const tag = b.constructor?.name ?? '';
    if (tag === 'BRaw') braw++;
    if (['BValue', 'BData', 'BClass', 'BInstance', 'BForeign', 'BTypeAlias'].includes(tag)) declTotal++;
  }
  locTotal += m.loc;
  const article = renderDocument({ moduleSlug: m.name, source: sourceLabel, blocks, notes: M.empty });
  return `<div class="book-module" id="mod-${slug(m.name)}" data-module="${m.name}" style="--accent: ${m.accent}">${article}</div>`;
});

// ── waxseal (B-ink) ───────────────────────────────────────────────────────
function waxseal() {
  const root = { name: pkg.name, children: [] };
  for (const m of mods) {
    let node = root;
    for (const part of m.name.split('.')) {
      let child = (node.children ??= []).find(c => c.name === part);
      if (!child) { child = { name: part }; node.children.push(child); }
      node = child;
    }
    node.loc = m.loc;
  }
  // a module that is also a namespace parent (Effect.Aff with Effect.Aff.*
  // children) must contribute a drawable leaf, not just weight on a container
  const hoist = node => {
    if (node.children) {
      if (node.loc != null) {
        node.children.push({ name: node.name + ' (self)', loc: node.loc });
        delete node.loc;
      }
      node.children.forEach(hoist);
    }
  };
  hoist(root);
  const R = 240, PACK_R = 190;
  const h = hierarchy(root).sum(d => d.loc ?? 0).sort((a, b) => b.value - a.value);
  pack().size([PACK_R * 2, PACK_R * 2]).padding(5)(h);
  const off = R - PACK_R;
  let body = '';
  for (const n of h.descendants()) {
    if (n.depth === 0) continue;
    body += n.children
      ? `<circle cx="${n.x + off}" cy="${n.y + off}" r="${n.r}" fill="none" stroke="#111" stroke-width="1.3"/>`
      : `<circle cx="${n.x + off}" cy="${n.y + off}" r="${n.r}" fill="#111"/>`;
  }
  const inscription = `PURESCRIPT &#183; ${pkg.name.toUpperCase().replace(/-/g, ' ')}`;
  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${2 * R} ${2 * R}" width="${2 * R}" height="${2 * R}">
  <rect width="100%" height="100%" fill="#fff"/>
  <circle cx="${R}" cy="${R}" r="${R - 6}" fill="none" stroke="#111" stroke-width="3"/>
  <circle cx="${R}" cy="${R}" r="${R - 46}" fill="none" stroke="#111" stroke-width="1.2"/>
  <defs><path id="rim" d="M ${R - (R - 25)} ${R} a ${R - 25} ${R - 25} 0 1 1 ${2 * (R - 25)} 0"/></defs>
  <text font-family="Inter, sans-serif" font-size="17" font-weight="600" letter-spacing="7" fill="#111">
    <textPath href="#rim" startOffset="50%" text-anchor="middle">${inscription}</textPath>
  </text>
  <text x="${R}" y="${2 * R - 22}" font-family="Georgia, serif" font-size="30" text-anchor="middle" fill="#111">${markGlyph}</text>
  <g transform="translate(0,6)">${body}</g>
</svg>`;
}
const sealSvg = waxseal();

// ── payload for the morph ─────────────────────────────────────────────────
const data = mods.map(m => ({
  name: m.name, slug: slug(m.name), level: m.level, loc: m.loc, accent: m.accent,
  imports: m.imports.map(slug),
  ax: m.ax, ay: m.ay, bx: m.bx, by: m.by, rA: +m.rA.toFixed(1), rB: +m.rB.toFixed(1),
  pack: m.pack,
}));

// ── assemble page ─────────────────────────────────────────────────────────
const today = new Date().toISOString().slice(0, 10);
const deck = opt('--deck') ??
  `${mods.length} modules &middot; ${declTotal} declarations &middot; import order left to right &middot; the swarm is the nav`;
const faviconUri = 'data:image/svg+xml;base64,' + Buffer.from(sealSvg).toString('base64');

const page = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>${title} &mdash; a specimen book</title>
<link rel="icon" href="${faviconUri}">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500;600&display=swap" rel="stylesheet">
<link rel="stylesheet" href="sigil.css">
<link rel="stylesheet" href="style.css">
<style>
  /* morph banner-nav layer (specimen-site) */
  #stage { position: fixed; inset: 0; width: 100vw; height: 100vh; overflow: visible; z-index: 5; pointer-events: none; }
  #stage a { pointer-events: auto; cursor: pointer; }
  #stage circle.ring { transition: stroke-width .2s ease; }
  #hero-title { position: fixed; top: 34px; left: 60px; z-index: 6; pointer-events: none; }
  #hero-title .book-kicker { margin-bottom: 0.5rem; }
  #hero-title .book-title { font-size: 1.9rem; margin: 0 0 0.5rem; }
  #hero-title .book-deck { font-size: 0.68rem; text-transform: uppercase; letter-spacing: 0.16em; color: var(--margin); max-width: none; }
  main#book { padding-top: 74vh; }
  .book-colophon { margin: 8rem 0 4rem; text-align: center; }
  .book-colophon svg { width: 300px; height: 300px; }
  .book-colophon .facts { margin-top: 1.4rem; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.22em; color: var(--margin); line-height: 2; }
  @media (max-width: 1320px) { #stage.railed { opacity: 0; pointer-events: none; } }
  #stage { transition: opacity .3s ease; }
</style>
</head>
<body>

<header id="hero-title" class="book-head">
  <div class="book-kicker">PureScript &middot; specimen book</div>
  <h1 class="book-title">${title}</h1>
  <p class="book-deck">${deck}</p>
</header>

<svg id="stage"></svg>

<main id="book">
  <div class="book-body">
${articles.join('\n')}
  </div>
  <footer class="book-colophon">
    ${sealSvg.replace('<rect width="100%" height="100%" fill="#fff"/>', '')}
    <div class="facts">
      ${sourceLabel}<br>
      ${mods.length} modules &middot; ${declTotal} declarations &middot; ${locTotal} lines<br>
      typeset by specimen &middot; ${today}
    </div>
  </footer>
</main>

<script>
const DATA = ${JSON.stringify(data)};
const svg = document.getElementById('stage');
const NS = 'http://www.w3.org/2000/svg';
const MORPH = 700;
const ease = t => t < .5 ? 2*t*t : 1 - Math.pow(-2*t + 2, 2) / 2;

const edgeG = document.createElementNS(NS, 'g'); svg.appendChild(edgeG);
const edges = [];
for (const d of DATA) for (const i of d.imports) {
  const e = document.createElementNS(NS, 'path');
  e.setAttribute('fill', 'none'); e.setAttribute('stroke', '#111');
  e.setAttribute('stroke-width', '0.6');
  edgeG.appendChild(e); edges.push({ e, a: d.slug, b: i });
}
const gs = {};
for (const d of DATA) {
  const a = document.createElementNS(NS, 'a');
  a.setAttribute('href', '#mod-' + d.slug);
  const g = document.createElementNS(NS, 'g');
  const bubble = document.createElementNS(NS, 'circle');   // rail identity (accent)
  bubble.setAttribute('fill', d.accent); bubble.setAttribute('opacity', '0');
  g.appendChild(bubble);
  const ring = document.createElementNS(NS, 'circle');     // banner identity (ink plate)
  ring.setAttribute('class', 'ring');
  ring.setAttribute('fill', '#fff'); ring.setAttribute('stroke', '#111');
  ring.setAttribute('stroke-width', '1.4'); g.appendChild(ring);
  const inner = document.createElementNS(NS, 'g');
  for (const p of d.pack) {
    const c = document.createElementNS(NS, 'circle');
    c.setAttribute('cx', p.dx); c.setAttribute('cy', p.dy); c.setAttribute('r', p.r);
    c.setAttribute('fill', '#111'); inner.appendChild(c);
  }
  g.appendChild(inner);
  const t = document.createElementNS(NS, 'title'); t.textContent = d.name; g.appendChild(t);
  a.appendChild(g); svg.appendChild(a);
  gs[d.slug] = { g, bubble, ring, inner, d };
}

let active = null;
function layout() {
  const W = innerWidth, H = innerHeight;
  const p = ease(Math.min(1, Math.max(0, scrollY / MORPH)));
  svg.classList.toggle('railed', p > 0.95);
  const bannerH = H * 0.66;
  document.getElementById('hero-title').style.opacity = String(Math.max(0, 1 - p * 1.4));
  // rail column: centred in the left margin, same slot as the-prelude's beeswarm
  const railLeft = Math.max(12, 0.5 * W - 36 * 16 - 12 * 16);
  for (const s of Object.values(gs)) {
    const d = s.d;
    const axp = d.ax * W, ayp = 90 + d.ay * (bannerH - 90);
    const bxp = railLeft + d.bx, byp = 60 + (d.by / 900) * (H - 120);
    const x = axp + (bxp - axp) * p, y = ayp + (byp - ayp) * p;
    const r = d.rA + (d.rB - d.rA) * p;
    s.g.setAttribute('transform', 'translate(' + x + ',' + y + ')');
    s.ring.setAttribute('r', r);
    s.inner.setAttribute('transform', 'scale(' + (r / d.rA) + ')');
    s.inner.setAttribute('opacity', String(Math.max(0, 1 - p * 1.7)));
    s.bubble.setAttribute('r', r);
    s.x = x; s.y = y; s.r = r;
    const isActive = s.d.slug === active;
    // rail state: coloured bubbles, muted except the active one (the-prelude grammar)
    const railOn = p > 0.5;
    s.bubble.setAttribute('opacity', String(railOn ? (isActive || p < 0.95 ? p : 0.35) : 0));
    s.ring.setAttribute('stroke-width', isActive ? '2.6' : '1.4');
    s.ring.setAttribute('fill', railOn ? 'none' : '#fff');
    s.ring.setAttribute('opacity', String(railOn && !isActive ? 0.35 : 1));
  }
  for (const { e, a, b } of edges) {
    const A = gs[a], B = gs[b];
    e.setAttribute('opacity', String(0.10 * (1 - p) + 0.05 * p));
    e.setAttribute('d', 'M ' + A.x + ' ' + A.y + ' C ' + (A.x + B.x) / 2 + ' ' + A.y + ', ' + (A.x + B.x) / 2 + ' ' + B.y + ', ' + B.x + ' ' + B.y);
  }
}

function spy() {
  let best = null, bestD = 1e9;
  for (const sec of document.querySelectorAll('.book-module')) {
    const d = Math.abs(sec.getBoundingClientRect().top - innerHeight * 0.35);
    if (d < bestD) { bestD = d; best = sec.id.replace(/^mod-/, ''); }
  }
  active = best;
}
addEventListener('scroll', () => { spy(); requestAnimationFrame(layout); }, { passive: true });
addEventListener('resize', () => requestAnimationFrame(layout));
spy(); layout();
</script>
</body>
</html>
`;

// ── emit ──────────────────────────────────────────────────────────────────
mkdirSync(outDir, { recursive: true });
writeFileSync(join(outDir, 'index.html'), page);
writeFileSync(join(outDir, 'waxseal.svg'), sealSvg);
for (const f of ['style.css', 'sigil.css']) cpSync(join(PRELUDE_BOOK, f), join(outDir, f));

console.log(`${pkg.name}${pkg.version === 'local' ? '' : ' v' + pkg.version}: ${mods.length} modules, ${declTotal} declarations, ${locTotal} lines, layers 0..${maxLevel}`);
console.log(`unclassified blocks (BRaw): ${braw}`);
console.log(`→ ${outDir}`);
