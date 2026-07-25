// The book page's behaviour: the hero swarm morphing into the left-margin
// rail on scroll, the scroll-spy that tracks the active module, and the
// FFI modal.
//
// Its data arrives as a JSON payload in the page (#book-index) rather
// than being interpolated into this file, so this is a static asset the
// generator copies rather than a string the generator builds.

const PAYLOAD = JSON.parse(document.getElementById('book-index').textContent);
const DATA = PAYLOAD.modules;
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
  const label = document.createElementNS(NS, 'text');
  label.textContent = d.name;
  label.setAttribute('text-anchor', 'middle');
  label.setAttribute('y', -(d.rA + 9));
  label.setAttribute('style', 'font-family: Inter, sans-serif; font-size: ' + PAYLOAD.labelSize + 'px; letter-spacing: 0.08em; fill: #777; stroke: rgba(250,250,247,0.88); stroke-width: 3px; paint-order: stroke; stroke-linejoin: round;');
  g.appendChild(label);
  const t = document.createElementNS(NS, 'title'); t.textContent = d.name; g.appendChild(t);
  a.appendChild(g); svg.appendChild(a);
  gs[d.slug] = { g, bubble, ring, inner, label, d };
}

let active = null;
const heroEl = document.getElementById('hero-title');
function layout() {
  const W = innerWidth, H = innerHeight;
  const p = ease(Math.min(1, Math.max(0, scrollY / MORPH)));
  svg.classList.toggle('railed', p > 0.95);
  const bannerH = H * 0.66;
  // at rest the banner band sits below the hero block (tall left columns
  // were overwriting the masthead); the band's top eases back toward the
  // page top as the morph runs, since layout B's rail already lives there
  const bandTop = Math.max(90, heroEl.getBoundingClientRect().bottom + 28) * (1 - p) + 90 * p;
  heroEl.style.opacity = String(Math.max(0, 1 - p * 1.4));
  // rail column: centred in the left margin, same slot as the-prelude's beeswarm
  const railLeft = Math.max(12, 0.5 * W - 36 * 16 - 12 * 16);
  for (const s of Object.values(gs)) {
    const d = s.d;
    const axp = d.ax * W, ayp = bandTop + d.ay * (bannerH - 90);
    const bxp = railLeft + d.bx, byp = 60 + (d.by / 900) * (H - 120);
    const x = axp + (bxp - axp) * p, y = ayp + (byp - ayp) * p;
    const r = d.rA + (d.rB - d.rA) * p;
    s.g.setAttribute('transform', 'translate(' + x + ',' + y + ')');
    s.ring.setAttribute('r', r);
    s.inner.setAttribute('transform', 'scale(' + (r / d.rA) + ')');
    s.inner.setAttribute('opacity', String(Math.max(0, 1 - p * 1.7)));
    s.label.setAttribute('y', -(r + 9));
    s.label.setAttribute('opacity', String(Math.max(0, 1 - p * 1.7)));
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

// ── FFI modal ──
const FFI = PAYLOAD.ffi;
const LANG_NAMES = { javascript: 'JavaScript', erlang: 'Erlang', julia: 'Julia', python: 'Python', go: 'Go' };
const modal = document.getElementById('ffi-modal');
const mTitle = modal.querySelector('.ffi-title');
const mTabs = modal.querySelector('nav');
const mPre = modal.querySelector('pre');
function openFfi(slug) {
  const entry = FFI[slug];
  if (!entry) return;
  mTitle.textContent = entry.name;
  mTabs.innerHTML = '';
  const langs = Object.keys(entry.langs);
  const show = lang => {
    mPre.textContent = entry.langs[lang];
    for (const b of mTabs.children) b.classList.toggle('on', b.dataset.lang === lang);
  };
  for (const lang of langs) {
    const b = document.createElement('button');
    b.textContent = LANG_NAMES[lang] ?? lang;
    b.dataset.lang = lang;
    b.addEventListener('click', () => show(lang));
    mTabs.appendChild(b);
  }
  show(langs[0]);
  modal.hidden = false;
}
document.addEventListener('click', e => {
  const row = e.target.closest('.row.kind-foreign[data-ffi]');
  if (row) { openFfi(row.dataset.ffi); return; }
  if (e.target.closest('.ffi-close') || e.target.classList.contains('ffi-backdrop')) modal.hidden = true;
});
addEventListener('keydown', e => { if (e.key === 'Escape') modal.hidden = true; });
