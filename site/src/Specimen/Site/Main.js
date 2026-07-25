import { readFileSync, writeFileSync, mkdirSync, existsSync, cpSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

// Vendored stylesheets and the page's script, copied beside every book.
// Resolved from the bundle's own location: the generator ships as
// cli/specimen-site.js, with the assets alongside it.
const ASSETS = join(dirname(fileURLToPath(import.meta.url)), "assets");
const COPIED = ["style.css", "sigil.css", "book.css", "book.js"];

export const args = () => process.argv.slice(2);
export const exit = (code) => () => process.exit(code);

export const die = (message) => () => {
  console.error(message);
  process.exit(1);
};

export const readText = (path) => () => readFileSync(path, "utf8");
export const writeText = (path) => (contents) => () => writeFileSync(path, contents);
export const makeDirectory = (path) => () => mkdirSync(path, { recursive: true });
export const joinPath = (parts) => join(...parts);
export const isoDate = () => new Date().toISOString().slice(0, 10);
export const epochMillis = () => Date.now();

export const copyAssets = (outDir) => () => {
  for (const f of COPIED) cpSync(join(ASSETS, f), join(outDir, f));
};

export const dataUri = (svg) =>
  "data:image/svg+xml;base64," + Buffer.from(svg).toString("base64");

const slugify = (name) => name.replace(/\./g, "-");

export const sidecarsFor = (paths) => () => {
  const found = [];
  for (const path of paths) {
    const js = path.replace(/\.purs$/, ".js");
    if (!existsSync(js)) continue;
    const name = (readFileSync(path, "utf8").match(/^module\s+([\w.]+)/m) ?? [])[1];
    if (name) found.push({ name, slug: slugify(name), javascript: readFileSync(js, "utf8") });
  }
  return found;
};

// `<` is escaped so an embedded payload can never close its own script tag.
const embed = (value) => JSON.stringify(value).replace(/</g, "\\u003c");

export const payloadJson = ({ modules, ffi, labelSize }) =>
  embed({
    modules: modules.map((m) => ({
      name: m.name, slug: m.slug, level: m.level, loc: m.loc, accent: m.accent,
      imports: m.imports,
      ax: m.ax, ay: m.ay, bx: m.bx, by: m.by,
      rA: +m.rA.toFixed(1), rB: +m.rB.toFixed(1),
      pack: m.plate,
    })),
    ffi: Object.fromEntries(
      ffi.map((s) => [s.slug, { name: s.name, langs: { javascript: s.javascript } }]),
    ),
    labelSize,
  });

export const bookJson = (facts) =>
  JSON.stringify(
    { ...facts, stableSince: facts.stableSince === "" ? null : facts.stableSince,
      releases: facts.releases.map((r) => ({ v: r.version, t: r.at, major: r.major })) },
    null, 2,
  );
