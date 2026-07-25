import { existsSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { join, dirname } from "node:path";
import { tmpdir } from "node:os";

const METADATA = "https://raw.githubusercontent.com/purescript/registry/main/metadata";

// Cached beside the vendored sources: a book is rebuilt far more often
// than a library is released.
const cachePath = (name) =>
  join(tmpdir(), "specimen-site-fetch", name, "registry-metadata.json");

export const fetchReleases = (name) => async () => {
  let meta;
  try {
    const cache = cachePath(name);
    if (existsSync(cache)) {
      meta = JSON.parse(readFileSync(cache, "utf8"));
    } else {
      const res = await fetch(`${METADATA}/${name}.json`);
      if (!res.ok) return [];
      meta = await res.json();
      mkdirSync(dirname(cache), { recursive: true });
      writeFileSync(cache, JSON.stringify(meta));
    }
  } catch {
    // No network, no cache, malformed metadata — the masthead loses its
    // timeline and the book is otherwise unaffected.
    return [];
  }
  return Object.entries(meta.published ?? {})
    .map(([version, m]) => ({ version, at: Date.parse(m.publishedTime), major: false }))
    .filter((r) => Number.isFinite(r.at))
    .sort((a, b) => a.at - b.at);
};

const MONTHS = ["January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"];

export const monthYear = (millis) => () => {
  const d = new Date(millis);
  return `${MONTHS[d.getMonth()]} ${d.getFullYear()}`;
};
