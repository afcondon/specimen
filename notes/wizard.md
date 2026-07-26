# `npx specimen` — packaging Specimen for other people

Turning Specimen from a thing that typesets *this* shelf into a thing
anyone can point at their own PureScript and get a book out of.

> Note: this file is deliberately **not** under `docs/` — that directory
> is the GitHub Pages publish root and everything in it is served.

## The shape, decided

```
$ npx specimen
  found spago.yaml — 3 packages
  ✓ core      12 modules
  ✓ client     8 modules
  ✓ shared     4 modules
  ✓ shelf over 3 books
  → ./specimen  (open index.html)
```

- **One book for one library** is the whole job. An earlier draft had this
  producing a book per package plus a shelf over them; that was scope the
  tool doesn't need. A single page for a single library is what someone
  trying Specimen on their own code actually wants.
- **Zero prompts when it can infer.** Reads `spago.yaml` and just runs.
  Prompts only on genuine ambiguity (several packages, no target given),
  and every prompt has a flag equivalent so it stays scriptable. `--yes`
  never prompts, for CI.

## What already works

More than expected, after the July 2026 port. From any directory, against
any PureScript source tree, with **only Node installed** — no spago, no
clone of this repo:

```
node cli/specimen-site.js ~/some/purescript-project -o ./out
→ 19 modules, 284 declarations, 3406 lines, layers 0..4
```

Output is self-contained: `index.html`, `style.css`, `sigil.css`,
`book.css`, `book.js`, `banner.svg`, `waxseal.svg`, `book.json`. `spago`
is only shelled out to for *registry* targets; a local directory needs
nothing.

So the engine is done. What is missing is distribution and the shelf.

## The gaps

### 1. The shelf — no longer a prerequisite

`cli/specimen-shelf.mjs` requires `cli/shelf.config.json`: per-book
authors, invented pull quotes, fake periodical names, shelf titles,
blurbs. That is editorial voice. A stranger has none, and inventing it for
them would be worse than omitting it.

Since the CLI produces a single book, none of this blocks it. It matters
if the shelf is ever to be useful to anyone but this repo, and the port
of the shelf generator (the last JavaScript here) is the moment to do it:

- the shelf must **derive itself from the books** it is given — names,
  module counts, versions, timelines all already live in each
  `book.json`;
- `shelf.config.json` becomes **optional enrichment**, not required
  input. Present: shelves, blurbs, quotes, authors as today. Absent: one
  shelf, books in dependency or alphabetical order, no quotes;
- colour needs a default palette when no shelf colours are given — the
  tab bar keys off shelf colour, so it cannot be absent.

### 2. Distribution

An npm package shipping the bundle plus `cli/assets/`. The bundle is
plain Node JS with **zero npm dependencies**, so this is packaging rather
than engineering. `specimen` is taken on npm; `@afcondon/specimen` or
`purescript-specimen`.

Needs a `prepublishOnly` that runs `spago bundle -p specimen-site` so the
shipped artifact can't drift from the source.

### 3. Aiming it at a workspace

Read `spago.yaml`: workspace root, `package:` block, and any subdirectory
packages. Decide per package rather than making the user reason about
whether to point at the root, a package, or `src/`.

Discovery belongs in `Specimen.Site.Sources` beside `resolvePackage`,
which already knows how to walk a tree and skip `output`, `node_modules`
and (by default) `test`.

## Distribution: the repo is the package

Not npm. The PureScript community is small enough that cloning is a
reasonable ask, and a clone gives someone both halves at once — the
source to build and tweak, and a tool that runs immediately.

That works today because **`cli/specimen-site.js` is a committed build
artifact**: a self-contained Node bundle with no dependencies. Verified
against a fresh clone of the public repo with no build step and no spago
on PATH. `./specimen` at the repo root is the ergonomic entry point.

`package.json` stays `private: true` so nothing can be published by
accident. If that is ever revisited, note `specimen` is taken on npm;
`purescript-specimen` and `@afcondon/specimen` were both free.

## Still open

1. **Workspace discovery** — reading `spago.yaml` to offer packages,
   rather than making the user reason about whether to point at the root,
   a package, or `src/`. Belongs in `Specimen.Site.Sources`.
2. **Port `specimen-shelf.mjs` to PureScript, with editorial optional.**
   Removes the last JavaScript from the repo, and is what would make the
   shelf useful to anyone but this repo.

## Deferred

**Nicer tab design.** Wants arbitrary content in a `Segment` rather than a
decoration keyed off colour, which is a `halogen-widgets` feature — 0.3.0,
a publish cycle, and a Specimen bump. Not worth the loop for a visual
tweak; revisit when something else needs a widget release anyway.

## Related, but independent

`cli/assets/book.js` is still 141 lines of hand-written browser
JavaScript — the scroll morph, the scroll-spy, the FFI modal — and it is
what every *visitor* executes. Rebuilding it as a Halogen island (the way
`shelf-ui/` already works) matters more once strangers are shipping these
pages, since it is then their readers running it. It does not block
packaging, and packaging does not block it.

Note the live `docs/` books still carry that script **inlined**; they
predate the extraction. The pending republish moves them to `book.js`.
