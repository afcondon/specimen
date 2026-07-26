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

- **A book per package, plus a shelf over them**, by default. The shelf is
  the most striking thing Specimen makes; burying it behind a flag would
  be a mistake. A single-package project gets a book and either a shelf of
  one or no shelf.
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

### 1. The shelf can't be generated generically — the real work

`cli/specimen-shelf.mjs` requires `cli/shelf.config.json`: per-book
authors, invented pull quotes, fake periodical names, shelf titles,
blurbs. That is editorial voice. A stranger has none, and inventing it for
them would be worse than omitting it.

So the port of the shelf generator (the last JavaScript in the repo) and
this are **one job**, and the port's design changes because of it:

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

## Ordering

1. **Port `specimen-shelf.mjs` to PureScript, with editorial optional.**
   Unblocks the wizard and removes the last JavaScript from the repo.
2. **Workspace discovery** in `Sources`.
3. **The `specimen` entry point** — subcommand-free, infers, prompts only
   when stuck.
4. **npm packaging** and a first publish.

## Related, but independent

`cli/assets/book.js` is still 141 lines of hand-written browser
JavaScript — the scroll morph, the scroll-spy, the FFI modal — and it is
what every *visitor* executes. Rebuilding it as a Halogen island (the way
`shelf-ui/` already works) matters more once strangers are shipping these
pages, since it is then their readers running it. It does not block
packaging, and packaging does not block it.

Note the live `docs/` books still carry that script **inlined**; they
predate the extraction. The pending republish moves them to `book.js`.
