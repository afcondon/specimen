# Specimen

> Typeset a single PureScript module as if it were a printed specimen — a
> poster, an artifact, made to show off a library or an algorithm.
> Not an editor view, not pretty syntax highlighting. Typography.

The eye should be drawn to the *structure* of the code first, the way
you read a Swiss poster: hierarchy through type, alignment, rhythm,
restraint.

Sibling project of [`purescript-sigil`](../../purescript-hylograph-libs/purescript-sigil/).
Marginalia: project 179 (`delta-yankee-tango-november`).

## Status

Specimen is a **library** consumed by typeset-book sites such as
[The Prelude](https://github.com/afcondon/the-prelude). It is not yet
published to the PureScript registry — the API surface is still
shifting (most recently, the addition of swappable commentary tracks).
Consume it via a `path:` extra-package entry in your `spago.yaml`
until it stabilises.

## Try it on your own code

Specimen ships as a repository, not a package. Clone it and run it —
there is nothing to build first, because `cli/specimen-site.js` is a
committed, self-contained Node bundle with no dependencies:

```bash
git clone https://github.com/afcondon/specimen.git
cd specimen
./specimen ~/code/purescript-my-library
```

That's it. No spago, no PureScript toolchain, no `npm install` — Node 18
or later is the only requirement. You get a directory holding a complete
static site: the typeset book, its stylesheets, the banner plate, the
waxseal, and a `book.json` of the facts.

```bash
./specimen <package-name | directory> [options]

  -o, --out <dir>    where to write        (default ./site/<package>)
  --title <string>   book title            (default: the package name)
  --deck <string>    the line under the title
  --mark <glyph>     the mark at the seal's foot   (default λ)
  --include-tests    include test/ modules
```

A **directory** is scanned for its `.purs` files and needs nothing else.
A **registry package name** — `./specimen aff` — is fetched with
`spago fetch` into a throwaway workspace, which is the one case that
needs spago on your PATH.

Open `index.html` in the output directory. Every page is self-contained
and can be served from `file://` or any static host.

## Pipeline

```
.purs file
   │
   ▼
[1] preprocess        Specimen.Preprocess — glyph substitution, comment extraction
   │
   ▼
[2] block classify    Specimen.Block — partition into typed regions (see taxonomy)
   │
   ▼
[3] signature analysis Specimen.Sig — where a type breaks, what it binds, ∀-var colours
   │
   ▼
[4] typeset           Specimen.Render — blocks → Halogen HTML
      └ decoration    Specimen.Html — composable token passes over code fragments
   │
   ├──────────────► Specimen.Component   live, in the browser
   └──────────────► renderDocumentHtml   serialised, for static sites
```

Every stage is a pure function over data. Nothing before the last step
touches the DOM, which is why `test/Test/Main.purs` can be a golden test
over the whole pipeline.

### One tree, two backends

`Specimen.Render.renderDocument` produces `HH.HTML w i` — the same value
a Halogen component renders. `renderDocumentHtml` serialises that value
through `halogen-vdom-string-renderer` for the static-site generator.
There is one description of the document, and both consumers agree by
construction.

Source text becomes markup only in `Specimen.Html.toHtml`, so escaping
happens exactly once. Decorations (glyph emphasis, type-variable
colour, quieted keywords, soft-break opportunities) are passes over an
`Array Token` that rewrite only the undecorated `Plain` runs — they
compose in any order and cannot corrupt each other's output.

## Sigil

Specimen is a sibling of
[`purescript-sigil`](../../purescript-hylograph-libs/purescript-sigil/),
not yet a consumer of it. Sigil typesets from a parsed `RenderType`
AST; `Specimen.Sig` works structurally over signature *text*, which is
what lets Specimen typeset a module without a full CST round-trip.
Routing Specimen's declarations through Sigil's AST — so signature
layout is shared rather than reimplemented — is the open piece of work.

## Block taxonomy

| Block                 | Treatment                                                                  |
|-----------------------|----------------------------------------------------------------------------|
| Module header         | Drop-cap module name; export list as a small-caps colophon                 |
| Imports               | Right-aligned `(...)` lists; `as` clauses column-aligned                   |
| Signature group       | `::` column-aligned within consecutive signatures                          |
| Data decl             | Constructors stacked, `\|` in left gutter, arities right-aligned           |
| Type class / instance | Heads at consistent indent; `::` columns shared with sigs                  |
| Record type           | Names left, `::` aligned, types left-aligned in their column               |
| Function clauses      | `=` aligned across siblings; guards' `\|` in gutter                        |
| `where` block         | Own alignment scope for local sigs + bindings                              |
| `let ... in`          | Same scoping rules as `where`                                              |
| `case` expr           | `->` aligned across patterns within one `case`                             |
| Comments              | Floated to the margin as marginalia, not inlined                           |

## Alignment moves

- **Vertical columns in sibling groups**: `::`, `=`, `->`, `<-`, `|`
- **Right alignment**: import lists, constructor arities, numeric tables,
  `where` over its block
- **Left gutter**: pipes, guards, marginalia
- **Hanging indents**: long types break with `→` hanging in the gutter
  (LaTeX-style hanging punctuation)
- **Operator hangs** (Knuth-ish): `<>`, `>>=`, `$`, `<$>` slightly into
  the gutter so operands form a clean left edge

## Preprocess passes (cheap-and-cheerful)

Things easier as text than as CST surgery:

- Glyph substitution (toggleable, off by default):
  `::`→`∷`, `->`→`→`, `=>`→`⇒`, `<-`→`←`, `\`→`λ`, `forall`→`∀`
- Strip trailing whitespace, normalize tabs
- *Tag* long lines for the layout pass; don't reflow
- Extract leading `--` comments above declarations as candidate marginalia

## Ornament layer

- Hairline rule between top-level declarations
- Module name as headline; deck below it for purpose / one-line description
- Instances marked with `§` or `¶` in the margin
- Page-foot colophon: package, version, source URL, build date
- Lining figures for arities
- Optional later: anatomy callouts pointing at a key construct

**Line numbers**: off by default. If used at all, very pale, old-style
figures, hung in the outer margin so they don't compete with the code's
left edge.

## Building from source

Only needed if you want to change Specimen itself.

```bash
spago build
spago test                       # golden tests over the pure pipeline
SPECIMEN_ACCEPT=1 spago test     # re-record goldens after an intended change

spago bundle -p specimen-site    # rebuilds cli/specimen-site.js
spago bundle                     # public/bundle.js, for the live viewer
npm run serve                    # the viewer on :3007
```

The viewer takes a `?module=` query parameter naming any module under
`public/examples/`, e.g. `localhost:3007/?module=Data.Variant`.

`cli/specimen-site.js` is committed deliberately: it is what makes the
clone-and-run path work for people who don't have the PureScript
toolchain. Rebundle it when you change anything under `site/` or `src/`.

## The shelf

The shelf page — the index over many books, as at
[afcondon.github.io/specimen](https://afcondon.github.io/specimen/) — is
a separate step, and presently needs hand-written editorial (shelf
titles, blurbs, per-book authors and pull quotes) in
`cli/shelf.config.json`:

```bash
node cli/specimen-shelf.mjs cli/shelf.config.json --sites docs
```

`docs/` in this repo is the generated shelf. Making a shelf that derives
itself from the books, so it is useful without that editorial, is open
work — see `notes/wizard.md`.

## The book's geometry

Everything the identity kit is built from lives in `site/` as the
`specimen-site` package, and it is all pure PureScript:

| Module | |
|---|---|
| `Site.Harvest` | module name, imports, LOC, declaration spans |
| `Site.Topo` | import-graph pruning, longest-path layering, accent hues |
| `Site.Pack` | the plates and the waxseal, via Hylograph's circle-pack |
| `Site.Beeswarm` | the force settle, run once at build time |
| `Site.Layout` | assembles the above into a laid-out book |
| `Site.Sources` | resolving a target to source files, and the fetch cache |
| `Site.Registry` | release history, for the masthead's timeline |
| `Site.Svg` | the seal, the banner and the sparkline |
| `Site.Book` | the book page |
| `Site.Main` | the `specimen-site` command |

**Specimen has no npm dependencies.** The circle-packing comes from
`DataViz.Layout.Hierarchy.Pack` in `hylograph-layout`, which reproduces
d3's front-chain algorithm exactly — verified circle-for-circle against
`d3-hierarchy` on a 50-module book, and rather more robustly (d3 emitted
one non-finite radius on that data; the port emitted none).

The force settle is hand-written, and deliberately so: Hylograph's
`hylograph-simulation` is an FFI wrapper over `d3-force`, so adopting it
would have renamed the dependency rather than removed it. It belongs in
`hylograph-layout` eventually — a pure, deterministic settle is not a
Specimen-specific idea.

## Known debt

- **`cli/specimen-shelf.mjs` is the last JavaScript** — 229 lines, and
  the only thing left that assembles markup by concatenation. It ports
  the way the book generator did: the covers and book cards want the
  typed-HTML treatment, and the page assembly mirrors `Site.Book`.
  Its design should change with the port, because the shelf currently
  *requires* hand-written editorial (`shelf.config.json`) and needs to be
  able to derive itself from the books instead — see `notes/wizard.md`.
- **`docs/` is behind the generator.** The published shelf still has the
  older self-contained pages; regenerating now also emits `book.css` and
  `book.js` beside each book. Worth doing as one republish once the
  shelf generator is ported, rather than twice.
- `cli/assets/book.js` and `shelf.js` are the two browser scripts. The
  shelf's is compiled from Halogen (`shelf-ui/`); the book's — the
  scroll morph, the scroll-spy, the FFI modal — is still hand-written
  JavaScript, and is the obvious next thing to build with Hylograph.
- Commentary panels are emitted with the `data-panel` wiring but nothing
  toggles them yet; `renderDocument` is deliberately action-free
  (`forall w i`), so opening them needs an action type threaded through.
- The book stylesheet exists in more than one copy
  (`public/style.css`, `cli/assets/style.css`) and they have drifted.
