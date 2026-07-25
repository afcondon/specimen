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

## Building

```bash
spago build
spago test                       # golden tests over the pure pipeline
SPECIMEN_ACCEPT=1 spago test     # re-record goldens after an intended change

spago bundle                     # public/bundle.js
npm run serve                    # the viewer on :3007
```

The viewer takes a `?module=` query parameter naming any module under
`public/examples/`, e.g. `localhost:3007/?module=Data.Variant`.

## Making a book

```bash
node cli/specimen-site.mjs <package-name | workspace-dir> [-o dir]
node cli/specimen-shelf.mjs cli/shelf.config.json --sites docs
```

`specimen-site` resolves a registry package with `spago fetch`, renders
every module through the pipeline, and writes a self-contained static
site — plus `banner.svg`, `waxseal.svg` and `book.json` for the shelf
page to draw from. `docs/` in this repo is the generated shelf.

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

- `cli/specimen-site.mjs` and `cli/specimen-shelf.mjs` are still
  JavaScript. They no longer compute anything — layout and typesetting
  are both PureScript calls now — but page assembly, SVG emission and
  the filesystem walk still live in JS, and the generated page still
  carries ~130 lines of inline vanilla JS for the scroll morph and the
  FFI modal.
- Commentary panels are emitted with the `data-panel` wiring but nothing
  toggles them yet; `renderDocument` is deliberately action-free
  (`forall w i`), so opening them needs an action type threaded through.
- The book stylesheet exists in more than one copy
  (`public/style.css`, `cli/assets/style.css`) and they have drifted.
