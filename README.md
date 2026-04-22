# Specimen

> Typeset a single PureScript module as if it were a printed specimen — a
> poster, an artifact, made to show off a library or an algorithm.
> Not an editor view, not pretty syntax highlighting. Typography.

The eye should be drawn to the *structure* of the code first, the way
you read a Swiss poster: hierarchy through type, alignment, rhythm,
restraint.

Sibling project of [`purescript-sigil`](../../purescript-hylograph-libs/purescript-sigil/).
Marginalia: project 179 (`delta-yankee-tango-november`).

## Pipeline

```
.purs file
   │
   ▼
[1] preprocess        regex passes — glyph subs, comment extraction, long-line tagging
   │
   ▼
[2] CST parse         purescript-language-cst-parser, tokens & whitespace retained
   │
   ▼
[3] block classify    partition into typed regions (see taxonomy)
   │
   ▼
[4] alignment passes  one per alignment family, each operates on a region type
                      and emits shared column anchors
   │
   ▼
[5] ornament layer    decorate the block stream — rules, marginalia, drop caps,
                      running heads, gutter glyphs
   │
   ▼
[6] Sigil render      whole document → HATS → SVG/HTML
```

Stages 1, 3, 4, 5 are independent small modules over the same block
data. Only stage 2 has heavy dependencies.

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

## Sigil's job

Everything past stage 5 is Sigil. Each block is a Sigil layout (or
composition). Cross-block alignment is realized by passing **shared
column anchors** into the tree. If Sigil doesn't yet support that exact
shape, treat it as a small upstream addition rather than working around it.

## First milestone — "hello, module"

Pick one short, charismatic module and render it with:

- Module header ornament
- `::` column alignment within signature groups
- Hairline rule between decls
- Sans-serif body, monospace code, restrained two-color palette
- Static SVG/HTML, no interactivity

The first artifact should be screenshotable as-is, and worth printing.

Candidate first targets (TBD):

- **`Control.Monad`** — small, classic, well-known shape; safest engineering target
- **a slice of `purerl-tidal`** — personal, charismatic; better "show off" piece
- **`purescript-language-cst-parser`** — meta: typesetting the code that parses code
