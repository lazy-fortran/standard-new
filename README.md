# standard-new

Normative Fortran standard → StandardIR → generated grammar, semantic
constraints, rule dependencies and test families.

This is the first production component of the specification-generated Lazy
Fortran compiler. The architecture, the evidence behind it and the research
record live in [lazy-fortran-new](https://github.com/lazy-fortran/lazy-fortran-new);
this repository contains only the code and the specifications.

## State

Early. `fortpdf` extracts Poppler's UTF-8 text and glyph rectangles, and the
small command-line tools produce raw layout, canonical text, provenance-
bearing production records, and a StandardIR SX projection. The parser retains
multi-token sequences, optional/repeated groups, terminal tokens and
source-clause provenance.
Generated grammar and semantic rules are not implemented yet. `ROADMAP.md` in
the laboratory carries the phase gates.

## Build

```sh
fo
fo test
fo exec pdfinfo <file.pdf>
fo exec pdfextract <file.pdf> <layout.txt>
fo exec pdfcanonical <file.pdf> <canonical.txt> <pages.index>
fo exec pdfproductions <canonical.txt> <pages.index> <productions.jsonl> 53 56
fo exec pdfstandardir <productions.jsonl> <standardir.sx> <source-sha256> <clause>
fo exec sxroundtrip <standardir.sx> <roundtripped.sx>
fo exec sxnormalize <standardir.sx> <normalized.jsonl>
```

Requires `poppler-glib`, `gobject-2.0` and `glib-2.0`, reached through
`ISO_C_BINDING`. No C shim, no other language in the build path.

## The normative document

Not committed here. It is pinned by URL and SHA-256 in the laboratory and
fetched into a gitignored cache:

```sh
cd ../lazy-fortran-new && scripts/fetch.sh j3-24-007
cd ../standard-new && fo exec pdfinfo ../lazy-fortran-new/.cache/j3-24-007.pdf
```

The target is J3/24-007, the Fortran 2023 final working draft: freely
available, and technically near-identical to ISO/IEC 1539-1:2023.

## Licence

MIT. See `LICENSE`. External documents and grammars keep their own licences and
are never vendored.
