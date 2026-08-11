# standard-new

Normative Fortran standard → StandardIR → generated grammar, semantic
constraints, rule dependencies and test families.

This is the first production component of the specification-generated Lazy
Fortran compiler. The architecture, the evidence behind it and the research
record live in [lazy-fortran-new](https://github.com/lazy-fortran/lazy-fortran-new);
this repository contains only the code and the specifications.

## State

Early. `fortpdf`, the poppler binding, opens a document and reports its page
count. No extraction, no StandardIR, no grammar yet. `ROADMAP.md` in the
laboratory carries the phase gates.

## Build

```sh
fo
fo test
fo exec pdfinfo <file.pdf>
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
