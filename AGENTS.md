# standard-new

Turns a normative Fortran standard into StandardIR and the artifacts derived
from it: grammar, semantic constraints, rule dependencies and test families.
Nothing else.

General Fortran, git and agent rules live in `~/code/prompts/AGENTS.md` and
`~/code/prompts/rules/fortran.md` and always apply. The programme's
architecture, evidence and research conventions live in
`../lazy-fortran-new/`. This file records only what is specific here.

**The forbidden direction.** This repository is a production component and
stays boring. No research notes, no model comparisons, no benchmark histories,
no experiment manifests, no abandoned approaches, no LLM transcripts. All of
that belongs in the laboratory. The test for anything you are about to add: if
it would still matter after this repository was rewritten from scratch, it does
not belong here.

## Layout

- `src/` — the library. `fortpdf` is the poppler binding; StandardIR handling
  follows.
- `app/` — small executables. `pdfinfo` reports what `fortpdf` can see, so the
  binding can be checked against an independent extractor on a real document.
- `test/` — behavioural tests.
- `specs/` — StandardIR sources, once they exist.
- `generated/` — derived artifacts. Never hand-edited; a change here that did
  not come from a generator is a bug in the generator.

## Build and test

```sh
fo              # the full pipeline; run before every commit
fo build
fo test
fo test test_fortpdf
fo exec pdfinfo <file.pdf>
```

`poppler-glib`, `gobject-2.0` and `glib-2.0` must be installed. They are C
libraries reached through `ISO_C_BINDING`; there is no C shim and there is not
going to be one.

## Conventions

Everything is Fortran. External functionality is reached by binding an
established C library, not by reimplementing it and not by shelling out to
another language. The escape hatch is quantitative: a component may stay
someone else's C library when writing it would take on the order of a hundred
thousand lines. PDF rendering qualifies; PDF text extraction does not. See
`../lazy-fortran-new/research/decisions/D0003-fortran-everywhere.md`.

Modules under 500 lines, hard cap 1000. Procedures under 50 lines, hard cap
100. No production `include` fragments — `ffc` has 51 of them totalling 69,902
lines against the same rule, and that is the outcome this cap exists to
prevent.

Derived types end in `_t`. Errors are returned, never printed from a library
procedure. Every C interface is declared in the module that uses it, with the
C signature it mirrors visible in the declaration.

## Grammar productions are derived, not copied

StandardIR entries come from the normative document. Do not lift productions
out of `lazy-fortran/standard`'s `.g4` corpus, LFortran or Flang. Those are
comparisons and they may be wrong; the whole point of the measurement is that
the grammar was derived from the document.

This is a rule about how one artifact is produced, not a blindfold. Read those
grammars to understand the problem or to adjudicate a disagreement, and record
it in `../lazy-fortran-new/docs/provenance.md`.

gfortran is GPL: behavioural comparison only, never read its source while
authoring the corresponding component. That one is absolute.

## Text policy gate (non-negotiable)

Text is immutable bytes plus spans and interned IDs. A Fortran `character` is
for system boundaries and for implementing target-language CHARACTER
semantics, nothing else. Repeated `character(:)` concatenation into an
accumulator is forbidden.

```sh
scripts/check_text_policy.sh              # scans src and app
scripts/check_text_policy.sh --self-test  # proves the gate can fail
```

Run both before committing Fortran. A boundary use of an allocatable character
is allowed and must say so on the declaration:

```fortran
character(len=:), allocatable :: uri  ! text-policy: C string boundary
```

The rule is D0011 and the reasoning is
`../lazy-fortran-new/docs/text-representation.md`. It exists because the same
quadratic concatenation defect was fixed twice in `fortfront` seven months
apart, and the same substring bug three times.

## Provenance gate (non-negotiable)

Every StandardIR entry cites document, clause, rule number, page and the source
document's hash. An entry that cannot cite the document is not a formalization
of it and does not merge.

The normative document is never committed here. It is pinned in
`../lazy-fortran-new/artifacts/standards/` and fetched by
`../lazy-fortran-new/scripts/fetch.sh`.

## Adding a capability to fortpdf

1. Declare the C interface in `src/fortpdf.f90`, mirroring the poppler
   signature exactly.
2. Wrap it so callers see Fortran types and returned errors, not `c_ptr`.
3. Add a test whose expected answer is established independently of the
   library — a constructed fixture, or agreement with another extractor. A test
   that asserts whatever poppler returned is not a test.
4. Free everything the C API says you own.

## Quality gates before claiming done

1. `fo` green, with no new warnings, and `scripts/check_text_policy.sh` clean.
2. Every test can fail. Check it: break the code deliberately and confirm the
   test goes red. A suite that has never been observed failing is not evidence.
3. Anything read from an external source is recorded in the laboratory's
   provenance log.
4. Added or changed lines already follow `fo fmt` style; do not reformat files
   that were dirty before you touched them.
