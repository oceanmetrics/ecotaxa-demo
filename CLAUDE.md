# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Quarto website of R notebooks that take one EcoTaxa project (on the node https://ecotaxa.oceanmetrics.io)
to a Darwin Core Archive for OBIS-USA (notebook 03) and a BagIt-style archive package for NCEI (notebook 04).
Companion to the brief "US EcoTaxa: pipelines to OBIS and NCEI"; the mermaid diagrams in `diagrams/*.mmd`
feed both the notebooks and the brief, so edit the `.mmd` and re-render the PNG together.

**Guardrails:** nothing here submits to an IPT, OBIS, GBIF or NCEI, and the node is only read by the
notebooks. The NCEI route in 04 is a proposal. Do not add upload/submission steps.

## Commands

```sh
Rscript tests/run_tests.R                                    # all tests (stops on failure)
Rscript -e 'testthat::test_file("tests/testthat/test-taxa.R")'  # one file (helper-source.R auto-loads)
quarto render                                                # whole site into _site/
quarto render 03_publish_to-obis.qmd                         # one notebook
ECOTAXA_PROJECT_ID=4 quarto render 01_export_ecotaxa.qmd --output-dir _site_project-4
```

Packages load with `librarian::shelf()`. Credentials live in a git-ignored `.Renviron`: `ECOTAXA_URL`,
`ECOTAXA_USER`, `ECOTAXA_PASSWORD`, or `ECOTAXA_TOKEN` instead of a password. `ECOTAXA_REFRESH=1` forces a
new export instead of reusing `data/export/project-<id>/`.

## Architecture

- **Not an R package.** `R/*.R` are plain function files; every notebook and `tests/testthat/helper-source.R`
  `source()` all of them. Keep the rules in `R/` (and their tests in `tests/testthat/`, one file per rule
  set, synthetic fixtures in `helper-source.R` / `fixtures/`, a canned WoRMS so tests need no network);
  notebooks only call them. No roxygen/`@importFrom`; call other packages as `pkg::fun` or via the notebook's
  `librarian::shelf`.
- **Pipeline and hand-offs between notebooks** (all keyed by `ECOTAXA_PROJECT_ID`, default 2):
  - 01 (`R/ecotaxa_api.R`, httr2) runs the general export job → `data/export/project-<id>/` TSVs + images,
    cached as `objects.parquet`. Later notebooks never touch the node. Without a login on project 2 it falls
    back to `data-raw/import_test/`, byte-identical to project 2's eight objects.
  - 02 (`R/taxa.R`) decides per EcoTaxa category (`aphia | name_match | ancestor | excluded`, or
    `unresolved`) and fills WoRMS → **`metadata/taxa_lookup_project-<id>.csv`, committed and meant for
    taxonomist review**. Deliberately differs from EcoTaxa's own wormsification (a morphological category
    whose only living ancestor is Biota is excluded).
  - 03 (`R/dwc.R`, `R/eml.R`) builds Event core / Occurrence / eMoF, EML (from `metadata/projects.csv`),
    `meta.xml` (driven by the term map: a column with no term is an error) and the zip under
    `data/darwincore/`. It stops on any unresolved taxon (`assert_taxa_resolved`) or DwC finding
    (`assert_dwc`). Vocabulary ids (`metadata/measurement_type.csv`) are filled only on exact match, never
    guessed.
  - 04 (`R/ncei.R`) packages TSVs, images and the DwC-A with a SHA-256 manifest and verifies it.
  - 05 is `eval: false` documentation of migrating a project from another node (done for project 4, SFER
    MBON PlanktoScope, ~968k objects); `engine: knitr` is pinned because it has only bash chunks.
- **Owner decisions are data in `metadata/`, never edits to the export or code**: `sample_fixes.csv`
  (applied to objects in 03 and to the map), `taxa_review.csv` (applied after the rules in 02, because 02
  rewrites the lookup), `column_units.csv` over `ecotaxa_data_dictionary.csv` (NCEI's draft dictionary;
  units drive `ecotaxa_volumes()`, and an unknown volume unit stops), license and credits in `projects.csv`
  (`license_check()`: OBIS accepts CC0 / CC BY / CC BY-NC, NCEI no NC). Helpers in `R/metadata.R`.
- `data/`, `_site/`, `_site_project-*/` are git-ignored outputs. `quarto publish` uses `_site` (project 2);
  per-project renders go to `_site_project-<id>` and are not published.
- Structure borrowed from the CalCOFI Darwin Core pipeline (calcofi4db `R/dwc.R`).

## Scripts and machines (`scripts/README.md`)

- `scripts/launch_project.sh <id> [from]` runs on the **laptop**: gets a token on the node host (`ssh
  msens`), hands it to the **mini** in a mode-600 file, and starts `render_project.sh` in tmux `om:prj<id>`.
  The render writes `data/run_project-<id>.log` and a `.done` marker (`ok`/`failed`); check those rather
  than attaching. Large projects take hours.
- `scripts/node/ecotaxa_admin.py` is piped over ssh (`ssh msens 'python3 -I - <cmd>' < ...`); the admin
  login stays in `/share/ecotaxa/.env` on the node and is never printed. `schema_diff.py` is read-only;
  `2026-11-10_schema_to_head.sql` is a historical record — do not re-run.
- People's names/emails go only in git-ignored `scripts/node/users_*.csv`.
