# ecotaxa-demo

Companion to the brief **US EcoTaxa: pipelines to OBIS and NCEI** (6 November 2026, a Word document kept
with the proposal files, built from the same mermaid diagrams as these notebooks). A Quarto website of R notebooks that take one EcoTaxa project all the way to a Darwin
Core Archive for OBIS-USA and to an archive package for NCEI, running on the demo node at
https://ecotaxa.oceanmetrics.io (project 2, "US EcoTaxa demo", eight Zooscan objects).

| | |
|---|---|
| `index.qmd` | overview and the end-to-end diagram |
| `01_export_ecotaxa.qmd` | project export through the REST API (`httr2`, token login, export job), cached under `data/` |
| `02_taxa_worms.qmd` | EcoTaxa categories to WoRMS (`worrms`) as the reviewable `metadata/taxa_lookup_project-<id>.csv` |
| `03_publish_to-obis.qmd` | Event / Occurrence / eMoF, EML, `meta.xml`, DwC-A zip, checks (`obistools`); no upload |
| `04_archive_to-ncei.qmd` | BagIt-style package with SHA-256 manifest, verified; the submission route as a proposal |
| `R/` | the functions the notebooks call: `ecotaxa_api.R`, `taxa.R`, `dwc.R`, `eml.R`, `ncei.R` |
| `tests/testthat/` | one test file per rule set, on small synthetic fixtures; `Rscript tests/run_tests.R` |
| `diagrams/` | mermaid sources (`*.mmd`) and their PNG renders, used by both the notebooks and the brief |
| `metadata/` | `measurement_type.csv` (NERC ids, filled only on an exact match) , `projects.csv` (title, abstract, license and contact per project) and the generated `taxa_lookup_project-<id>.csv` |
| `data-raw/import_test/` | the upstream EcoTaxa test set, the same eight objects as project 2, for runs without a login |

## Run

```sh
# credentials (never committed): .Renviron with ECOTAXA_URL, ECOTAXA_USER, ECOTAXA_PASSWORD
Rscript tests/run_tests.R
quarto render
```

Without `ECOTAXA_USER` / `ECOTAXA_PASSWORD`, notebook 01 reads `data-raw/import_test/` instead of calling the
export endpoint and says so; the rest of the pipeline is identical.

## Guardrails

No submission to any IPT, to OBIS, to GBIF or to NCEI happens here. The node is only read. The NCEI route in
notebook 04 is a proposal to agree with NCEI.

Borrowed in structure from the CalCOFI Darwin Core pipeline ([workflows](https://calcofi.io/workflows/#publish),
[calcofi4db](https://calcofi.io/calcofi4db/)).
