# metadata.R: the hand-kept decisions that sit beside an export (metadata/*.csv)
#
# each file records a person's decision with its source, so a correction is reviewable and never
# made silently in code: the column data dictionary NCEI is drafting (definitions and units), the
# owner's unit corrections to it, fixes to sample fields, and the licenses each target accepts.

# the data dictionary ----

# metadata/ecotaxa_data_dictionary.csv: the EcoTaxa column dictionary NCEI is drafting (saved from
# the shared sheet "EcoTaxa_data_dictionary", 2026-11-12). the sheet's working Status column is dropped.
read_data_dictionary <- function(path) {
  d <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  stopifnot(all(c("Field", "Descriptive Category", "Definition", "Units") %in% names(d)))
  tibble::tibble(
    field      = trimws(d$Field),
    category   = trimws(d$`Descriptive Category`),
    definition = trimws(d$Definition),
    unit       = dplyr::na_if(trimws(d$Units), ""))
}

# metadata/column_units.csv: a unit a data owner gave for one project's column where the dictionary
# says otherwise or says nothing (project_id, field, unit, source)
read_column_units <- function(path, project_id) {
  u <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(project_id = "i", .default = "c"))
  u[u$project_id == as.integer(project_id), c("field", "unit", "source")]
}

# one row per exported column: the dictionary's category, definition and unit, the owner's unit
# where one is recorded, and whether the dictionary documents the column at all. this is the
# column dictionary of the NCEI package and the source of the units the volumes are converted from.
ecotaxa_column_dictionary <- function(cols, dict, units = NULL) {
  out <- tibble::tibble(field = cols) |>
    dplyr::left_join(dict[!duplicated(dict$field), ], by = "field") |>
    dplyr::mutate(documented = !is.na(definition),
                  unit_source = ifelse(is.na(unit), NA_character_, "data dictionary"))
  if (!is.null(units) && nrow(units)) {
    i <- match(out$field, units$field); hit <- !is.na(i)
    out$unit[hit]        <- units$unit[i[hit]]
    out$unit_source[hit] <- units$source[i[hit]]
  }
  out
}

# the unit of each named column from a column dictionary, NA when none is known
column_unit <- function(coldict, field) {
  u <- coldict$unit[match(field, coldict$field)]
  stats::setNames(ifelse(is.na(u) | u %in% c("N/A", ""), NA_character_, u), field)
}

# factor from a volume unit to m3; an unknown unit stops rather than guessing
VOLUME_TO_M3 <- c(m3 = 1, L = 1e-3, l = 1e-3, mL = 1e-6, ml = 1e-6, uL = 1e-9, `µL` = 1e-9)
volume_to_m3 <- function(value, unit) {
  if (is.na(unit) || !unit %in% names(VOLUME_TO_M3))
    stop("no known volume unit (", unit, "); record it in metadata/column_units.csv", call. = FALSE)
  suppressWarnings(as.numeric(value)) * VOLUME_TO_M3[[unit]]
}

# fixes to sample fields ----

# metadata/sample_fixes.csv: a value a data owner gave for one sample's field (project_id,
# sample_id, field, value, source). an empty value publishes the field as unknown.
read_sample_fixes <- function(path, project_id) {
  f <- readr::read_csv(path, show_col_types = FALSE, na = character(),
                       col_types = readr::cols(project_id = "i", .default = "c"))
  f[f$project_id == as.integer(project_id), ]
}

# apply the fixes to every row of each sample. `fields` renames the fixes' EcoTaxa field names to
# the table's own (the sample map has lat and lon); `id` is the table's sample-name column. a fix
# for a sample or a field the table does not have stops: a stale fix is a finding, not a no-op.
apply_sample_fixes <- function(d, fixes, id = "sample_id", fields = NULL) {
  if (is.null(fixes) || !nrow(fixes)) return(d)
  for (k in seq_len(nrow(fixes))) {
    fx <- fixes[k, ]
    col <- if (!is.null(fields) && fx$field %in% names(fields)) fields[[fx$field]] else fx$field
    if (!col %in% names(d)) stop("sample fix for a field the table does not have: ", fx$field, call. = FALSE)
    rows <- d[[id]] == fx$sample_id
    if (!any(rows)) stop("sample fix for a sample the table does not have: ", fx$sample_id, call. = FALSE)
    v <- if (nzchar(fx$value)) fx$value else NA
    d[[col]][rows] <- if (is.numeric(d[[col]])) as.numeric(v) else as.character(v)
  }
  d
}

# licenses ----

# the three licenses OBIS accepts, with the text GBIF and the OBIS IPT expect in EML; NCEI accepts
# no non-commercial clause (https://github.com/ioos/ncei-archiving-cookbook/issues/6)
LICENSES <- tibble::tribble(
  ~code,          ~title,                                                       ~url,                                                    ~obis, ~ncei,
  "CC0 1.0",      "Creative Commons Public Domain (CC0 1.0) License",            "http://creativecommons.org/publicdomain/zero/1.0/legalcode", TRUE,  TRUE,
  "CC BY 4.0",    "Creative Commons Attribution (CC-BY) 4.0 License",            "http://creativecommons.org/licenses/by/4.0/legalcode",       TRUE,  TRUE,
  "CC BY-NC 4.0", "Creative Commons Attribution Non Commercial (CC-BY-NC) 4.0 License", "http://creativecommons.org/licenses/by-nc/4.0/legalcode", TRUE, FALSE)

# the LICENSES row for a projects.csv license, NULL when it is not one of them
license_info <- function(license) {
  i <- match(trimws(license), LICENSES$code)
  if (is.na(i)) NULL else as.list(LICENSES[i, ])
}

# a findings row: error when the license is unknown or not accepted by the target (obis | ncei)
license_check <- function(license, target = c("obis", "ncei")) {
  target <- match.arg(target)
  li <- license_info(license)
  ok <- !is.null(li) && isTRUE(li[[target]])
  tibble::tibble(finding = if (ok) "ok" else if (is.null(li)) "license_unknown" else paste0("license_not_accepted_by_", target),
                 level = if (ok) "ok" else "error", detail = as.character(license))
}

# people credited besides the creator, from projects.csv's associated_parties:
# "Name | Organization | role; Name | Organization | role"
parse_parties <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x)))
    return(tibble::tibble(name = character(), organization = character(), role = character()))
  p <- strsplit(trimws(strsplit(x, ";", fixed = TRUE)[[1]]), "|", fixed = TRUE)
  stopifnot("each associated party is 'Name | Organization | role'" = all(lengths(p) == 3))
  tibble::tibble(name = trimws(vapply(p, `[`, "", 1)), organization = trimws(vapply(p, `[`, "", 2)),
                 role = trimws(vapply(p, `[`, "", 3)))
}

# a person's name as EML givenName and surName: "Surname, Given" when the surname has several words
# ("Di Geronimo, Sebastian"), else the last word is the surname ("Ana Carolina Peralta")
split_person_name <- function(x) {
  x <- trimws(x)
  if (grepl(",", x, fixed = TRUE)) {
    p <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    return(list(given = if (length(p) > 1 && nzchar(p[2])) p[2] else NA_character_, sur = p[1]))
  }
  t <- strsplit(x, "\\s+")[[1]]
  list(given = if (length(t) > 1) paste(t[-length(t)], collapse = " ") else NA_character_, sur = t[length(t)])
}
