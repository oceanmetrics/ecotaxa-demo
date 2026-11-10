# ecotaxa_api.R: read a project out of an EcoTaxa node through its REST API
#
# the node's API is documented at {ECOTAXA_URL}/api/docs (OpenAPI). every call here is a
# read, except starting an export job, which writes nothing into any project.
# credentials come from env vars (a git-ignored .Renviron): ECOTAXA_URL, ECOTAXA_USER,
# ECOTAXA_PASSWORD, or ECOTAXA_TOKEN alone (a JWT from POST /api/login, handed to the
# process so no password sits on the machine). nothing in this file prints a password or token.

ecotaxa_url <- function() {
  u <- Sys.getenv("ECOTAXA_URL", "https://ecotaxa.oceanmetrics.io")
  sub("/+$", "", u)
}

# the project the notebooks run on: ECOTAXA_PROJECT_ID, else the demo project 2
ecotaxa_project_id <- function(default = 2L) {
  v <- Sys.getenv("ECOTAXA_PROJECT_ID", as.character(default))
  id <- suppressWarnings(as.integer(v))
  stopifnot("ECOTAXA_PROJECT_ID must be a whole number" = length(id) == 1 && !is.na(id) && id > 0)
  id
}

# TRUE when a login is configured in the environment (a token, or a user and password)
ecotaxa_has_login <- function()
  nzchar(Sys.getenv("ECOTAXA_TOKEN")) ||
    (nzchar(Sys.getenv("ECOTAXA_USER")) && nzchar(Sys.getenv("ECOTAXA_PASSWORD")))

# a bare request to {base}/api/{path}; token adds the bearer header
ecotaxa_req <- function(path, token = NULL, base = ecotaxa_url()) {
  r <- httr2::request(paste0(base, "/api/", sub("^/+", "", path))) |>
    httr2::req_user_agent("oceanmetrics/ecotaxa-demo (R httr2)") |>
    httr2::req_timeout(120)
  if (!is.null(token)) r <- httr2::req_auth_bearer_token(r, token)
  r
}

# POST /api/login returns a JWT used as a bearer token on later calls; a token already in
# ECOTAXA_TOKEN is returned as is
ecotaxa_login <- function(user = Sys.getenv("ECOTAXA_USER"),
                          password = Sys.getenv("ECOTAXA_PASSWORD"),
                          base = ecotaxa_url(), token = Sys.getenv("ECOTAXA_TOKEN")) {
  if (nzchar(token)) return(invisible(token))
  stopifnot("ECOTAXA_USER and ECOTAXA_PASSWORD must be set" = nzchar(user) && nzchar(password))
  tok <- ecotaxa_req("login", base = base) |>
    httr2::req_body_json(list(username = user, password = password)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  stopifnot("login did not return a token" = is.character(tok) && nzchar(tok))
  invisible(tok)
}

# GET json helper
ecotaxa_get <- function(path, token = NULL, ..., base = ecotaxa_url()) {
  ecotaxa_req(path, token, base) |>
    httr2::req_url_query(...) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# the public project list (no login needed on this node); one row per project
ecotaxa_projects <- function(token = NULL, base = ecotaxa_url()) {
  p <- ecotaxa_get("projects/search", token, also_others = "true", base = base)
  dplyr::bind_rows(lapply(p, function(x) tibble::as_tibble(
    lapply(x, function(v) if (is.null(v)) NA else v))))
}

# full project record (needs a login with read rights on the project)
ecotaxa_project <- function(project_id, token, base = ecotaxa_url())
  ecotaxa_get(paste0("projects/", project_id), token, for_managing = "false", base = base)

# the taxonomy rows for a set of EcoTaxa category ids (public): id, name, display_name,
# type (P = phylogenetic, M = morphological), rank, aphia_id, lineage, id_lineage
ecotaxa_taxa <- function(ids, base = ecotaxa_url()) {
  ids <- unique(as.integer(ids))
  if (!length(ids)) return(ecotaxa_taxa_empty())
  out <- list()
  for (chunk in split(ids, ceiling(seq_along(ids) / 100))) {
    t <- ecotaxa_get("taxon_set/query", ids = paste(chunk, collapse = ","), base = base)
    out <- c(out, lapply(t, function(x) tibble::tibble(
      ecotaxa_id   = as.integer(x$id),
      name         = x$name,
      display_name = x$display_name,
      type         = x$type,
      rank         = if (is.null(x$rank)) NA_character_ else x$rank,
      aphia_id     = if (is.null(x$aphia_id)) NA_integer_ else as.integer(x$aphia_id),
      lineage      = paste(rev(unlist(x$lineage)), collapse = " > "),
      id_lineage   = paste(rev(unlist(x$id_lineage)), collapse = ","))))
  }
  dplyr::bind_rows(out)
}
ecotaxa_taxa_empty <- function() tibble::tibble(
  ecotaxa_id = integer(), name = character(), display_name = character(), type = character(),
  rank = character(), aphia_id = integer(), lineage = character(), id_lineage = character())

# the category ids a project uses and its validation counts (public):
# used_taxa, nb_validated, nb_predicted, nb_dubious, nb_unclassified
ecotaxa_project_taxo_stats <- function(project_id, base = ecotaxa_url()) {
  s <- ecotaxa_get("project_set/taxo_stats", ids = as.character(project_id), base = base)[[1]]
  list(used_taxa = as.integer(unlist(s$used_taxa)), nb_validated = s$nb_validated,
       nb_predicted = s$nb_predicted, nb_dubious = s$nb_dubious,
       nb_unclassified = s$nb_unclassified)
}

# the body of POST /api/object_set/export/general (GeneralExportReq in the node's OpenAPI):
# split_by is one of sample, acquisition, taxon, none; with_images one of all, first, none.
# samples (EcoTaxa's numeric sample ids) restricts the export through the samples filter.
EXPORT_SPLIT_BY   <- c("sample", "acquisition", "taxon", "none")
EXPORT_WITH_IMAGES <- c("all", "first", "none")
ecotaxa_export_body <- function(project_id, with_images = "first", with_types_row = TRUE, split_by = "sample",
                                samples = NULL) {
  stopifnot(with_images %in% EXPORT_WITH_IMAGES, split_by %in% EXPORT_SPLIT_BY)
  filters <- structure(list(), names = character())
  if (length(samples)) filters$samples <- paste(format(as.numeric(samples), scientific = FALSE, trim = TRUE),
                                                collapse = ",")
  list(
    filters = filters,
    request = list(project_id = as.integer(project_id), with_images = with_images,
                   with_types_row = with_types_row, with_internal_ids = FALSE,
                   only_annotations = FALSE, split_by = split_by, out_to_ftp = FALSE))
}

# start a general export job (TSV + images) for one project, or some of its samples; returns the job id
ecotaxa_export_start <- function(project_id, token, with_images = "first",
                                 with_types_row = TRUE, samples = NULL, base = ecotaxa_url()) {
  body <- ecotaxa_export_body(project_id, with_images, with_types_row, samples = samples)
  j <- ecotaxa_req("object_set/export/general", token, base) |>
    httr2::req_body_json(body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  as.integer(j$job_id)
}

# poll a job until it is finished ('F'); stop on error ('E'), a question ('A'), the overall
# timeout, or a stall: a job whose process died (the node's back end was OOM-killed on a whole
# project export, 2026-11-10) stays 'R' at the same progress forever
ecotaxa_job_wait <- function(job_id, token, timeout_s = 600, poll_s = 3, stall_s = 1200, base = ecotaxa_url()) {
  t0 <- Sys.time(); last <- NULL; t_last <- t0
  repeat {
    j <- ecotaxa_get(paste0("jobs/", job_id, "/"), token, base = base)
    st <- j$state
    if (identical(st, "F")) return(invisible(j))
    if (st %in% c("E", "A"))
      stop("export job ", job_id, " ended in state ", st, ": ",
           paste(unlist(j$errors), collapse = "; "), call. = FALSE)
    now <- Sys.time(); prog <- paste(j$progress_pct, j$progress_msg)
    if (!identical(prog, last)) { last <- prog; t_last <- now }
    if (as.numeric(difftime(now, t_last, units = "secs")) > stall_s)
      stop("export job ", job_id, " stalled at '", prog, "' for ", stall_s, " s", call. = FALSE)
    if (as.numeric(difftime(now, t0, units = "secs")) > timeout_s)
      stop("export job ", job_id, " still in state ", st, " after ", timeout_s, " s", call. = FALSE)
    Sys.sleep(poll_s)
  }
}

# download the finished job's zip to path. a project with images runs to gigabytes, so the
# 120 s request timeout is lifted to timeout_s for this one call
ecotaxa_job_file <- function(job_id, token, path, timeout_s = 6 * 3600, base = ecotaxa_url()) {
  ecotaxa_req(paste0("jobs/", job_id, "/file"), token, base) |>
    httr2::req_timeout(timeout_s) |>
    httr2::req_perform(path = path)
  invisible(path)
}

# remove a finished job and its files from the node (DELETE /api/jobs/{id})
ecotaxa_job_delete <- function(job_id, token, base = ecotaxa_url()) {
  ecotaxa_req(paste0("jobs/", job_id), token, base) |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()
  invisible(job_id)
}

# objects per sample (all statuses), from GET /api/sample_set/taxo_stats (public)
ecotaxa_sample_counts <- function(sample_ids, base = ecotaxa_url()) {
  ids <- unique(as.numeric(sample_ids))
  out <- list()
  for (chunk in split(ids, ceiling(seq_along(ids) / 100))) {
    s <- ecotaxa_get("sample_set/taxo_stats", base = base,
                     sample_ids = paste(format(chunk, scientific = FALSE, trim = TRUE), collapse = ","))
    out <- c(out, lapply(s, function(x) tibble::tibble(
      sample_id = as.numeric(x$sample_id),
      n_objects = sum(unlist(x[c("nb_unclassified", "nb_validated", "nb_dubious", "nb_predicted")])))))
  }
  dplyr::bind_rows(out)
}

# split samples into export batches of at most max_objects, keeping their order; a sample
# larger than max_objects goes alone. pure, so it is tested without a node
ecotaxa_sample_batches <- function(sample_id, n_objects, max_objects = 40000) {
  stopifnot(length(sample_id) == length(n_objects), max_objects > 0, !anyNA(n_objects))
  batch <- integer(length(sample_id)); b <- 1L; tot <- 0
  for (i in seq_along(sample_id)) {
    if (tot > 0 && tot + n_objects[i] > max_objects) { b <- b + 1L; tot <- 0 }
    batch[i] <- b; tot <- tot + n_objects[i]
  }
  unname(split(sample_id, batch))
}

# the whole export: login, then one job per batch of samples (so a large project does not have to
# fit in the node's memory at once), wait, download, unzip into dir, delete the job on the node.
# a batch already unzipped (its marker under dir/batches/) is skipped, so a run that stops resumes.
# the system unzip is used because R's internal one fails on zip64 archives (over 4 GB).
ecotaxa_export <- function(project_id, dir, token = ecotaxa_login(), with_images = "first",
                           timeout_s = 600, max_objects = 40000, base = ecotaxa_url()) {
  dir.create(file.path(dir, "batches"), recursive = TRUE, showWarnings = FALSE)
  smp <- ecotaxa_project_samples(project_id, token = token, base = base)
  cnt <- ecotaxa_sample_counts(smp$sample_id, base = base)
  smp <- dplyr::left_join(smp, cnt, by = "sample_id") |> dplyr::arrange(orig_id)
  stopifnot("a sample has no object count" = !anyNA(smp$n_objects))
  batches <- ecotaxa_sample_batches(smp$sample_id, smp$n_objects, max_objects)
  message(nrow(smp), " samples, ", sum(smp$n_objects), " objects, ", length(batches), " export batches")
  for (k in seq_along(batches)) {
    mark <- file.path(dir, "batches", sprintf("batch-%03d.json", k))
    ids  <- batches[[k]]
    if (file.exists(mark) && setequal(jsonlite::fromJSON(mark)$samples, ids)) next
    job <- ecotaxa_export_start(project_id, token, with_images = with_images, samples = ids, base = base)
    ecotaxa_job_wait(job, token, timeout_s = timeout_s, poll_s = 5, base = base)
    zip <- file.path(dir, "batches", sprintf("batch-%03d_job-%d.zip", k, job))
    ecotaxa_job_file(job, token, zip, base = base)
    utils::unzip(zip, exdir = dir, unzip = "unzip")
    ecotaxa_job_delete(job, token, base = base)
    unlink(zip)
    writeLines(jsonlite::toJSON(list(batch = k, job = job, samples = ids,
                                     n_objects = sum(smp$n_objects[smp$sample_id %in% ids])),
                                auto_unbox = TRUE, digits = NA), mark)
    message(sprintf("batch %d/%d: job %d, %d samples", k, length(batches), job, length(ids)))
  }
  ecotaxa_export_tsvs(dir)
}

# the TSVs of an export already unzipped under dir (empty when there is none)
ecotaxa_export_tsvs <- function(dir)
  list.files(dir, pattern = "\\.tsv$", recursive = TRUE, full.names = TRUE)

# read one EcoTaxa TSV (import or export format). the optional second row holds the
# column types ([t] text, [f] float) and is used to type the columns, then dropped.
read_ecotaxa_tsv <- function(path) {
  hdr <- readr::read_tsv(path, n_max = 1, col_names = FALSE, col_types = readr::cols(.default = "c"),
                         show_col_types = FALSE, progress = FALSE)
  nm <- as.character(hdr[1, ])
  row2 <- readr::read_tsv(path, skip = 1, n_max = 1, col_names = FALSE,
                          col_types = readr::cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
  has_types <- nrow(row2) == 1 && all(grepl("^\\[[tf]\\]$", as.character(row2[1, ])))
  d <- readr::read_tsv(path, skip = if (has_types) 2 else 1, col_names = nm,
                       col_types = readr::cols(.default = "c"), show_col_types = FALSE,
                       progress = FALSE, na = c("", "NA"))
  if (has_types) {
    types <- as.character(row2[1, ])
    for (k in which(types == "[f]")) d[[k]] <- suppressWarnings(as.numeric(d[[k]]))
  }
  d$source_file <- basename(path)
  d
}

# read every TSV of an export directory into one objects table
read_ecotaxa_export <- function(paths) {
  stopifnot(length(paths) > 0)
  dplyr::bind_rows(lapply(paths, read_ecotaxa_tsv))
}

# EcoTaxa's own WoRMS mapping (since the 2026 major version the taxonomy IS WoRMS): for a
# set of category ids, GET /api/taxon_set/wormsification returns the taxa reached by
# "phylo -> itself when it has an aphia_id, else its rename target; morpho -> nearest
# non-morpho ancestor" (py/BO/WoRMSification.py). a category with no solution is absent
# from the answer. used as a cross-check of taxa_decide(), not as the rule.
ecotaxa_wormsification <- function(ids, base = ecotaxa_url()) {
  ids <- unique(as.integer(ids))
  if (!length(ids)) return(tibble::tibble(ecotaxa_id = integer(), maps_to_id = integer(),
                                          maps_to_name = character(), maps_to_aphia_id = integer()))
  w <- ecotaxa_get("taxon_set/wormsification", ids = paste(ids, collapse = ","), base = base)
  parse_wormsification(w, ids)
}
# the answer is keyed by the id of the TARGET taxon; a requested id maps to the target whose
# id equals it (phylo) or whose children/lineage covers it. the API gives no explicit
# from -> to pairs, so the mapping is recovered through id_lineage: a requested morpho id maps
# to the first returned non-morpho taxon in its lineage, a phylo id to itself when returned.
parse_wormsification <- function(w, ids, taxa = NULL) {
  tg <- lapply(w, function(x) tibble::tibble(
    id = as.integer(x$id), name = x$display_name, type = x$type,
    aphia_id = if (is.null(x$aphia_id)) NA_integer_ else as.integer(x$aphia_id)))
  tg <- dplyr::bind_rows(tg)
  if (is.null(taxa)) taxa <- ecotaxa_taxa(ids)
  out <- lapply(ids, function(i) {
    t <- taxa[taxa$ecotaxa_id == i, ]
    if (!nrow(t)) return(NULL)
    lin <- as.integer(strsplit(t$id_lineage, ",", fixed = TRUE)[[1]])
    # lineage is root -> self; walk from self upwards
    hit <- NA_integer_
    for (a in rev(lin)) if (a %in% tg$id && (a == i || tg$type[tg$id == a] != "M")) { hit <- a; break }
    tibble::tibble(ecotaxa_id = i,
                   maps_to_id = hit,
                   maps_to_name = if (is.na(hit)) NA_character_ else tg$name[tg$id == hit],
                   maps_to_aphia_id = if (is.na(hit)) NA_integer_ else tg$aphia_id[tg$id == hit])
  })
  dplyr::bind_rows(out)
}
