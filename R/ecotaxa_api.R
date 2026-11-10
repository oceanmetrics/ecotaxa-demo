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

# start a general export job (TSV + images) for one project; returns the job id
ecotaxa_export_start <- function(project_id, token, with_images = "first",
                                 with_types_row = TRUE, base = ecotaxa_url()) {
  body <- list(
    filters = structure(list(), names = character()),
    request = list(project_id = as.integer(project_id), with_images = with_images,
                   with_types_row = with_types_row, with_internal_ids = FALSE,
                   only_annotations = FALSE, split_by = "", out_to_ftp = FALSE))
  j <- ecotaxa_req("object_set/export/general", token, base) |>
    httr2::req_body_json(body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  as.integer(j$job_id)
}

# poll a job until it is finished ('F'); stop on error ('E') or a question ('A')
ecotaxa_job_wait <- function(job_id, token, timeout_s = 600, poll_s = 3, base = ecotaxa_url()) {
  t0 <- Sys.time()
  repeat {
    j <- ecotaxa_get(paste0("jobs/", job_id, "/"), token, base = base)
    st <- j$state
    if (identical(st, "F")) return(invisible(j))
    if (st %in% c("E", "A"))
      stop("export job ", job_id, " ended in state ", st, ": ",
           paste(unlist(j$errors), collapse = "; "), call. = FALSE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_s)
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

# the whole export: login, job, wait, download, unzip into dir; returns the TSV paths.
# the system unzip is used because R's internal one fails on zip64 archives (over 4 GB).
ecotaxa_export <- function(project_id, dir, token = ecotaxa_login(), with_images = "first",
                           timeout_s = 600, base = ecotaxa_url()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  job <- ecotaxa_export_start(project_id, token, with_images = with_images, base = base)
  ecotaxa_job_wait(job, token, timeout_s = timeout_s, poll_s = 10, base = base)
  zip <- file.path(dir, paste0("export_project-", project_id, "_job-", job, ".zip"))
  ecotaxa_job_file(job, token, zip, base = base)
  utils::unzip(zip, exdir = dir, unzip = "unzip")
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
