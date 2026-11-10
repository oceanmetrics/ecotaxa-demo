# taxa.R: resolve EcoTaxa categories to WoRMS, as a reviewable lookup table
#
# since its 2026 major version EcoTaxa's taxonomy IS WoRMS: a phylogenetic category (type P)
# carries an aphia_id and a WoRMS lineage; a morphological one (type M: detritus, egg, t001,
# "other", ...) carries none. the TSV export carries the category name, id and lineage, not the
# AphiaID, so each distinct category is resolved once through the node's taxonomy. the rules
# below decide per category and the result is a CSV a taxonomist reviews
# (metadata/taxa_lookup_project-<id>.csv). they differ from EcoTaxa's own wormsification in one place: a
# morphological category whose nearest living ancestor is only Biota is excluded here, where
# EcoTaxa's DwC export publishes it as Biota. nothing is guessed silently: a category that
# resolves to nothing is `unresolved`, and the OBIS notebook stops on it.

# roots of the EcoTaxa tree that can never be an occurrence
TAXA_NONLIVING_ROOTS <- c("not-living", "temporary")
# an ancestor at or above this rank is not worth publishing as an occurrence
TAXA_TOO_HIGH_RANKS <- c("Superdomain", "Domain")

# the pure decision, no network. taxa: the frame from ecotaxa_taxa(). returns one row per
# category with `decision` in aphia | name_match | ancestor | excluded, and `note`.
taxa_decide <- function(taxa) {
  stopifnot(all(c("ecotaxa_id", "name", "type", "aphia_id", "lineage", "id_lineage") %in% names(taxa)))
  by_id <- stats::setNames(split(taxa, seq_len(nrow(taxa))), taxa$ecotaxa_id)
  rows <- lapply(seq_len(nrow(taxa)), function(i) {
    t <- taxa[i, ]
    root <- strsplit(t$lineage, " > ", fixed = TRUE)[[1]][1]
    if (root %in% TAXA_NONLIVING_ROOTS)
      return(c(decision = "excluded", aphia_id = NA, note = paste0("non-living root: ", root)))
    if (!is.na(t$aphia_id))
      return(c(decision = "aphia", aphia_id = t$aphia_id, note = "aphia_id from the EcoTaxa taxonomy"))
    if (identical(t$type, "P"))
      return(c(decision = "name_match", aphia_id = NA, note = "no aphia_id on the node; exact name match in WoRMS"))
    # morphological under Biota: nearest ancestor with an aphia_id, if it is worth anything
    anc_ids <- rev(as.integer(strsplit(t$id_lineage, ",", fixed = TRUE)[[1]]))
    anc_ids <- anc_ids[anc_ids != t$ecotaxa_id]
    for (a in anc_ids) {
      ar <- by_id[[as.character(a)]]
      if (is.null(ar) || is.na(ar$aphia_id)) next
      if (ar$rank %in% TAXA_TOO_HIGH_RANKS)
        return(c(decision = "excluded", aphia_id = NA,
                 note = paste0("morphological; nearest living ancestor is ", ar$name, " (", ar$rank, ")")))
      return(c(decision = "ancestor", aphia_id = ar$aphia_id,
               note = paste0("morphological; published as ancestor ", ar$name, " (", ar$rank, ")")))
    }
    c(decision = "excluded", aphia_id = NA,
      note = "morphological; no ancestor with an aphia_id in the lookup (pass the ancestors too)")
  })
  dec <- do.call(rbind, rows)
  out <- taxa |>
    dplyr::select(ecotaxa_id, ecotaxa_name = display_name, type, lineage) |>
    dplyr::mutate(decision = unname(dec[, "decision"]),
                  aphia_id = suppressWarnings(as.integer(dec[, "aphia_id"])),
                  note     = unname(dec[, "note"]))
  out
}

# fetch a WoRMS record by AphiaID with worrms; returns the fields the lookup keeps
worms_by_id <- function(aphia_id) {
  r <- worrms::wm_record(as.integer(aphia_id))
  worms_row(r)
}
# exact-name match in WoRMS; NULL when there is no exact (case-insensitive) hit
worms_by_name <- function(name) {
  r <- tryCatch(worrms::wm_records_name(name, fuzzy = FALSE), error = function(e) NULL)
  if (is.null(r) || !nrow(r)) return(NULL)
  r <- r[tolower(r$scientificname) == tolower(name), , drop = FALSE]
  if (!nrow(r)) return(NULL)
  # prefer an accepted record
  r <- r[order(r$status != "accepted"), , drop = FALSE]
  worms_row(as.list(r[1, ]))
}
worms_row <- function(r) {
  g <- function(k) { v <- r[[k]]; if (is.null(v) || length(v) == 0 || is.na(v)) NA else v }
  tibble::tibble(
    aphia_id        = as.integer(g("AphiaID")),
    scientific_name = as.character(g("scientificname")),
    status          = as.character(g("status")),
    valid_aphia_id  = as.integer(g("valid_AphiaID")),
    valid_name      = as.character(g("valid_name")),
    rank            = as.character(g("rank")),
    kingdom = as.character(g("kingdom")), phylum = as.character(g("phylum")),
    class = as.character(g("class")), order = as.character(g("order")),
    family = as.character(g("family")), genus = as.character(g("genus")),
    lsid            = as.character(g("lsid")))
}

# fill the decided lookup from WoRMS. `by_id` and `by_name` are injectable so tests run
# without the network. a name_match with no hit becomes `unresolved`.
taxa_worms_fill <- function(decided, by_id = worms_by_id, by_name = worms_by_name) {
  empty <- worms_row(list())[0, ]
  recs <- lapply(seq_len(nrow(decided)), function(i) {
    d <- decided[i, ]
    w <- switch(d$decision,
      aphia = , ancestor = by_id(d$aphia_id),
      name_match = by_name(d$ecotaxa_name),
      NULL)
    if (is.null(w)) return(empty[rep(1, 0), ])
    w
  })
  n <- vapply(recs, nrow, 1L)
  filled <- dplyr::bind_rows(lapply(seq_along(recs), function(i)
    if (n[i]) dplyr::mutate(recs[[i]], .row = i) else tibble::tibble(.row = i)))
  out <- decided |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::select(-aphia_id) |>
    dplyr::left_join(filled, by = ".row") |>
    dplyr::select(-.row) |>
    dplyr::mutate(
      decision = ifelse(decision == "name_match" & is.na(aphia_id), "unresolved", decision),
      note     = ifelse(decision == "unresolved", "no exact WoRMS match for the category name; review", note),
      # an unaccepted name is published under its valid name and id
      aphia_id        = dplyr::coalesce(valid_aphia_id, aphia_id),
      scientific_name = dplyr::coalesce(valid_name, scientific_name),
      lsid            = ifelse(is.na(aphia_id), NA_character_,
                               paste0("urn:lsid:marinespecies.org:taxname:", aphia_id)))
  out
}

# the full lookup for a set of taxa, then write/read it as the reviewable CSV
build_taxa_lookup <- function(taxa, ...) taxa_worms_fill(taxa_decide(taxa), ...)
write_taxa_lookup <- function(lookup, path) readr::write_csv(lookup, path, na = "")
read_taxa_lookup <- function(path) readr::read_csv(path, show_col_types = FALSE,
  col_types = readr::cols(ecotaxa_id = "i", aphia_id = "i", valid_aphia_id = "i", .default = "c"))

# the gate the OBIS notebook applies: every category used by the objects must be decided
assert_taxa_resolved <- function(objects, lookup) {
  used <- unique(as.integer(objects$object_annotation_category_id))
  miss <- setdiff(used, lookup$ecotaxa_id)
  if (length(miss)) stop("categories used by the objects but absent from the lookup: ",
                         paste(miss, collapse = ", "), call. = FALSE)
  unres <- lookup$ecotaxa_name[lookup$ecotaxa_id %in% used & lookup$decision == "unresolved"]
  if (length(unres)) stop("unresolved categories (review metadata/taxa_lookup_project-<id>.csv): ",
                          paste(unres, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
