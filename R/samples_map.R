# sample positions for a project, and the rule for which ones can go on a map ----

#' Samples of a project, from the EcoTaxa API (no login needed for an open project)
#'
#' @return tibble with sample_id (EcoTaxa's numeric id), orig_id, lat, lon
ecotaxa_project_samples <- function(project_id, token = NULL, base = ecotaxa_url()) {
  x <- ecotaxa_get("samples/search", token = token, project_ids = project_id, id_pattern = "%", base = base)
  tibble::tibble(
    sample_id = vapply(x, function(s) as.numeric(s$sampleid), numeric(1)),
    orig_id   = vapply(x, function(s) s$orig_id, character(1)),
    lat       = vapply(x, function(s) if (is.null(s$latitude))  NA_real_ else as.numeric(s$latitude),  numeric(1)),
    lon       = vapply(x, function(s) if (is.null(s$longitude)) NA_real_ else as.numeric(s$longitude), numeric(1)))
}

#' Split samples into those that can be mapped and those that cannot
#'
#' A sample is dropped when a coordinate is missing or when latitude and longitude are both exactly 0, the
#' usual placeholder for an unknown position. A latitude or a longitude of 0 alone is a real place.
#'
#' @param samples data frame with columns lat and lon
#' @return list of two data frames: mapped, and dropped with a reason column
split_mappable_samples <- function(samples) {
  stopifnot(all(c("lat", "lon") %in% names(samples)))
  is_missing <- is.na(samples$lat) | is.na(samples$lon)
  is_zero    <- !is_missing & samples$lat == 0 & samples$lon == 0
  dropped    <- samples[is_missing | is_zero, , drop = FALSE]
  dropped$reason <- ifelse(is_missing[is_missing | is_zero], "missing coordinates", "latitude and longitude both 0")
  list(mapped = samples[!(is_missing | is_zero), , drop = FALSE], dropped = dropped)
}

#' Caption for the sample map, naming every sample left off it
samples_map_caption <- function(mapped, dropped, label) {
  stopifnot(all(c("orig_id", "reason") %in% names(dropped)))
  n_all <- nrow(mapped) + nrow(dropped)
  txt   <- glue::glue("Positions of {nrow(mapped)} of the {n_all} samples in {label}.")
  if (nrow(dropped) == 0) return(as.character(txt))
  why <- paste(glue::glue("{dropped$orig_id} ({dropped$reason} in the source data)"), collapse = "; ")
  as.character(glue::glue("{txt} Dropped from the map because the position is not known: {why}."))
}
