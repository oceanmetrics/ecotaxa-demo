# dwc.R: EcoTaxa objects -> Darwin Core Event core + Occurrence + eMoF -> meta.xml -> DwC-A
#
# borrowed in structure from calcofi4db R/dwc.R (calcofi.io/calcofi4db): the term map drives
# meta.xml so a column without a term is an error, archives are deterministic over their data,
# and vocabulary ids are filled only on an exact match, never invented.

DWC_NS   <- "http://rs.tdwg.org/dwc/terms/"
DWC_TERM_OVERRIDE <- c(
  measurementTypeID  = "http://rs.iobis.org/obis/terms/measurementTypeID",
  measurementValueID = "http://rs.iobis.org/obis/terms/measurementValueID",
  measurementUnitID  = "http://rs.iobis.org/obis/terms/measurementUnitID",
  occurrenceID       = "http://rs.tdwg.org/dwc/terms/occurrenceID")
DWC_ROW_TYPES <- list(
  event = list(file = "event.csv", id = "eventID", rowType = "http://rs.tdwg.org/dwc/terms/Event"),
  occurrence = list(file = "occurrence.csv", id = "occurrenceID", coreid = "eventID",
                    rowType = "http://rs.tdwg.org/dwc/terms/Occurrence"),
  emof = list(file = "extendedMeasurementOrFact.csv", id = "measurementID", coreid = "eventID",
              rowType = "http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"))

dwc_term_map <- function(cols) {
  out <- stats::setNames(paste0(DWC_NS, cols), cols)
  ov <- intersect(names(DWC_TERM_OVERRIDE), cols)
  out[ov] <- DWC_TERM_OVERRIDE[ov]
  out
}

# ISO 8601 UTC from EcoTaxa's object_date (YYYYMMDD) and object_time (HHMMSS)
ecotaxa_datetime <- function(date, time) {
  d <- as.character(date); t <- as.character(time)
  ok <- !is.na(d) & grepl("^\\d{8}$", d)
  tt <- ifelse(!is.na(t) & grepl("^\\d{6}$", t), t, NA)
  out <- rep(NA_character_, length(d))
  out[ok & !is.na(tt)] <- sprintf("%s-%s-%sT%s:%s:%sZ",
    substr(d, 1, 4), substr(d, 5, 6), substr(d, 7, 8),
    substr(tt, 1, 2), substr(tt, 3, 4), substr(tt, 5, 6))[ok & !is.na(tt)]
  out[ok & is.na(tt)] <- sprintf("%s-%s-%s", substr(d, 1, 4), substr(d, 5, 6), substr(d, 7, 8))[ok & is.na(tt)]
  out
}

# one unique non-NA value per group, else NA (a sample with two positions gets none, not a mean)
.one <- function(x) { u <- unique(x[!is.na(x)]); if (length(u) == 1) u else x[NA_integer_][1] }
.col <- function(d, nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))

# the Event core: one event per sample; a child event per acquisition when a sample has
# more than one. prefix (the project key) keeps ids unique across projects.
dwc_event <- function(objects, prefix, instrument_url = NA_character_) {
  stopifnot(all(c("sample_id", "object_lat", "object_lon", "object_date") %in% names(objects)))
  o <- objects |>
    dplyr::mutate(acq_id = .col(objects, "acq_id"), gear = .col(objects, "sample_samplinggear"),
                  instrument = .col(objects, "acq_instrument"),
                  tot_vol = suppressWarnings(as.numeric(.col(objects, "sample_tot_vol"))),
                  dt = ecotaxa_datetime(object_date, .col(objects, "object_time")),
                  depth_min = suppressWarnings(as.numeric(.col(objects, "object_depth_min"))),
                  depth_max = suppressWarnings(as.numeric(.col(objects, "object_depth_max"))))
  per_sample <- o |>
    dplyr::group_by(sample_id) |>
    dplyr::summarise(
      n_acq = dplyr::n_distinct(acq_id, na.rm = TRUE),
      eventDate = if (all(is.na(dt))) NA_character_ else
        if (min(dt, na.rm = TRUE) == max(dt, na.rm = TRUE)) min(dt, na.rm = TRUE) else
          paste0(min(dt, na.rm = TRUE), "/", max(dt, na.rm = TRUE)),
      decimalLatitude = .one(as.numeric(object_lat)), decimalLongitude = .one(as.numeric(object_lon)),
      minimumDepthInMeters = suppressWarnings(min(depth_min, na.rm = TRUE)),
      maximumDepthInMeters = suppressWarnings(max(depth_max, na.rm = TRUE)),
      samplingProtocol = .one(paste0(instrument, ifelse(is.na(gear), "", paste0("; ", gear)))),
      sampleSizeValue = .one(tot_vol), .groups = "drop") |>
    dplyr::mutate(
      minimumDepthInMeters = ifelse(is.finite(minimumDepthInMeters), minimumDepthInMeters, NA),
      maximumDepthInMeters = ifelse(is.finite(maximumDepthInMeters), maximumDepthInMeters, NA),
      sampleSizeUnit = ifelse(is.na(sampleSizeValue), NA_character_, "m3"))
  ev <- per_sample |>
    dplyr::transmute(eventID = paste0(prefix, ":", sample_id), parentEventID = NA_character_,
                     eventType = "sample", eventDate, decimalLatitude, decimalLongitude,
                     geodeticDatum = "EPSG:4326", minimumDepthInMeters, maximumDepthInMeters,
                     samplingProtocol, sampleSizeValue, sampleSizeUnit, datasetID = prefix)
  multi <- per_sample$sample_id[per_sample$n_acq > 1]
  if (length(multi)) {
    acq <- o |>
      dplyr::filter(sample_id %in% multi) |>
      dplyr::group_by(sample_id, acq_id) |>
      dplyr::summarise(eventDate = if (all(is.na(dt))) NA_character_ else min(dt, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::transmute(eventID = paste0(prefix, ":", sample_id, ":", acq_id),
                       parentEventID = paste0(prefix, ":", sample_id), eventType = "acquisition",
                       eventDate, decimalLatitude = NA_real_, decimalLongitude = NA_real_,
                       geodeticDatum = NA_character_, minimumDepthInMeters = NA_real_,
                       maximumDepthInMeters = NA_real_, samplingProtocol = NA_character_,
                       sampleSizeValue = NA_real_, sampleSizeUnit = NA_character_, datasetID = prefix)
    ev <- dplyr::bind_rows(ev, acq)
  }
  .drop_all_na(ev, keep = c("eventID", "parentEventID"))
}

# the event an object belongs to: its acquisition event when the sample has several, else the sample
.object_event <- function(objects, event, prefix) {
  acq_ev <- event$eventID[event$eventType == "acquisition"]
  acq_id <- .col(objects, "acq_id")
  cand <- paste0(prefix, ":", objects$sample_id, ":", acq_id)
  ifelse(cand %in% acq_ev, cand, paste0(prefix, ":", objects$sample_id))
}

# Occurrence: objects aggregated per event x taxon x verification status. only `validated`
# objects by default; predicted (the classifier's guess) is an opt-in, dubious is never published.
dwc_occurrence <- function(objects, event, lookup, prefix, include_predicted = FALSE) {
  assert_taxa_resolved(objects, lookup)
  statuses <- c("validated", if (include_predicted) "predicted")
  lk <- lookup |>
    dplyr::filter(decision %in% c("aphia", "name_match", "ancestor")) |>
    dplyr::select(ecotaxa_id, ecotaxa_name, aphia_id, scientific_name, rank, kingdom, phylum,
                  class, order, family, genus, lsid)
  o <- objects |>
    dplyr::mutate(eventID = .object_event(objects, event, prefix),
                  status = tolower(.col(objects, "object_annotation_status")),
                  cat_id = as.integer(object_annotation_category_id),
                  annotator = .col(objects, "object_annotation_person_name"),
                  annot_date = .col(objects, "object_annotation_date"),
                  esd = suppressWarnings(as.numeric(.col(objects, "object_esd")))) |>
    dplyr::filter(status %in% statuses) |>
    dplyr::inner_join(lk, by = c("cat_id" = "ecotaxa_id"))
  if (!nrow(o)) return(.occurrence_empty())
  o |>
    dplyr::group_by(eventID, aphia_id, status) |>
    dplyr::summarise(
      scientificName = dplyr::first(scientific_name), scientificNameID = dplyr::first(lsid),
      taxonRank = dplyr::first(rank), kingdom = dplyr::first(kingdom), phylum = dplyr::first(phylum),
      class = dplyr::first(class), order = dplyr::first(order), family = dplyr::first(family),
      genus = dplyr::first(genus),
      organismQuantity = dplyr::n(),
      identifiedBy = paste(sort(unique(stats::na.omit(annotator))), collapse = " | "),
      dateIdentified = if (all(is.na(annot_date))) NA_character_ else
        ecotaxa_datetime(max(annot_date, na.rm = TRUE), NA),
      identificationRemarks = paste0("EcoTaxa category: ",
        paste(sort(unique(paste0(ecotaxa_name, " (", cat_id, ")"))), collapse = "; ")),
      .groups = "drop") |>
    dplyr::transmute(
      occurrenceID = paste0(eventID, ":", aphia_id, ":", status), eventID,
      basisOfRecord = "MachineObservation", occurrenceStatus = "present",
      scientificName, scientificNameID, taxonID = paste0("worms:", aphia_id), taxonRank,
      kingdom, phylum, class, order, family, genus,
      organismQuantity, organismQuantityType = "individuals",
      identificationVerificationStatus = status,
      identifiedBy = ifelse(nzchar(identifiedBy), identifiedBy, NA_character_),
      dateIdentified, identificationRemarks) |>
    dplyr::arrange(occurrenceID) |>
    .drop_all_na(keep = c("occurrenceID", "eventID"))
}
.occurrence_empty <- function() tibble::tibble(occurrenceID = character(), eventID = character(),
  basisOfRecord = character(), occurrenceStatus = character(), scientificName = character(),
  scientificNameID = character(), taxonID = character(), taxonRank = character(),
  organismQuantity = integer(), organismQuantityType = character(),
  identificationVerificationStatus = character(), identificationRemarks = character())

# the measurement registry: measurementType -> NERC ids, filled only on an exact concept
read_measurement_registry <- function(path) readr::read_csv(path, show_col_types = FALSE,
  col_types = readr::cols(.default = "c"))

# eMoF: event grain (instrument, sample volume) and occurrence grain (count per occurrence,
# concentration when a sample volume exists, mean ESD in pixels as exported)
dwc_emof <- function(objects, event, occurrence, registry, prefix, instrument_url = NA_character_) {
  reg_of <- function(type, col) {
    v <- registry[[col]][match(type, registry$measurement_type)]
    ifelse(is.na(v) | v == "", NA_character_, v)
  }
  row <- function(eventID, occurrenceID, type, value, unit = NULL, value_id = NA_character_) tibble::tibble(
    eventID = eventID, occurrenceID = occurrenceID,
    measurementID = paste0(ifelse(is.na(occurrenceID), eventID, occurrenceID), ":", type),
    measurementType = type, measurementTypeID = reg_of(type, "nerc_p01"),
    measurementValue = as.character(value), measurementValueID = value_id,
    measurementUnit = if (is.null(unit)) reg_of(type, "unit") else unit,
    measurementUnitID = reg_of(type, "nerc_p06"))
  out <- list()
  # event grain
  ev <- event[event$eventType == "sample", ]
  inst <- objects |>
    dplyr::mutate(eventID = paste0(prefix, ":", sample_id), instrument = .col(objects, "acq_instrument")) |>
    dplyr::group_by(eventID) |> dplyr::summarise(instrument = .one(instrument), .groups = "drop") |>
    dplyr::filter(!is.na(instrument))
  if (nrow(inst))
    out$instrument <- row(inst$eventID, NA_character_, "sampling instrument", inst$instrument, unit = NA_character_,
                          value_id = rep(instrument_url, nrow(inst)))
  if ("sampleSizeValue" %in% names(ev)) {
    vol <- ev[!is.na(ev$sampleSizeValue), ]
    if (nrow(vol)) out$volume <- row(vol$eventID, NA_character_, "sample volume", vol$sampleSizeValue)
  }
  # occurrence grain
  if (nrow(occurrence)) {
    out$count <- row(occurrence$eventID, occurrence$occurrenceID, "object count", occurrence$organismQuantity)
    if ("sampleSizeValue" %in% names(ev)) {
      v <- ev$sampleSizeValue[match(occurrence$eventID, ev$eventID)]
      ok <- !is.na(v) & v > 0
      if (any(ok)) out$conc <- row(occurrence$eventID[ok], occurrence$occurrenceID[ok], "concentration",
                                   signif(occurrence$organismQuantity[ok] / v[ok], 6))
    }
    if ("object_esd" %in% names(objects)) {
      esd <- tibble::tibble(eventID = .object_event(objects, event, prefix),
                            status  = tolower(.col(objects, "object_annotation_status")),
                            cat_id  = as.integer(objects$object_annotation_category_id),
                            esd     = suppressWarnings(as.numeric(objects$object_esd))) |>
        dplyr::filter(!is.na(esd))
      # mean esd per occurrence through the category ids named in identificationRemarks: one
      # (occurrence, category) row per id, joined to the objects, so a million objects is a
      # join and not one scan per occurrence
      ids <- regmatches(occurrence$identificationRemarks, gregexpr("\\((\\d+)\\)", occurrence$identificationRemarks))
      key <- tibble::tibble(occurrenceID = rep(occurrence$occurrenceID, lengths(ids)),
                            eventID      = rep(occurrence$eventID, lengths(ids)),
                            status       = rep(occurrence$identificationVerificationStatus, lengths(ids)),
                            cat_id       = as.integer(gsub("[()]", "", unlist(ids))))
      m <- key |>
        dplyr::inner_join(esd, by = c("eventID", "status", "cat_id")) |>
        dplyr::group_by(occurrenceID) |>
        dplyr::summarise(esd = mean(esd), .groups = "drop")
      m <- m[match(occurrence$occurrenceID, m$occurrenceID), ]
      ok <- !is.na(m$esd)
      if (any(ok)) out$esd <- row(occurrence$eventID[ok], occurrence$occurrenceID[ok],
                                  "mean equivalent spherical diameter", signif(m$esd[ok], 6))
    }
  }
  if (!length(out)) return(NULL)
  # the NERC id columns stay even when empty: an empty id is a visible gap, a missing column is not
  dplyr::bind_rows(out) |> .drop_all_na(keep = c("eventID", "occurrenceID", "measurementID",
                                                "measurementTypeID", "measurementUnitID"))
}

.drop_all_na <- function(d, keep = character()) {
  drop <- vapply(names(d), function(n) !(n %in% keep) && all(is.na(d[[n]])), TRUE)
  d[, !drop, drop = FALSE]
}

# meta.xml from the term map: a column with no term is an error, never a silent drop
dwc_meta_xml <- function(tables) {
  stopifnot(!is.null(tables$event))
  hdr <- paste0('encoding="UTF-8" fieldsTerminatedBy="," linesTerminatedBy="\\n" ',
                "fieldsEnclosedBy='\"' ignoreHeaderLines=\"1\"")
  fields <- function(cols, skip = NULL) {
    terms <- dwc_term_map(cols)
    keep <- !(cols %in% skip)
    paste(sprintf('    <field index="%d" term="%s"/>', seq_along(cols)[keep] - 1L, terms[cols[keep]]),
          collapse = "\n")
  }
  ev <- DWC_ROW_TYPES$event; cols <- names(tables$event)
  core <- sprintf('  <core %s rowType="%s">\n    <files>\n      <location>%s</location>\n    </files>\n    <id index="%d" />\n%s\n  </core>',
                  hdr, ev$rowType, ev$file, which(cols == ev$id) - 1L, fields(cols))
  ext <- character()
  for (nm in c("occurrence", "emof")) {
    d <- tables[[nm]]; if (is.null(d) || !nrow(d)) next
    sp <- DWC_ROW_TYPES[[nm]]; cols <- names(d)
    ext <- c(ext, sprintf('  <extension %s rowType="%s">\n    <files>\n      <location>%s</location>\n    </files>\n    <coreid index="%d" />\n%s\n  </extension>',
                          hdr, sp$rowType, sp$file, which(cols == sp$coreid) - 1L, fields(cols, skip = sp$coreid)))
  }
  paste0('<?xml version="1.0" encoding="UTF-8"?>\n<archive xmlns="http://rs.tdwg.org/dwc/text/"\n',
         '         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n',
         '         xsi:schemaLocation="http://rs.tdwg.org/dwc/text/ http://rs.tdwg.org/dwc/text/tdwg_dwc_text.xsd">\n',
         paste(c(core, ext), collapse = "\n"), "\n</archive>\n")
}

# structural checks before anything is zipped; `error` findings block the archive
dwc_check <- function(event, occurrence = NULL, emof = NULL) {
  f <- list()
  add <- function(finding, level, n, detail = "") f[[length(f) + 1]] <<- tibble::tibble(
    finding = finding, level = level, n = as.integer(n), detail = detail)
  if (anyDuplicated(event$eventID)) add("duplicate_id", "error", sum(duplicated(event$eventID)), "eventID")
  orphan <- event$parentEventID[!is.na(event$parentEventID) & !(event$parentEventID %in% event$eventID)]
  if (length(orphan)) add("orphan_event", "error", length(orphan), paste(unique(orphan), collapse = ", "))
  if (!is.null(occurrence) && nrow(occurrence)) {
    if (anyDuplicated(occurrence$occurrenceID)) add("duplicate_id", "error", sum(duplicated(occurrence$occurrenceID)), "occurrenceID")
    oo <- !(occurrence$eventID %in% event$eventID)
    if (any(oo)) add("orphan_occurrence", "error", sum(oo))
    if (any(is.na(occurrence$scientificNameID))) add("no_scientific_name_id", "warn", sum(is.na(occurrence$scientificNameID)))
    ev_of <- event[match(occurrence$eventID, event$eventID), ]
    nodate <- is.na(ev_of$eventDate); nocoord <- is.na(ev_of$decimalLatitude) | is.na(ev_of$decimalLongitude)
    if (any(nodate)) add("no_event_date", "warn", sum(nodate))
    if (any(nocoord)) add("no_coordinates", "warn", sum(nocoord))
    pred <- occurrence$identificationVerificationStatus == "predicted"
    if (any(pred)) add("predicted_included", "warn", sum(pred), "classifier predictions published, not validated")
  } else add("no_occurrence", "error", 0L, "no occurrence rows")
  if (!is.null(emof) && nrow(emof)) {
    oe <- !(emof$eventID %in% event$eventID)
    if (any(oe)) add("orphan_emof", "error", sum(oe))
    oc <- !is.na(emof$occurrenceID) & !(emof$occurrenceID %in% occurrence$occurrenceID)
    if (any(oc)) add("orphan_emof", "error", sum(oc), "occurrenceID")
    if ("measurementTypeID" %in% names(emof) && any(is.na(emof$measurementTypeID)))
      add("no_measurement_type_id", "warn", sum(is.na(emof$measurementTypeID)))
  }
  if (!length(f)) add("ok", "ok", 0L)
  dplyr::bind_rows(f)
}
assert_dwc <- function(findings) {
  bad <- findings[findings$level == "error", ]
  if (nrow(bad)) stop("Darwin Core errors: ", paste0(bad$finding, " (", bad$n, ")", collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# write the CSVs + meta.xml (+ eml.xml), zip them with a fixed timestamp, and a manifest with
# a content hash over the data files so a rebuild of identical rows reproduces the bytes
dwc_archive <- function(dir, event, occurrence = NULL, emof = NULL, eml_path = NULL,
                        dataset_key, version, zip_path = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list(event = event, occurrence = occurrence, emof = emof)
  files <- character()
  for (nm in names(DWC_ROW_TYPES)) {
    d <- tables[[nm]]; if (is.null(d) || !nrow(d)) next
    p <- file.path(dir, DWC_ROW_TYPES[[nm]]$file)
    utils::write.csv(d, p, row.names = FALSE, na = "", quote = TRUE, fileEncoding = "UTF-8")
    files <- c(files, p)
  }
  meta <- file.path(dir, "meta.xml"); writeLines(dwc_meta_xml(tables), meta, useBytes = TRUE)
  files <- c(files, meta)
  if (!is.null(eml_path) && file.exists(eml_path)) {
    file.copy(eml_path, file.path(dir, "eml.xml"), overwrite = TRUE); files <- c(files, file.path(dir, "eml.xml"))
  }
  csvs <- files[grepl("\\.csv$", files)]
  content_hash <- digest::digest(paste(vapply(sort(csvs), function(p) digest::digest(file = p, algo = "sha256"), ""),
                                       collapse = ""), algo = "sha256")
  if (is.null(zip_path)) zip_path <- file.path(dirname(dir), paste0(dataset_key, "_", version, ".zip"))
  if (file.exists(zip_path)) unlink(zip_path)
  stamp <- as.POSIXct(paste(if (grepl("^\\d{4}-\\d{2}-\\d{2}$", version)) version else "2020-01-01", "00:00:00"), tz = "UTC")
  Sys.setFileTime(files, rep(stamp, length(files)))
  utils::zip(zip_path, files = files, flags = "-jqX")
  counts <- list(event = nrow(event), occurrence = if (is.null(occurrence)) 0L else nrow(occurrence),
                 emof = if (is.null(emof)) 0L else nrow(emof))
  man <- list(dataset_key = dataset_key, version = version, content_hash = content_hash,
              archive = basename(zip_path), archive_sha256 = digest::digest(file = zip_path, algo = "sha256"),
              counts = counts, generated_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"))
  mpath <- file.path(dirname(zip_path), paste0(dataset_key, "_", version, "_manifest.json"))
  writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE), mpath)
  list(zip = zip_path, manifest = mpath, content_hash = content_hash, counts = counts)
}
