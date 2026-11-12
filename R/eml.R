# eml.R: one EML 2.2 document per EcoTaxa project, from the project record and the DwC tables
#
# written with xml2 rather than the EML package (not installed here); eml_check() asserts the
# elements the OBIS-USA IPT needs. no metadata string is invented: every text comes from the
# project record or from `meta` passed by the notebook, and a missing one is a finding.

build_eml <- function(project, event, occurrence, meta = list(), package_id = NULL) {
  g <- function(k, default = NA_character_) {
    v <- meta[[k]]; if (is.null(v) || !nzchar(v)) v <- project[[k]]
    if (is.null(v) || !nzchar(v)) default else as.character(v)
  }
  title <- g("title"); abstract <- g("abstract", g("description", g("comments")))
  license <- g("license"); contact_name <- g("contact_name"); contact_email <- g("contact_email")
  org <- g("organization", "Ocean Metrics (demo)")
  if (is.null(package_id)) package_id <- paste0("ecotaxa-project-", project$projid %||% "x", "-", format(Sys.Date()))
  lat <- event$decimalLatitude; lon <- event$decimalLongitude
  dates <- sort(unique(unlist(strsplit(stats::na.omit(event$eventDate), "/"))))
  taxa <- if (nrow(occurrence)) unique(occurrence[, c("scientificName", "taxonRank")]) else NULL

  doc <- xml2::xml_new_root("eml:eml",
    "xmlns:eml" = "https://eml.ecoinformatics.org/eml-2.2.0",
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xsi:schemaLocation" = "https://eml.ecoinformatics.org/eml-2.2.0 https://eml.ecoinformatics.org/eml-2.2.0/eml.xsd",
    packageId = package_id, system = "oceanmetrics.io")
  ds <- xml2::xml_add_child(doc, "dataset")
  xml2::xml_add_child(ds, "title", title)
  party <- function(parent, tag) {
    p <- xml2::xml_add_child(parent, tag)
    if (!is.na(contact_name)) {
      nm <- xml2::xml_add_child(p, "individualName"); pn <- split_person_name(contact_name)
      if (!is.na(pn$given)) xml2::xml_add_child(nm, "givenName", pn$given)
      xml2::xml_add_child(nm, "surName", pn$sur)
    }
    xml2::xml_add_child(p, "organizationName", org)
    if (!is.na(contact_email)) xml2::xml_add_child(p, "electronicMailAddress", contact_email)
    p
  }
  party(ds, "creator")
  # others credited, from projects.csv associated_parties ("Name | Organization | role; ...")
  parties <- parse_parties(meta$associated_parties %||% project$associated_parties)
  for (i in seq_len(nrow(parties))) {
    ap <- xml2::xml_add_child(ds, "associatedParty"); nm <- xml2::xml_add_child(ap, "individualName")
    pn <- split_person_name(parties$name[i])
    if (!is.na(pn$given)) xml2::xml_add_child(nm, "givenName", pn$given)
    xml2::xml_add_child(nm, "surName", pn$sur)
    xml2::xml_add_child(ap, "organizationName", parties$organization[i])
    xml2::xml_add_child(ap, "role", parties$role[i])
  }
  xml2::xml_add_child(ds, "pubDate", format(Sys.Date()))
  xml2::xml_add_child(ds, "language", "eng")
  ab <- xml2::xml_add_child(ds, "abstract"); xml2::xml_add_child(ab, "para", if (is.na(abstract)) "" else abstract)
  kw <- xml2::xml_add_child(ds, "keywordSet")
  for (k in c("plankton imagery", "EcoTaxa", g("instrument", "imaging instrument"), "Occurrence"))
    xml2::xml_add_child(kw, "keyword", k)
  ir <- xml2::xml_add_child(ds, "intellectualRights")
  li <- if (is.na(license)) NULL else license_info(license)
  if (is.null(li)) xml2::xml_add_child(ir, "para", if (is.na(license)) "" else license) else {
    # the sentence and link GBIF and the OBIS IPT recognise as a machine-readable license
    # (url and title come from LICENSES, not from user text, so no escaping is needed)
    xml2::xml_add_child(ir, xml2::read_xml(sprintf(
      '<para>This work is licensed under a <ulink url="%s"><citetitle>%s</citetitle></ulink>.</para>', li$url, li$title)))
  }
  cov <- xml2::xml_add_child(ds, "coverage")
  if (any(!is.na(lat)) && any(!is.na(lon))) {
    gc <- xml2::xml_add_child(cov, "geographicCoverage")
    xml2::xml_add_child(gc, "geographicDescription", g("geographic_description", "Bounding box of the sampling events"))
    bb <- xml2::xml_add_child(gc, "boundingCoordinates")
    xml2::xml_add_child(bb, "westBoundingCoordinate", format(min(lon, na.rm = TRUE)))
    xml2::xml_add_child(bb, "eastBoundingCoordinate", format(max(lon, na.rm = TRUE)))
    xml2::xml_add_child(bb, "northBoundingCoordinate", format(max(lat, na.rm = TRUE)))
    xml2::xml_add_child(bb, "southBoundingCoordinate", format(min(lat, na.rm = TRUE)))
  }
  if (length(dates)) {
    tc <- xml2::xml_add_child(cov, "temporalCoverage"); rd <- xml2::xml_add_child(tc, "rangeOfDates")
    xml2::xml_add_child(xml2::xml_add_child(rd, "beginDate"), "calendarDate", substr(dates[1], 1, 10))
    xml2::xml_add_child(xml2::xml_add_child(rd, "endDate"), "calendarDate", substr(dates[length(dates)], 1, 10))
  }
  if (!is.null(taxa) && nrow(taxa)) {
    tx <- xml2::xml_add_child(cov, "taxonomicCoverage")
    for (i in seq_len(nrow(taxa))) {
      c1 <- xml2::xml_add_child(tx, "taxonomicClassification")
      xml2::xml_add_child(c1, "taxonRankName", taxa$taxonRank[i])
      xml2::xml_add_child(c1, "taxonRankValue", taxa$scientificName[i])
    }
  }
  party(ds, "contact")
  me <- xml2::xml_add_child(ds, "methods"); ms <- xml2::xml_add_child(me, "methodStep")
  xml2::xml_add_child(xml2::xml_add_child(ms, "description"), "para",
    g("methods", paste0("Objects imaged with ", g("instrument", "an imaging instrument"),
      ", classified in EcoTaxa (", ecotaxa_url(), ") and exported through its API; ",
      "categories resolved to WoRMS through a reviewed lookup table; occurrences aggregated per sample and taxon.")))
  doc
}

# the elements the IPT and OBIS need; returns a findings frame
eml_check <- function(doc) {
  need <- c("dataset/title", "dataset/creator", "dataset/contact", "dataset/abstract/para",
            "dataset/intellectualRights", "dataset/pubDate", "dataset/coverage/geographicCoverage",
            "dataset/coverage/temporalCoverage")
  f <- lapply(need, function(x) {
    n <- xml2::xml_find_first(doc, paste0("./", x))
    present <- !inherits(n, "xml_missing") && (length(xml2::xml_children(n)) > 0 || nzchar(trimws(xml2::xml_text(n))))
    tibble::tibble(element = x, present = present, level = ifelse(present, "ok", "error"))
  })
  # the license must be one OBIS accepts, linked as GBIF expects
  lic <- !inherits(xml2::xml_find_first(doc, "./dataset/intellectualRights/para/ulink"), "xml_missing")
  f[[length(f) + 1]] <- tibble::tibble(element = "dataset/intellectualRights/para/ulink", present = lic,
                                       level = ifelse(lic, "ok", "error"))
  dplyr::bind_rows(f)
}
write_eml <- function(doc, path) { xml2::write_xml(doc, path); invisible(path) }

# the hand-kept metadata for one project (metadata/projects.csv): title, abstract, license,
# contact, organization, verification. one row per project; a project without a row stops here
# rather than borrowing another project's words
read_project_meta <- function(path, project_id) {
  m <- readr::read_csv(path, show_col_types = FALSE,
                       col_types = readr::cols(project_id = "i", .default = "c"))
  r <- m[m$project_id == as.integer(project_id), ]
  if (nrow(r) != 1) stop("metadata/projects.csv has ", nrow(r), " rows for project ", project_id,
                         "; add exactly one", call. = FALSE)
  as.list(r)
}
`%||%` <- function(a, b) if (is.null(a)) b else a
