# ncei.R: the archive package for one EcoTaxa project at a dated version
#
# a BagIt-style bag (bagit.txt, bag-info.txt, data/, manifest-sha256.txt, tagmanifest-sha256.txt)
# holding the images, the EcoTaxa TSV export, the DwC-A and the metadata. this is a PROPOSAL of
# layout to agree with NCEI; nothing here submits anything anywhere.

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

ncei_package <- function(out_dir, dataset_key, version, tsv_paths, image_dir = NULL, dwca_zip = NULL,
                         eml_path = NULL, dataset_json = list(), source_url = ecotaxa_url()) {
  bag <- file.path(out_dir, paste0(dataset_key, "_", version))
  if (dir.exists(bag)) unlink(bag, recursive = TRUE)
  dirs <- file.path(bag, "data", c("images", "ecotaxa_export", "dwca", "metadata"))
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  # payload
  for (p in tsv_paths) file.copy(p, file.path(bag, "data/ecotaxa_export", basename(p)))
  n_img <- 0L
  if (!is.null(image_dir) && dir.exists(image_dir)) {
    # vectorized: a project can hold a million images, so one dir.create per folder and one
    # file.copy call, not one of each per image
    rel  <- list.files(image_dir, pattern = "\\.(jpg|jpeg|png|tif|tiff)$", recursive = TRUE, ignore.case = TRUE)
    dest <- file.path(bag, "data/images", rel)
    for (d in unique(dirname(dest))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(file.path(image_dir, rel), dest)
    stopifnot("some images could not be copied into the bag" = all(ok))
    n_img <- length(rel)
  }
  if (!is.null(dwca_zip) && file.exists(dwca_zip)) file.copy(dwca_zip, file.path(bag, "data/dwca", basename(dwca_zip)))
  if (!is.null(eml_path) && file.exists(eml_path)) file.copy(eml_path, file.path(bag, "data/metadata/eml.xml"))
  dj <- c(list(dataset_key = dataset_key, version = version, source = source_url,
               n_images = n_img, n_tsv = length(tsv_paths),
               created_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")), dataset_json)
  writeLines(jsonlite::toJSON(dj, auto_unbox = TRUE, pretty = TRUE), file.path(bag, "data/metadata/dataset.json"))
  # tag files
  writeLines(c("BagIt-Version: 1.0", "Tag-File-Character-Encoding: UTF-8"), file.path(bag, "bagit.txt"))
  payload <- list.files(file.path(bag, "data"), recursive = TRUE, full.names = TRUE)
  rel <- sub(paste0("^", bag, "/"), "", payload)
  size <- sum(file.size(payload))
  writeLines(c(paste0("Source-Organization: ", dataset_json$organization %||% "Ocean Metrics (demo)"),
               paste0("External-Identifier: ", dataset_key, "_", version),
               paste0("Bagging-Date: ", format(Sys.Date())),
               paste0("Payload-Oxum: ", size, ".", length(payload)),
               paste0("Bag-Size: ", round(size / 1024^2, 2), " MB"),
               paste0("External-Description: EcoTaxa project export (images, TSV, Darwin Core Archive, metadata) from ", source_url)),
             file.path(bag, "bag-info.txt"))
  writeLines(paste(vapply(payload, sha256_file, ""), rel), file.path(bag, "manifest-sha256.txt"))
  tags <- file.path(bag, c("bagit.txt", "bag-info.txt", "manifest-sha256.txt"))
  writeLines(paste(vapply(tags, sha256_file, ""), basename(tags)), file.path(bag, "tagmanifest-sha256.txt"))
  list(bag = bag, n_payload = length(payload), bytes = size, n_images = n_img)
}

# re-hash every payload file and compare with the manifest; findings frame (ok when empty)
ncei_verify <- function(bag) {
  man <- readLines(file.path(bag, "manifest-sha256.txt"))
  parts <- regmatches(man, regexpr(" ", man), invert = TRUE)
  want <- stats::setNames(vapply(parts, `[`, "", 1), vapply(parts, `[`, "", 2))
  have_files <- list.files(file.path(bag, "data"), recursive = TRUE)
  have <- stats::setNames(vapply(file.path(bag, "data", have_files), sha256_file, ""), paste0("data/", have_files))
  # vectorized: a per-name lookup in a loop is quadratic and never finishes on a million files
  # (regression: project 4's bag, 2026-11-10)
  both <- intersect(names(want), names(have))
  f <- tibble::tibble(
    finding = c(rep("missing_file", length(setdiff(names(want), names(have)))),
                rep("unlisted_file", length(setdiff(names(have), names(want)))),
                rep("checksum_mismatch", sum(want[both] != have[both])),
                if (!file.exists(file.path(bag, "bagit.txt"))) "missing_file"),
    path = c(setdiff(names(want), names(have)), setdiff(names(have), names(want)),
             both[want[both] != have[both]],
             if (!file.exists(file.path(bag, "bagit.txt"))) "bagit.txt"))
  if (!nrow(f)) return(tibble::tibble(finding = "ok", path = NA_character_))
  f
}
