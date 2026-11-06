# source every function file; the notebooks do the same
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
fixture <- function(...) file.path(testthat::test_path(), "fixtures", ...)
# the six categories project 2 uses, as /api/taxon_set/query returned them on 2026-11-06
fixture_taxa <- function() tibble::tibble(
  ecotaxa_id   = c(1L, 45072L, 78418L, 84963L, 85011L, 85012L, 85078L),
  name         = c("Biota", "Cyclopoida", "Oncaeidae", "detritus", "other", "t001", "egg"),
  display_name = c("Biota", "Cyclopoida", "Oncaeidae", "detritus<not-living", "other<Biota", "t001", "egg<other"),
  type         = c("P", "P", "P", "M", "M", "M", "M"),
  rank         = c("Superdomain", "Order", "Family", NA, NA, NA, NA),
  aphia_id     = c(1L, 1101L, 128586L, NA, NA, NA, NA),
  lineage      = c("Biota", "Biota > Animalia > Arthropoda > Crustacea > Multicrustacea > Copepoda > Neocopepoda > Podoplea > Cyclopoida",
                   "Biota > Animalia > Arthropoda > Crustacea > Multicrustacea > Copepoda > Neocopepoda > Podoplea > Cyclopoida > Ergasilida > Oncaeidae",
                   "not-living > detritus", "Biota > other", "temporary > t001", "Biota > other > egg"),
  id_lineage   = c("1", "1,2,3,4,5,6,7,8,45072", "1,2,3,4,5,6,7,8,45072,9,78418", "90,84963", "1,85011", "91,85012", "1,85011,85078"))
# a canned WoRMS, no network
fake_worms <- list(
  by_id = function(id) {
    recs <- list(
      `1101`   = list(AphiaID = 1101, scientificname = "Cyclopoida", status = "accepted", valid_AphiaID = 1101, valid_name = "Cyclopoida", rank = "Order", kingdom = "Animalia", phylum = "Arthropoda", class = "Copepoda", order = "Cyclopoida", family = NA, genus = NA, lsid = "urn:lsid:marinespecies.org:taxname:1101"),
      `128586` = list(AphiaID = 128586, scientificname = "Oncaeidae", status = "accepted", valid_AphiaID = 128586, valid_name = "Oncaeidae", rank = "Family", kingdom = "Animalia", phylum = "Arthropoda", class = "Copepoda", order = "Cyclopoida", family = "Oncaeidae", genus = NA, lsid = "urn:lsid:marinespecies.org:taxname:128586"),
      `1`      = list(AphiaID = 1, scientificname = "Biota", status = "accepted", valid_AphiaID = 1, valid_name = "Biota", rank = "Superdomain", lsid = "urn:lsid:marinespecies.org:taxname:1"))
    worms_row(recs[[as.character(id)]])
  },
  by_name = function(name) if (name == "Calanoida") worms_row(list(AphiaID = 1100, scientificname = "Calanoida", status = "accepted", valid_AphiaID = 1100, valid_name = "Calanoida", rank = "Order", kingdom = "Animalia", lsid = "x")) else NULL)
# a tiny synthetic export: two samples, one with two acquisitions; 6 objects
fixture_objects <- function() tibble::tibble(
  object_id = paste0("o", 1:6),
  object_lat = c(43.6, 43.6, 43.6, 43.6, 44.1, 44.1), object_lon = c(7.3, 7.3, 7.3, 7.3, 7.9, 7.9),
  object_date = rep(c("20140701", "20140702"), c(4, 2)), object_time = c("100000", "100000", "103000", "103000", "090000", "090000"),
  object_depth_min = c(0, 0, 0, 0, 10, 10), object_depth_max = c(100, 100, 100, 100, 200, 200),
  object_annotation_status = c("validated", "validated", "predicted", "validated", "validated", "dubious"),
  object_annotation_person_name = c("Ann Smith", "Ann Smith", NA, "Bo Li", "Ann Smith", "Bo Li"),
  object_annotation_date = c("20150101", "20150101", NA, "20150102", "20150103", "20150103"),
  object_annotation_category = c("Cyclopoida", "Oncaeidae", "Cyclopoida", "detritus<not-living", "egg<other", "Cyclopoida"),
  object_annotation_category_id = c(45072L, 78418L, 45072L, 84963L, 85078L, 45072L),
  object_esd = c(10, 20, 30, 40, 50, 60),
  sample_id = rep(c("s1", "s2"), c(4, 2)), sample_tot_vol = c(2, 2, 2, 2, NA, NA),
  sample_samplinggear = "wp2 200um", acq_id = c("a1", "a1", "a2", "a2", "a3", "a3"), acq_instrument = "Zooscan")
