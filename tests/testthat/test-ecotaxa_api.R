test_that("read_ecotaxa_tsv() drops the types row and types [f] columns", {
  p <- fixture("export_small.tsv")
  d <- read_ecotaxa_tsv(p)
  expect_equal(nrow(d), 2L)
  expect_type(d$object_lat, "double")
  expect_type(d$object_id, "character")
  expect_equal(d$object_annotation_category_id, c("45072", "84963"))
  expect_equal(d$source_file, rep("export_small.tsv", 2))
})
test_that("read_ecotaxa_tsv() reads the upstream import set (no types row twist: it has one)", {
  p <- file.path(root, "data-raw/import_test/m106_mn01_n1_sml/ecotaxa_m106_mn01_n1_sml.tsv")
  skip_if_not(file.exists(p))
  d <- read_ecotaxa_tsv(p)
  expect_equal(nrow(d), 5L)
  expect_true(all(c("object_annotation_category_id", "sample_id", "acq_instrument") %in% names(d)))
})
test_that("ecotaxa_datetime() builds ISO 8601 UTC and tolerates a missing time", {
  expect_equal(ecotaxa_datetime("20140701", "103005"), "2014-07-01T10:30:05Z")
  expect_equal(ecotaxa_datetime("20140701", NA), "2014-07-01")
  expect_equal(ecotaxa_datetime(NA, "103005"), NA_character_)
})
test_that("parse_wormsification() recovers EcoTaxa's own mapping: phylo to itself, morpho to nearest living ancestor, none for non-living", {
  # the answer /api/taxon_set/wormsification gave on 2026-11-06 for the demo project's categories
  w <- list(`1` = list(id = 1, display_name = "Biota", type = "P", aphia_id = 1),
            `45072` = list(id = 45072, display_name = "Cyclopoida", type = "P", aphia_id = 1101),
            `78418` = list(id = 78418, display_name = "Oncaeidae", type = "P", aphia_id = 128586))
  ids <- c(45072L, 78418L, 84963L, 85011L, 85012L, 85078L)
  m <- parse_wormsification(w, ids, taxa = fixture_taxa())
  expect_equal(m$maps_to_aphia_id, c(1101L, 128586L, NA, 1L, NA, 1L))
  expect_equal(m$maps_to_name[m$ecotaxa_id == 85078], "Biota")      # egg<other lands on Biota
  expect_true(is.na(m$maps_to_id[m$ecotaxa_id == 84963]))             # detritus: no solution
})
