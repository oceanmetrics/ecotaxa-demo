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
test_that("ecotaxa_project_id() reads ECOTAXA_PROJECT_ID, defaults to 2 and refuses junk", {
  withr::with_envvar(c(ECOTAXA_PROJECT_ID = NA), expect_equal(ecotaxa_project_id(), 2L))
  withr::with_envvar(c(ECOTAXA_PROJECT_ID = "4"), expect_equal(ecotaxa_project_id(), 4L))
  withr::with_envvar(c(ECOTAXA_PROJECT_ID = "four"), expect_error(ecotaxa_project_id(), "whole number"))
})
test_that("a token alone counts as a login and is returned without calling the node", {
  withr::with_envvar(c(ECOTAXA_TOKEN = "abc.def.ghi", ECOTAXA_USER = NA, ECOTAXA_PASSWORD = NA), {
    expect_true(ecotaxa_has_login())
    expect_equal(ecotaxa_login(base = "http://127.0.0.1:9"), "abc.def.ghi")
  })
  withr::with_envvar(c(ECOTAXA_TOKEN = NA, ECOTAXA_USER = NA, ECOTAXA_PASSWORD = NA),
                     expect_false(ecotaxa_has_login()))
})
test_that("ecotaxa_export_body() sends values the node's GeneralExportReq accepts (regression: split_by '' was a 422)", {
  b <- ecotaxa_export_body(4)
  expect_equal(b$request$split_by, "sample")
  expect_true(b$request$split_by %in% c("sample", "acquisition", "taxon", "none"))
  expect_equal(b$request$with_images, "first"); expect_identical(b$request$project_id, 4L)
  expect_equal(as.character(jsonlite::toJSON(b$filters)), "{}")
  expect_error(ecotaxa_export_body(4, split_by = ""))
  expect_error(ecotaxa_export_body(4, with_images = "yes"))
})
test_that("ecotaxa_sample_batches() caps objects per batch, keeps order, and sends an oversized sample alone", {
  b <- ecotaxa_sample_batches(c(11, 12, 13, 14, 15), c(10, 20, 15, 50, 5), max_objects = 30)
  expect_equal(b, list(c(11, 12), 13, 14, 15))
  expect_equal(unlist(b), c(11, 12, 13, 14, 15))
  expect_equal(ecotaxa_sample_batches(1:3, c(1, 1, 1), 100), list(1:3))
  expect_error(ecotaxa_sample_batches(1:2, c(1, NA)))
})
test_that("ecotaxa_export_body(samples =) fills the samples filter as comma-separated ids (no scientific notation)", {
  b <- ecotaxa_export_body(4, samples = c(400000123, 400000124))
  expect_equal(b$filters$samples, "400000123,400000124")
  expect_equal(as.character(jsonlite::toJSON(ecotaxa_export_body(4)$filters)), "{}")
})
