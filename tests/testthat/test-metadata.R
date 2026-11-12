dict <- tibble::tibble(
  field      = c("sample_total_volume", "acq_imaged_volume", "object_lat", "object_area"),
  category   = c("Sample details", "Acquisition details", "Object details", "Object details"),
  definition = c("Total volume of the sample", "Volume of water imaged", "Latitude", "Area of object."),
  unit       = c("mL", "mL", "decimal degrees North", "pixel^2"))
owner_units <- tibble::tibble(field = "sample_total_volume", unit = "L", source = "owner, 2026-11-11")

test_that("read_data_dictionary() reads the saved NCEI sheet and drops its Status column", {
  d <- read_data_dictionary(file.path(root, "metadata/ecotaxa_data_dictionary.csv"))
  expect_equal(names(d), c("field", "category", "definition", "unit"))
  expect_true(all(c("object_id", "sample_total_volume", "acq_imaged_volume", "process_pixel") %in% d$field))
  expect_equal(d$unit[d$field == "acq_imaged_volume"], "mL")
  expect_equal(sum(duplicated(d$field)), 0L)
})
test_that("ecotaxa_column_dictionary() documents each column and lets the owner's unit win", {
  cd <- ecotaxa_column_dictionary(c("object_lat", "sample_total_volume", "acq_imaged_volume", "sample_ship_x"), dict, owner_units)
  expect_equal(cd$documented, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(cd$unit[cd$field == "sample_total_volume"], "L")
  expect_equal(cd$unit_source[cd$field == "sample_total_volume"], "owner, 2026-11-11")
  expect_equal(cd$unit_source[cd$field == "acq_imaged_volume"], "data dictionary")
  expect_true(is.na(cd$unit[cd$field == "sample_ship_x"]))
  expect_equal(unname(column_unit(cd, c("acq_imaged_volume", "sample_ship_x"))), c("mL", NA))
})
test_that("volume_to_m3() converts known units and stops on an unknown one", {
  expect_equal(volume_to_m3(40300, "L"), 40.3)
  expect_equal(volume_to_m3("0.6242", "mL"), 6.242e-7)
  expect_error(volume_to_m3(1, "pixel"), "column_units.csv")
  expect_error(volume_to_m3(1, NA), "no known volume unit")
})
test_that("apply_sample_fixes() sets every row of a sample, blanks an empty value, and stops on a stale fix", {
  o <- fixture_objects()
  fx <- tibble::tibble(project_id = 2L, sample_id = c("s2", "s2", "s1"), field = c("object_lat", "object_lon", "sample_tot_vol"),
                       value = c("25.0038", "-80.3757", ""), source = "test")
  x <- apply_sample_fixes(o, fx)
  expect_equal(unique(x$object_lat[x$sample_id == "s2"]), 25.0038)
  expect_equal(unique(x$object_lon[x$sample_id == "s2"]), -80.3757)
  expect_equal(x$object_lat[x$sample_id == "s1"], o$object_lat[o$sample_id == "s1"])
  expect_true(all(is.na(x$sample_tot_vol[x$sample_id == "s1"])))
  # the sample map's own column names
  m <- tibble::tibble(orig_id = c("s1", "s2"), lat = c(1, 0), lon = c(2, 0))
  mm <- apply_sample_fixes(m, fx[1:2, ], id = "orig_id", fields = c(object_lat = "lat", object_lon = "lon"))
  expect_equal(mm$lat, c(1, 25.0038)); expect_equal(mm$lon, c(2, -80.3757))
  expect_error(apply_sample_fixes(o, transform(fx[1, ], sample_id = "nope")), "sample the table does not have: nope")
  expect_error(apply_sample_fixes(o, transform(fx[1, ], field = "nope")), "field the table does not have")
})
test_that("the project 4 fixes put ws21093_MR_64um at the owner's position (regression: 0,0 in the backup)", {
  fx <- read_sample_fixes(file.path(root, "metadata/sample_fixes.csv"), 4)
  o <- fixture_objects(); o$sample_id[o$sample_id == "s2"] <- "ws21093_MR_64um"
  o$object_lat[o$sample_id == "ws21093_MR_64um"] <- 0; o$object_lon[o$sample_id == "ws21093_MR_64um"] <- 0
  o$sample_total_volume <- "2557544141"
  x <- apply_sample_fixes(o, fx)
  ev <- dwc_event(x, "p4")
  w <- ev[ev$eventID == "p4:ws21093_MR_64um", ]
  expect_equal(c(w$decimalLatitude, w$decimalLongitude), c(25.0038, -80.3757))
  expect_true(all(is.na(x$sample_total_volume[x$sample_id == "ws21093_MR_64um"])))
  expect_equal(nrow(read_sample_fixes(file.path(root, "metadata/sample_fixes.csv"), 2)), 0L)
})
test_that("license_check() accepts the OBIS licenses and refuses NC for NCEI", {
  expect_equal(license_check("CC BY 4.0", "obis")$level, "ok")
  expect_equal(license_check("CC BY 4.0", "ncei")$level, "ok")
  expect_equal(license_check("CC BY-NC 4.0", "obis")$level, "ok")
  expect_equal(license_check("CC BY-NC 4.0", "ncei")$finding, "license_not_accepted_by_ncei")
  expect_equal(license_check("Not yet chosen", "obis")$finding, "license_unknown")
  expect_equal(license_info(" CC0 1.0 ")$url, "http://creativecommons.org/publicdomain/zero/1.0/legalcode")
})
test_that("parse_parties() reads 'Name | Organization | role' lists and refuses a malformed one", {
  p <- parse_parties("Di Geronimo, Sebastian | University of South Florida | annotator; Lilly Verrill | USF | annotator")
  expect_equal(p$name, c("Di Geronimo, Sebastian", "Lilly Verrill"))
  expect_equal(p$role, c("annotator", "annotator"))
  expect_equal(nrow(parse_parties(NA)), 0L); expect_equal(nrow(parse_parties("")), 0L)
  expect_error(parse_parties("Just A Name"), "Name \\| Organization \\| role")
})
test_that("split_person_name() keeps a multi-word surname given as 'Surname, Given' (regression: Di Geronimo)", {
  expect_equal(split_person_name("Di Geronimo, Sebastian"), list(given = "Sebastian", sur = "Di Geronimo"))
  expect_equal(split_person_name("Ana Carolina Peralta"), list(given = "Ana Carolina", sur = "Peralta"))
  expect_equal(split_person_name("Plato"), list(given = NA_character_, sur = "Plato"))
})
test_that("projects.csv: every project has a license OBIS accepts", {
  m <- readr::read_csv(file.path(root, "metadata/projects.csv"), show_col_types = FALSE)
  expect_true(all(vapply(m$license, function(l) license_check(l, "obis")$level == "ok", TRUE)))
})
