lk <- build_taxa_lookup(fixture_taxa(), by_id = fake_worms$by_id, by_name = fake_worms$by_name)
reg <- read_measurement_registry(file.path(root, "metadata/measurement_type.csv"))

test_that("dwc_event() makes one event per sample and child events only for multi-acquisition samples", {
  ev <- dwc_event(fixture_objects(), "p2")
  expect_equal(sort(ev$eventID), sort(c("p2:s1", "p2:s1:a1", "p2:s1:a2", "p2:s2")))
  s1 <- ev[ev$eventID == "p2:s1", ]
  expect_equal(s1$eventDate, "2014-07-01T10:00:00Z/2014-07-01T10:30:00Z")
  expect_equal(s1$decimalLatitude, 43.6); expect_equal(s1$maximumDepthInMeters, 100)
  expect_equal(s1$sampleSizeValue, 2); expect_equal(s1$sampleSizeUnit, "m3")
  expect_equal(s1$samplingProtocol, "Zooscan; wp2 200um")
  expect_equal(ev$parentEventID[ev$eventID == "p2:s1:a2"], "p2:s1")
  expect_true(is.na(ev$sampleSizeValue[ev$eventID == "p2:s2"]))
  expect_equal(ev$geodeticDatum[ev$eventType == "sample"], rep("EPSG:4326", 2))
})
test_that("dwc_occurrence() publishes validated objects only, aggregated per event x taxon, excluded categories dropped", {
  o <- fixture_objects(); ev <- dwc_event(o, "p2")
  oc <- dwc_occurrence(o, ev, lk, "p2")
  # validated: o1 Cyclopoida (s1/a1), o2 Oncaeidae (s1/a1), o4 detritus (excluded), o5 egg (excluded); dubious o6 never
  expect_equal(nrow(oc), 2L)
  a <- oc[oc$scientificName == "Cyclopoida", ]
  expect_equal(a$eventID, "p2:s1:a1"); expect_equal(a$organismQuantity, 1L)
  expect_equal(a$scientificNameID, "urn:lsid:marinespecies.org:taxname:1101")
  expect_equal(a$identificationVerificationStatus, "validated"); expect_equal(a$identifiedBy, "Ann Smith")
  expect_equal(a$basisOfRecord, "MachineObservation"); expect_equal(a$dateIdentified, "2015-01-01")
  expect_match(a$identificationRemarks, "Cyclopoida \\(45072\\)")
  expect_equal(sum(duplicated(oc$occurrenceID)), 0L)
})
test_that("dwc_occurrence(include_predicted = TRUE) adds predicted rows with their own status", {
  o <- fixture_objects(); ev <- dwc_event(o, "p2")
  oc <- dwc_occurrence(o, ev, lk, "p2", include_predicted = TRUE)
  expect_equal(nrow(oc), 3L)
  p <- oc[oc$identificationVerificationStatus == "predicted", ]
  expect_equal(p$eventID, "p2:s1:a2"); expect_true(is.na(p$identifiedBy))
  expect_equal(p$occurrenceID, "p2:s1:a2:1101:predicted")
})
test_that("dwc_emof() emits event and occurrence grains with registry ids only on exact matches", {
  o <- fixture_objects(); ev <- dwc_event(o, "p2"); oc <- dwc_occurrence(o, ev, lk, "p2")
  em <- dwc_emof(o, ev, oc, reg, "p2", instrument_url = "http://vocab.nerc.ac.uk/collection/L22/current/TOOL1581/")
  inst <- em[em$measurementType == "sampling instrument", ]
  expect_equal(nrow(inst), 2L); expect_equal(unique(inst$measurementValueID), "http://vocab.nerc.ac.uk/collection/L22/current/TOOL1581/")
  vol <- em[em$measurementType == "sample volume", ]
  expect_equal(vol$eventID, "p2:s1"); expect_equal(vol$measurementUnitID, "http://vocab.nerc.ac.uk/collection/P06/current/UCUB/")
  cnt <- em[em$measurementType == "object count", ]
  expect_equal(nrow(cnt), 2L); expect_true(all(!is.na(cnt$occurrenceID)))
  conc <- em[em$measurementType == "concentration", ]
  expect_equal(nrow(conc), 0L)  # the acquisition events carry no volume: no concentration is derived
  esd <- em[em$measurementType == "mean equivalent spherical diameter", ]
  expect_equal(esd$measurementValue[esd$occurrenceID == "p2:s1:a1:1101:validated"], "10")
  expect_true(all(is.na(esd$measurementTypeID)))
  expect_equal(sum(duplicated(em$measurementID)), 0L)
})
test_that("dwc_check() and dwc_meta_xml() gate and describe the archive", {
  o <- fixture_objects(); ev <- dwc_event(o, "p2"); oc <- dwc_occurrence(o, ev, lk, "p2")
  em <- dwc_emof(o, ev, oc, reg, "p2")
  f <- dwc_check(ev, oc, em)
  expect_false(any(f$level == "error"))
  expect_true("no_measurement_type_id" %in% f$finding)
  bad <- oc; bad$eventID[1] <- "nope"
  expect_error(assert_dwc(dwc_check(ev, bad, em)), "orphan_occurrence")
  expect_error(assert_dwc(dwc_check(ev, oc[0, ], NULL)), "no_occurrence")
  x <- xml2::read_xml(dwc_meta_xml(list(event = ev, occurrence = oc, emof = em)))
  ns <- c(d = "http://rs.tdwg.org/dwc/text/")
  expect_equal(length(xml2::xml_find_all(x, "//d:extension", ns)), 2L)
  expect_true(any(grepl("rs.iobis.org/obis/terms/measurementTypeID", xml2::xml_attr(xml2::xml_find_all(x, "//d:field", ns), "term"))))
})
test_that("dwc_archive() zips deterministically and writes a manifest", {
  o <- fixture_objects(); ev <- dwc_event(o, "p2"); oc <- dwc_occurrence(o, ev, lk, "p2")
  em <- dwc_emof(o, ev, oc, reg, "p2")
  d <- tempfile(); dir.create(d)
  a1 <- dwc_archive(file.path(d, "a"), ev, oc, em, dataset_key = "p2", version = "2026-11-06")
  dir.create(file.path(d, "other"))
  a2 <- dwc_archive(file.path(d, "other/b"), ev, oc, em, dataset_key = "p2", version = "2026-11-06",
                    zip_path = file.path(d, "other/b.zip"))
  expect_equal(a1$content_hash, a2$content_hash)
  expect_equal(sort(utils::unzip(a1$zip, list = TRUE)$Name),
               sort(c("event.csv", "occurrence.csv", "extendedMeasurementOrFact.csv", "meta.xml")))
  m <- jsonlite::fromJSON(a1$manifest)
  expect_equal(m$counts$occurrence, 2L); expect_equal(m$archive, "p2_2026-11-06.zip")
})
test_that("dwc_emof() mean ESD pools every category of an occurrence and only its own event and status (regression: vectorized join)", {
  o <- fixture_objects()
  # a seventh object: validated Cyclopoida in s1/a1 again, so that occurrence averages two objects
  o <- dplyr::bind_rows(o, o[1, ] |> dplyr::mutate(object_id = "o7", object_esd = 30))
  ev <- dwc_event(o, "p2"); oc <- dwc_occurrence(o, ev, lk, "p2", include_predicted = TRUE)
  em <- dwc_emof(o, ev, oc, reg, "p2")
  esd <- em[em$measurementType == "mean equivalent spherical diameter", ]
  expect_equal(esd$measurementValue[esd$occurrenceID == "p2:s1:a1:1101:validated"], "20")   # mean(10, 30)
  # the predicted Cyclopoida in s1/a2 (esd 30) stays apart from the validated one
  expect_equal(esd$measurementValue[esd$occurrenceID == "p2:s1:a2:1101:predicted"], "30")
  expect_equal(nrow(esd), nrow(oc))
})
test_that("ecotaxa_datetime() reads the general export's YYYY-MM-DD and HH:MM:SS too (regression: project 4 export)", {
  expect_equal(ecotaxa_datetime("2021-09-14", "13:05:00"), "2021-09-14T13:05:00Z")
  expect_equal(ecotaxa_datetime("2021-09-14", NA), "2021-09-14")
  expect_equal(ecotaxa_datetime("20210914", "130500"), "2021-09-14T13:05:00Z")
})
test_that("dwc_event() publishes a 0,0 sample without coordinates, but keeps a real 0 latitude (regression: project 4 ws21093_MR_64um)", {
  o <- fixture_objects()
  o$object_lat[o$sample_id == "s2"] <- 0; o$object_lon[o$sample_id == "s2"] <- 0
  ev <- dwc_event(o, "p2")
  s2 <- ev[ev$eventID == "p2:s2", ]
  expect_true(is.na(s2$decimalLatitude)); expect_true(is.na(s2$decimalLongitude))
  expect_false("null_island" %in% names(ev))
  o$object_lon[o$sample_id == "s2"] <- -80
  expect_equal(dwc_event(o, "p2")$decimalLatitude[dwc_event(o, "p2")$eventID == "p2:s2"], 0)
})
# a PlanktoScope-like export: s1 with two acquisitions, s2 with one; units as project 4's owner gave them
planktoscope_objects <- function() fixture_objects() |>
  dplyr::select(-sample_tot_vol) |>
  dplyr::mutate(sample_total_volume = c("40300", "40300", "40300", "40300", "20000", "20000"),
                sample_concentrated_sample_volume = c("584", "584", "584", "584", "500", "500"),
                sample_dilution_factor = c("1", "1", "1", "1", "2", "2"),
                acq_imaged_volume = "0.6242", acq_instrument = "PlanktoScope v2.5")
ps_units <- tibble::tibble(
  field = c("sample_total_volume", "sample_concentrated_sample_volume", "acq_imaged_volume"),
  category = NA, definition = "x", unit = c("L", "mL", "mL"), documented = TRUE, unit_source = "owner")

test_that("ecotaxa_volumes() follows the PlanktoScope relation total x imaged / (concentrated x dilution)", {
  v <- ecotaxa_volumes(planktoscope_objects(), ps_units)
  a1 <- v[v$acq_id == "a1", ]
  expect_equal(a1$sample_m3, 40.3)
  expect_equal(a1$imaged_m3, 40.3 * 0.6242e-6 / 584e-6)        # 0.0430737 m3, about 43 L
  s2 <- v[v$sample_id == "s2", ]
  expect_equal(s2$imaged_m3, 20 * 0.6242e-6 / (500e-6 * 2))     # the dilution halves it
  expect_match(a1$method, "sample_total_volume \\(L\\) x acq_imaged_volume \\(mL\\)")
  # without units the PlanktoScope columns cannot be read: stop, never assume
  expect_error(ecotaxa_volumes(planktoscope_objects()), "no known volume unit")
})
test_that("ecotaxa_volumes() for Zooscan divides sample_tot_vol by acq_sub_part, and gives none without it", {
  o <- fixture_objects()
  expect_true(all(is.na(ecotaxa_volumes(o)$imaged_m3)))
  o$acq_sub_part <- "4"
  v <- ecotaxa_volumes(o)
  expect_equal(v$imaged_m3[v$acq_id == "a1"], 0.5)
  expect_equal(v$sample_m3[v$acq_id == "a1"], 2)
})
test_that("dwc_event() and dwc_emof() report sampled and imaged volume, and concentration per imaged volume", {
  o <- planktoscope_objects()
  ev <- dwc_event(o, "p4", coldict = ps_units); oc <- dwc_occurrence(o, ev, lk, "p4")
  expect_equal(ev$sampleSizeValue[ev$eventID == "p4:s1"], 40.3)
  expect_equal(ev$sampleSizeValue[ev$eventID == "p4:s1:a1"], signif(40.3 * 0.6242 / 584, 6))
  em <- dwc_emof(o, ev, oc, reg, "p4", coldict = ps_units)
  img <- em[em$measurementType == "imaged volume", ]
  expect_equal(img$eventID, "p4:s1:a1")                          # only events that hold occurrences
  conc <- em[em$measurementType == "concentration", ]
  expect_equal(nrow(conc), nrow(oc))
  expect_equal(as.numeric(conc$measurementValue[conc$occurrenceID == "p4:s1:a1:1101:validated"]),
               signif(1 / (40.3 * 0.6242e-6 / 584e-6), 6))       # 23.2 individuals per m3
  expect_equal(unique(conc$measurementUnit), "individuals per m3")
  expect_match(conc$measurementMethod[1], "^object count / imaged volume; PlanktoScope")
  expect_equal(em$measurementValue[em$measurementType == "sample volume" & em$eventID == "p4:s2"], "20")
  expect_equal(sum(duplicated(em$measurementID)), 0L)
  expect_false(any(dwc_check(ev, oc, em)$level == "error"))
})
test_that("dwc_event() reads the PlanktoScope sampling gear column too", {
  o <- planktoscope_objects() |> dplyr::select(-sample_samplinggear) |> dplyr::mutate(sample_sampling_gear = "net")
  ev <- dwc_event(o, "p4", coldict = ps_units)
  expect_equal(ev$samplingProtocol[ev$eventID == "p4:s1"], "PlanktoScope v2.5; net")
})
test_that("volume_review() flags dilution, missing inputs and outlying totals per sample (regression: project 4)", {
  o <- planktoscope_objects()
  o$sample_total_volume[o$sample_id == "s2"] <- "2557544141"
  o <- dplyr::bind_rows(o, o[5, ] |> dplyr::mutate(object_id = "o9", sample_id = "s3", acq_id = "a9",
                                                   sample_total_volume = "30000", sample_dilution_factor = "-0.5"),
                        o[5, ] |> dplyr::mutate(object_id = "o10", sample_id = "s4", acq_id = "a10",
                                                sample_total_volume = "35000", sample_dilution_factor = "1"))
  r <- volume_review(o, ps_units)
  expect_equal(nrow(r), 4L)
  expect_match(r$finding[r$sample_id == "s2"], "dilution factor not 1; total volume more than 10x off")
  expect_equal(r$finding[r$sample_id == "s3"], "dilution factor not positive")
  expect_equal(r$finding[r$sample_id == "s1"], "")
  expect_equal(r$sample_id[1:2], c("s2", "s3"))                  # findings first
  # a missing dilution is missing, not "not positive" (regression: flags read the text column)
  o$sample_dilution_factor[o$sample_id == "s4"] <- NA
  expect_equal(volume_review(o, ps_units)$finding[volume_review(o, ps_units)$sample_id == "s4"],
               "dilution factor missing (1 assumed)")
})
test_that("volumes = FALSE withholds sampled and imaged volume and concentration, keeps counts", {
  o <- planktoscope_objects()
  ev <- dwc_event(o, "p4", coldict = ps_units, volumes = FALSE); oc <- dwc_occurrence(o, ev, lk, "p4")
  expect_false("sampleSizeValue" %in% names(ev))
  em <- dwc_emof(o, ev, oc, reg, "p4", coldict = ps_units, volumes = FALSE)
  expect_false(any(c("sample volume", "imaged volume", "concentration") %in% em$measurementType))
  expect_true("object count" %in% em$measurementType)
})
