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
