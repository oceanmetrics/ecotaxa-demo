test_that("taxa_decide() applies the four rules", {
  d <- taxa_decide(fixture_taxa())
  dec <- stats::setNames(d$decision, d$ecotaxa_name)
  expect_equal(dec[["Cyclopoida"]], "aphia")
  expect_equal(dec[["Oncaeidae"]], "aphia")
  expect_equal(dec[["detritus<not-living"]], "excluded")   # non-living root
  expect_equal(dec[["t001"]], "excluded")                  # temporary root
  expect_equal(dec[["egg<other"]], "excluded")             # morphological; ancestor is only Biota
  expect_equal(dec[["other<Biota"]], "excluded")
  expect_equal(d$aphia_id[d$ecotaxa_name == "Cyclopoida"], 1101L)
  expect_match(d$note[d$ecotaxa_name == "egg<other"], "Biota \\(Superdomain\\)")
})
test_that("taxa_decide() publishes a morphological category under a useful ancestor", {
  t <- fixture_taxa()
  t <- rbind(t, tibble::tibble(ecotaxa_id = 99L, name = "nauplii", display_name = "nauplii<Cyclopoida", type = "M",
                               rank = NA, aphia_id = NA, lineage = "Biota > ... > Cyclopoida > nauplii", id_lineage = "1,45072,99"))
  d <- taxa_decide(t)
  r <- d[d$ecotaxa_id == 99, ]
  expect_equal(r$decision, "ancestor"); expect_equal(r$aphia_id, 1101L)
})
test_that("taxa_decide() asks for a name match when a P category has no aphia_id", {
  t <- fixture_taxa()[2, ]; t$aphia_id <- NA_integer_
  expect_equal(taxa_decide(t)$decision, "name_match")
})
test_that("taxa_worms_fill() fills from WoRMS and marks a failed name match unresolved", {
  t <- fixture_taxa()
  t <- rbind(t, tibble::tibble(ecotaxa_id = 7L, name = "Calanoida", display_name = "Calanoida", type = "P", rank = "Order",
                               aphia_id = NA, lineage = "Biota > Calanoida", id_lineage = "1,7"),
             tibble::tibble(ecotaxa_id = 8L, name = "Nonsense", display_name = "Nonsense", type = "P", rank = "Genus",
                            aphia_id = NA, lineage = "Biota > Nonsense", id_lineage = "1,8"))
  lk <- build_taxa_lookup(t, by_id = fake_worms$by_id, by_name = fake_worms$by_name)
  expect_equal(lk$scientific_name[lk$ecotaxa_id == 78418], "Oncaeidae")
  expect_equal(lk$lsid[lk$ecotaxa_id == 45072], "urn:lsid:marinespecies.org:taxname:1101")
  expect_equal(lk$decision[lk$ecotaxa_id == 7], "name_match"); expect_equal(lk$aphia_id[lk$ecotaxa_id == 7], 1100L)
  expect_equal(lk$decision[lk$ecotaxa_id == 8], "unresolved")
  expect_true(is.na(lk$aphia_id[lk$ecotaxa_id == 84963]))
  # round trip through the reviewable csv
  p <- tempfile(fileext = ".csv"); write_taxa_lookup(lk, p); lk2 <- read_taxa_lookup(p)
  expect_equal(lk2$decision, lk$decision); expect_equal(lk2$aphia_id, lk$aphia_id)
})
test_that("assert_taxa_resolved() stops on a category missing from the lookup", {
  lk <- build_taxa_lookup(fixture_taxa(), by_id = fake_worms$by_id, by_name = fake_worms$by_name)
  expect_true(assert_taxa_resolved(fixture_objects(), lk))
  o <- fixture_objects(); o$object_annotation_category_id[1] <- 12345L
  expect_error(assert_taxa_resolved(o, lk), "absent from the lookup")
})
test_that("ecotaxa_category_ids() maps exported display names to ids and refuses a guess", {
  taxa <- tibble::tibble(ecotaxa_id = c(45072L, 84963L), display_name = c("Cyclopoida", "detritus"))
  expect_equal(ecotaxa_category_ids(c("detritus", "Cyclopoida", NA), taxa), c(84963L, 45072L, NA))
  expect_error(ecotaxa_category_ids("Oncaeidae", taxa), "not among the project's categories: Oncaeidae")
  taxa2 <- tibble::tibble(ecotaxa_id = 1:2, display_name = c("egg", "egg"))
  expect_error(ecotaxa_category_ids("egg", taxa2), "shared by two categories")
})
test_that("taxa_apply_review() excludes and flags as the reviewer decided, idempotently", {
  lk <- build_taxa_lookup(fixture_taxa(), by_id = fake_worms$by_id, by_name = fake_worms$by_name)
  rv <- tibble::tibble(project_id = 2L, ecotaxa_id = c(78418L, 45072L), ecotaxa_name = c("Oncaeidae", "Cyclopoida"),
                       decision = c("excluded", "verify"), note = c("not wanted", "check the order"),
                       reviewed_by = "A Person", reviewed_on = "2026-11-11")
  x <- taxa_apply_review(lk, rv)
  onc <- x[x$ecotaxa_id == 78418L, ]
  expect_equal(onc$decision, "excluded"); expect_true(is.na(onc$aphia_id)); expect_true(is.na(onc$lsid))
  expect_equal(onc$note, "review 2026-11-11 (A Person): not wanted")
  cyc <- x[x$ecotaxa_id == 45072L, ]
  expect_equal(cyc$decision, lk$decision[lk$ecotaxa_id == 45072L]); expect_equal(cyc$aphia_id, 1101L)
  expect_match(cyc$note, "; TO VERIFY, review 2026-11-11 \\(A Person\\): check the order$")
  expect_identical(taxa_apply_review(x, rv), x)
  # an excluded category is no longer an occurrence
  oc <- dwc_occurrence(fixture_objects(), dwc_event(fixture_objects(), "p2"), x, "p2")
  expect_false("Oncaeidae" %in% oc$scientificName)
  expect_error(taxa_apply_review(lk, transform(rv[1, ], ecotaxa_id = 999L)), "absent from the lookup: 999")
  expect_error(taxa_apply_review(lk, transform(rv[1, ], decision = "maybe")), "unknown review decision: maybe")
})
test_that("the project 4 review excludes part<Crustacea and multiple<Copepoda in the committed lookup", {
  lk4 <- read_taxa_lookup(file.path(root, "metadata/taxa_lookup_project-4.csv"))
  rv4 <- read_taxa_review(file.path(root, "metadata/taxa_review.csv"), 4)
  expect_identical(taxa_apply_review(lk4, rv4), lk4)            # already applied
  ex <- lk4$decision[match(c("part<Crustacea", "multiple<Copepoda", "rods<other", "microplastics"), lk4$ecotaxa_name)]
  expect_equal(ex, rep("excluded", 4))
  expect_true(all(grepl("TO VERIFY", lk4$note[lk4$ecotaxa_name %in% c("centric", "ciliate-tintinnids", "tintinnida")])))
})
