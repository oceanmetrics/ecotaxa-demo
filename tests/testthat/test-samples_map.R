samples <- function() tibble::tibble(
  orig_id = c("ok", "zero", "no_lat", "equator", "greenwich"),
  lat     = c(24.5, 0,      NA,       0,         51.5),
  lon     = c(-81,  0,      -81,      -30,       0))

test_that("0,0 and missing coordinates are dropped, with the reason", {
  x <- split_mappable_samples(samples())
  expect_equal(x$dropped$orig_id, c("zero", "no_lat"))
  expect_equal(x$dropped$reason, c("latitude and longitude both 0", "missing coordinates"))
})

test_that("a latitude or longitude of 0 alone is a real place and is kept", {
  expect_equal(split_mappable_samples(samples())$mapped$orig_id, c("ok", "equator", "greenwich"))
})

test_that("nothing dropped gives an empty dropped table", {
  x <- split_mappable_samples(samples()[1, ])
  expect_equal(nrow(x$dropped), 0)
  expect_equal(nrow(x$mapped), 1)
})

test_that("the caption counts samples and names each dropped one", {
  x <- split_mappable_samples(samples())
  expect_equal(
    samples_map_caption(x$mapped, x$dropped, "project 4"),
    paste0("Positions of 3 of the 5 samples in project 4. Dropped from the map because the position is not known: ",
           "zero (latitude and longitude both 0 in the source data); no_lat (missing coordinates in the source data)."))
})

test_that("the caption has no dropped clause when every sample maps", {
  x <- split_mappable_samples(samples()[1, ])
  expect_equal(samples_map_caption(x$mapped, x$dropped, "project 4"), "Positions of 1 of the 1 samples in project 4.")
})
