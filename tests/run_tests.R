# Rscript tests/run_tests.R
librarian::shelf(testthat, quiet = TRUE)
res <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
