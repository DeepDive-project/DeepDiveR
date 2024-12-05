test_that("prep_dd_input works", {
  # Test dataset
  dat <- data.frame(Taxon = c("A", "B", "C", "D", "E"),
                    Region = c("Europe", "Europe", "Asia", "Asia", "Asia"),
                    MinAge = c(0, 0, 5, 10, 10),
                    MaxAge = c(5, 10, 10, 20, 20),
                    Locality = c(1, 2, 3, 4, 4))

  bins <- c(66, 23, 2.6, 0)

  # Expect warning
  expect_warning(prep_dd_input(dat = dat, bins = bins, r = 2,
                               age_m = "median",
                               output_file = "test.csv"))

  # Expect error
  expect_error(prep_dd_input())

  dat$MinAge <- c(NA, 0, 5, 10, 10)
  expect_error(prep_dd_input(dat = dat))

  dat$MinAge <- c(10, 0, 5, 10, 10)
  expect_error(prep_dd_input(dat = dat))

  dat$MinAge <- c(0, 0, 5, 10, 10)
  expect_error(prep_dd_input(dat = dat, bins = "test"))

  expect_error(prep_dd_input(dat = dat, bins = c("Taxon", "Min", "MaxAge")))

  colnames(dat) <- c("Taxon", "test", "MinAge", "MaxAge", "Locality")
  expect_error(prep_dd_input(dat = dat, bins = bins))

  dat$MinAge <- as.factor(dat$MinAge)
  expect_error(prep_dd_input(dat = dat, bins = bins))

  colnames(dat) <- c("Taxon", "Region", "MinAge", "MaxAge", "Locality")
  expect_error(prep_dd_input(dat = dat, bins = bins, r = "test"))

  colnames(dat) <- c("Taxon", "Region", "MinAge", "MaxAge", "Loc")
  expect_error(prep_dd_input(dat = dat, age_m = "random_by_loc"))

  expect_error(prep_dd_input(dat = dat, bins = bins, output_file = dat))

  output_file <- "test.txt"
  expect_error(prep_dd_input(dat = dat, bins = bins, output_file = output_file))
})
