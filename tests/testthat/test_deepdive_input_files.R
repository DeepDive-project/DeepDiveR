test_that("prep_dd_input works", {
  # Test dataset
  dat <- data.frame(Taxon = c("A", "B", "C", "D", "E"),
                    Area = c("Europe", "Europe", "Asia", "Asia", "Asia"),
                    MinAge = c(0, 0, 5, 10, 10),
                    MaxAge = c(5, 10, 10, 20, 20),
                    Locality = c(1, 2, 3, 4, 4))

  bins <- c(66, 23, 2.6, 0)

  # Expect equal
  #expect_equal(nrow(prep_dd_input(dat = dat)), nrow(dat))

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

  colnames(dat) <- c("Taxon", "Area", "MinAge", "MaxAge", "Locality")
  expect_error(prep_dd_input(dat = dat, bins = bins, r = "test"))

  colnames(dat) <- c("Taxon", "Area", "MinAge", "MaxAge", "Loc")
  expect_error(ages(dat = dat, method = "random_by_loc"))
})
