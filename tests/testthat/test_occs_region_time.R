test_that("occs_region_time works", {
  # Test dataset
  dat <- data.frame(Taxon = c("A", "B", "C", "D", "E"),
                    Region = c("Europe", "Europe", "Asia", "Asia", "Asia"),
                    Locality = c(1, 2, 3, 4, 4),
                    SampledAge = c(0.2, 0.7, 4.6, 10.2, 10.2))

  bins <- c(66, 23, 2.6, 0)

  # Expect equal
  expect_equal(nrow(localities_through_time(dat = dat, bins = bins)),
               length(unique(dat$Region)))

  # Expect error
  expect_error(occs_region_time())

  dat$Locality <- c(NA, 2, 3, 4, 4)
  expect_error(occs_region_time(dat = dat))

  dat$Locality <- c(1, 2, 3, 4, 4)
  expect_error(occs_region_time(dat = dat, bins = "test"))

  expect_error(occs_region_time(dat = dat, bins = c("Taxon", "Min",
                                                  "MaxAge")))

  colnames(dat) <- c("Taxon", "test", "Locality", "SampledAge")
  expect_error(occs_region_time(dat = dat, bins = bins))

  colnames(dat) <- c("Taxon", "Region", "Locality", "SampledAge")
  dat$SampledAge <- as.factor(dat$SampledAge)
  expect_error(occs_region_time(dat = dat, bins = bins))
})
