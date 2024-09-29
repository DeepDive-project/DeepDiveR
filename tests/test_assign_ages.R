test_that("assign_ages works", {
  # Test dataset
  dat <- data.frame(Taxa = c("A", "B", "C", "D", "E"),
                    MinAge = c(0, 0, 5, 10, 10),
                    MaxAge = c(5, 10, 10, 20, 20),
                    Locality = c(1, 2, 3, 4, 4))

  # Expect equal
  expect_equal(nrow(ages(dat = dat)), nrow(dat))
  expect_equal(ncol(ages(dat = dat)), (ncol(dat) + 1))
  expect_equal(nrow(ages(dat = dat, method = "random")), nrow(dat))
  expect_equal(ncol(ages(dat = dat, method = "random")), (ncol(dat) + 1))
  expect_equal(nrow(ages(dat = dat, method = "random_by_loc")), nrow(dat))
  expect_equal(ncol(ages(dat = dat, method = "random_by_loc")), (ncol(dat) + 1))

  # Expect error
  expect_error(ages())

  expect_error(ages(dat = dat, method = "test"))

  colnames(dat) <- c("Taxa", "Min", "MaxAge", "Locality")
  expect_error(ages(dat = dat))

  colnames(dat) <- c("Taxa", "MinAge", "MaxAge", "Locality")
  dat$MinAge <- as.factor(dat$MinAge)
  expect_error(ages(dat = dat))

  dat$MinAge <- c(NA, 0, 5, 10, 10)
  expect_error(ages(dat = dat))

  dat$MinAge <- c(10, 0, 5, 10, 10)
  expect_error(ages(dat = dat))

  dat$MinAge <- c(0, 0, 5, 10, 10)
  colnames(dat) <- c("Taxa", "MinAge", "MaxAge", "Loc")
  expect_error(ages(dat = dat, method = "random_by_loc"))
})
