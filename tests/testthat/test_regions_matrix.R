test_that("regions_matrix works", {

  # Expect error
  expect_error(regions_matrix())

  expect_error(regions_matrix(config = "files"))

})
