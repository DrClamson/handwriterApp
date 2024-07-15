testthat::test_that("create directory works", {
  create_dir(file.path(tempdir(), "new"))
  testthat::expect_true(dir.exists(file.path(tempdir(), "new")))
  
  # delete new directory
  unlink(file.path(tempdir(), "new"), recursive=TRUE)
})
