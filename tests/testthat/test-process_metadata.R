test_that("process_metadata validates output path and parent directory", {
  root <- file.path(tempdir(), paste0("cam2model-meta-", Sys.getpid(), "-", as.integer(Sys.time())))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  expect_error(
    process_metadata(root, ""),
    "non-empty output file path"
  )

  expect_error(
    process_metadata(root, "/abs/path.csv"),
    "must be a relative path"
  )

  expect_error(
    process_metadata(root, file.path("missing", "out.csv")),
    "output directory does not exist"
  )
})

test_that("process_metadata works on empty camera directories and writes CSV", {
  root <- file.path(tempdir(), paste0("cam2model-meta-ok-", Sys.getpid(), "-", as.integer(Sys.time())))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "camera_1"), recursive = TRUE, showWarnings = FALSE)

  out_file <- file.path("exports", "meta.csv")
  dir.create(file.path(root, "exports"), recursive = TRUE, showWarnings = FALSE)

  res <- process_metadata(root, out_file, UserComment = FALSE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)

  written <- file.path(root, out_file)
  expect_true(file.exists(written))
})
