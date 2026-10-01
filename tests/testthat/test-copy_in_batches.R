test_that("copy_in_batches validates inputs and required columns", {
  expect_error(copy_in_batches(df = 1:3, output_dir = tempdir()), "must be a data.frame")

  bad_df <- data.frame(id = "a")
  expect_error(
    copy_in_batches(df = bad_df, output_dir = tempdir()),
    "Missing required columns"
  )

  good_df <- data.frame(id = "a", path = tempfile())
  expect_error(
    copy_in_batches(df = good_df, output_dir = tempdir(), batch_size = 0),
    "positive integer"
  )
})

test_that("copy_in_batches supports dry_run without copying files", {
  td <- file.path(tempdir(), paste0("cib-dry-", Sys.getpid(), "-", as.integer(Sys.time())))
  src <- file.path(td, "src")
  out <- file.path(td, "out")
  dir.create(src, recursive = TRUE, showWarnings = FALSE)

  f1 <- make_temp_image_file(src, "img1.jpg")
  df <- data.frame(id = "ID1", path = f1, stringsAsFactors = FALSE)

  report <- copy_in_batches(
    df = df,
    output_dir = out,
    batch_size = 1,
    dry_run = TRUE,
    verbose = FALSE
  )

  expect_equal(report$status, "dry_run")
  expect_true(is.na(report$copied))
  expect_false(file.exists(report$dest_path))
})

test_that("copy_in_batches copies files and reports missing sources", {
  td <- file.path(tempdir(), paste0("cib-ok-", Sys.getpid(), "-", as.integer(Sys.time())))
  src <- file.path(td, "src")
  out <- file.path(td, "out")
  dir.create(src, recursive = TRUE, showWarnings = FALSE)

  f1 <- make_temp_image_file(src, "img1.jpg")
  missing <- file.path(src, "missing.jpg")

  df <- data.frame(
    id = c("ID1", "ID2"),
    path = c(f1, missing),
    stringsAsFactors = FALSE
  )

  report <- copy_in_batches(
    df = df,
    output_dir = out,
    batch_size = 1,
    rename_with_id = TRUE,
    dry_run = FALSE,
    verbose = FALSE
  )

  expect_true(any(report$status == "copied"))
  expect_true(any(report$status == "source_not_found"))
  copied_idx <- which(report$status == "copied")
  expect_true(file.exists(report$dest_path[copied_idx]))
})

