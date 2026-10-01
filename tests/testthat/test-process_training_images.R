test_that("process_training_images validates required metadata columns", {
  td <- tempdir()
  meta_path <- file.path(td, paste0("meta-missing-", Sys.getpid(), ".csv"))
  readr::write_csv(data.frame(FileName = "a.jpg"), meta_path)

  expect_error(
    process_training_images(
      metadata_file = meta_path,
      dest_dir = file.path(td, "out-a"),
      output_sample_file = "sample",
      output_annotation_file = "ann",
      copy_images = FALSE
    ),
    "must contain at least 'Directory' and 'FileName'"
  )
})

test_that("process_training_images validates sample_proportion and filters", {
  td <- file.path(tempdir(), paste0("pti-", Sys.getpid(), "-", as.integer(Sys.time())))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  src <- file.path(td, "src")
  dir.create(src)

  f1 <- make_temp_image_file(src, "a.jpg")
  f2 <- make_temp_image_file(src, "b.jpg")

  md <- data.frame(
    Directory = src,
    FileName = basename(c(f1, f2)),
    score = c(1, 2)
  )

  meta_path <- file.path(td, "meta.csv")
  make_training_metadata(meta_path, md)

  expect_error(
    process_training_images(
      meta_path, file.path(td, "out1"), "sample", "ann",
      sample_proportion = 2,
      copy_images = FALSE
    ),
    "numeric value in \\(0, 1\\]"
  )

  expect_error(
    process_training_images(
      meta_path, file.path(td, "out2"), "sample", "ann",
      filters = list(list(var = "missing_col", min = 0, max = 1)),
      copy_images = FALSE
    ),
    "was not found"
  )
})

test_that("process_training_images writes expected csv outputs and optionally copies", {
  set.seed(123)
  td <- file.path(tempdir(), paste0("pti-ok-", Sys.getpid(), "-", as.integer(Sys.time())))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  src1 <- file.path(td, "camA")
  src2 <- file.path(td, "camB")
  dir.create(src1)
  dir.create(src2)

  a1 <- make_temp_image_file(src1, "a1.jpg")
  a2 <- make_temp_image_file(src1, "a2.jpg")
  b1 <- make_temp_image_file(src2, "b1.jpg")

  md <- data.frame(
    Directory = c(src1, src1, src2),
    FileName = basename(c(a1, a2, b1)),
    File_hour = c(8, 14, 9)
  )
  meta_path <- file.path(td, "meta.csv")
  make_training_metadata(meta_path, md)

  out_dir <- file.path(td, "out")
  res <- process_training_images(
    metadata_file = meta_path,
    dest_dir = out_dir,
    output_sample_file = "sample",
    output_annotation_file = "annotation",
    sample_proportion = 0.5,
    filters = list(list(var = "File_hour", min = 7, max = 12)),
    min_per_dir = 1,
    max_per_dir = 1,
    copy_images = TRUE,
    show_progress = FALSE
  )

  expect_true(file.exists(file.path(out_dir, "sample.csv")))
  expect_true(file.exists(file.path(out_dir, "annotation.csv")))
  expect_true("file_path" %in% names(res))
  expect_true("copied" %in% names(res))
  expect_true(all(res$copied))

  copied_files <- file.path(out_dir, res$FileName)
  expect_true(all(file.exists(copied_files)))
})
