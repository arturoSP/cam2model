test_that("rename_images returns result tibble by default", {
  fake_plan <- tibble::tibble(
    original_path = "in/a.jpg",
    new_name = "new_a.jpg",
    new_path = "out/new_a.jpg"
  )
  fake_result <- dplyr::mutate(fake_plan, copied = TRUE)

  testthat::local_mocked_bindings(
    build_rename_plan = function(...) fake_plan,
    execute_rename_plan = function(plan_tbl, overwrite = FALSE) {
      expect_identical(plan_tbl, fake_plan)
      expect_false(overwrite)
      fake_result
    }
  )

  out <- rename_images("in", "out")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("original_path", "new_name", "new_path", "copied"))
})

test_that("rename_images can return plan and result", {
  fake_plan <- tibble::tibble(
    original_path = "in/a.jpg",
    new_name = "new_a.jpg",
    new_path = "out/new_a.jpg"
  )
  fake_result <- dplyr::mutate(fake_plan, copied = FALSE)

  testthat::local_mocked_bindings(
    build_rename_plan = function(...) fake_plan,
    execute_rename_plan = function(plan_tbl, overwrite = FALSE) fake_result
  )

  out <- rename_images("in", "out", return_plan = TRUE)
  expect_type(out, "list")
  expect_named(out, c("plan", "result"))
  expect_identical(out$plan, fake_plan)
  expect_identical(out$result, fake_result)
})
