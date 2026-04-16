test_that("generate_plot validates required inputs", {
  expect_error(generate_plot(NULL, x = a, y = b), "must be a data frame")

  df <- data.frame(a = 1:3, b = 3:1)
  expect_error(generate_plot(df, y = b), "must be specified")
  expect_error(generate_plot(df, x = a), "must be specified")
})

test_that("generate_plot returns ggplot and supports filter/color/facets", {
  df <- data.frame(
    when = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 01:00:00", "2024-01-01 02:00:00"), tz = "UTC"),
    value = c(1, 2, 3),
    camera = c("A", "A", "B")
  )

  p <- generate_plot(
    processed_metadata = df,
    x = when,
    y = value,
    color = camera,
    facets = camera,
    filter_expr = value >= 2,
    angle = 45
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$theme$axis.text.x$angle, 45)
  expect_equal(length(p$layers), 2)
})
