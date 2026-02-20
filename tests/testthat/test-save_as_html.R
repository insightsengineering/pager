tbl <-
  cards::ADAE[1:150, ] |>
  gtsummary::tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
  )

gt_tbl <- gtsummary::as_gt(tbl)

test_that("save_html() works with gt table", {
  # test with a single gt table
  file_path <- tempfile(fileext = ".html")
  expect_error(
    res <- gt_tbl |>
      save_html(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  expect_match(regexp = "html_document", res[3], fixed = TRUE)
  expect_match(regexp = "self_contained: true", res[4], fixed = TRUE)
  expect_match(regexp = "library(gt)", res[9], fixed = TRUE)
  expect_match(regexp = "x[[1]]", res[16], fixed = TRUE)
})

test_that("save_html() works with a list of gt tables", {
  file_path <- tempfile(fileext = ".html")
  expect_error(
    res <- gtsummary::tbl_split_by_rows(tbl, row_numbers = 20) |>
      map(gtsummary::as_gt) |>
      save_html(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  expect_match(regexp = "x[[1]]", res[16], fixed = TRUE)
  expect_match(regexp = "x[[2]]", res[22], fixed = TRUE)
})

test_that("save_html() works with gtsummary table", {
  file_path <- tempfile(fileext = ".html")
  expect_error(
    tbl |>
      save_html(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})

test_that("save_html() works with flextable", {
  skip_if_not_installed("flextable")
  # test with a single flextable
  file_path <- tempfile(fileext = ".html")
  expect_error(
    gtsummary::as_flex_table(tbl) |>
      save_html(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # test with a list of flextables
  file_path <- tempfile(fileext = ".html")
  expect_error(
    gtsummary::tbl_split_by_rows(tbl, row_numbers = 20) |>
      map(gtsummary::as_flex_table) |>
      save_html(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})

test_that("save_html() works with figures", {
  library(ggplot2)
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  file_path <- tempfile(fileext = ".html")
  expect_error(
    save_html(p1, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # test with grid object
  p2 <- grid::circleGrob()
  file_path <- tempfile(fileext = ".html")
  expect_error(
    save_html(p2, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # list of mixed objects
  file_path <- tempfile(fileext = ".html")
  expect_error(
    list(p1, p2) |> save_html(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})

test_that("save_html() works with custom CSS", {
  file_path <- tempfile(fileext = ".html")
  expect_error(
    res <- gt_tbl |>
      save_html(
        path = file_path,
        css = "body { font-family: Arial; font-size: 12px; }"
      ),
    NA
  )
  expect_true(file.exists(file_path))

  # css: line should be present in the yaml header
  expect_true(any(grepl("css:", res, fixed = TRUE)))
})

test_that("save_html() fails with incorrect inputs", {
  expect_error(
    save_html(),
    "The `x` argument cannot be missing."
  )

  expect_error(
    save_html(x = tbl),
    "The `path` argument cannot be missing."
  )

  expect_error(
    save_html(x = tbl, path = 123),
    "The `path` argument must be a string, not a number."
  )

  expect_error(
    save_html(x = "not_a_table", path = tempfile(fileext = ".html")),
    "<gg/ggplot/grob/gtsummary/gt_tbl/flextable/list>"
  )

  expect_error(
    save_html(list("a", "b"), "a"),
    "each list element must be one of the following classes"
  )

  expect_error(
    save_html(x = gt_tbl, path = tempfile(fileext = ".html"), css = 123),
    "The `css` argument must be a string, not a number."
  )
})
