tbl <-
  cards::ADAE[1:150, ] |>
  gtsummary::tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
  )


test_that("save_docx() works with flextable", {
  skip_if_not_installed("flextable")
  # test with a single table
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    gtsummary::as_flex_table(tbl) |>
      save_docx(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # test with a list of tables
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    gtsummary::tbl_split_by_rows(tbl, row_numbers = 20) |>
      map(gtsummary::as_flex_table) |>
      save_docx(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})

test_that("save_docx() works with gtsummary table", {
  # test with a single table
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    res <- tbl |>
      save_docx(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  expect_match(regexp = "*.reference_docx.*", res[4])
  expect_match(regexp = "library(gtsummary)", res[9], fixed = TRUE)
  expect_match(regexp = "print_obj(x[[1]])", res[19], fixed = TRUE)

  # test with a list of tables
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    res <- gtsummary::tbl_split_by_rows(tbl, row_numbers = 20) |>
      save_docx(path = file_path),
    NA
  )

  expect_true(file.exists(file_path))

  # Read the contents of the temporary Rmd file
  expect_match(regexp = "*.reference_docx.*", res[4])
  expect_match(regexp = "library(gtsummary)", res[9], fixed = TRUE)
  expect_match(regexp = "print_obj(x[[1]])", res[19], fixed = TRUE)
  expect_match(regexp = "print_obj(x[[2]])", res[25], fixed = TRUE)
})

test_that("save_docx() fails with incorrect inputs", {
  expect_snapshot(
    save_docx(),
    error = TRUE
  )

  expect_snapshot(
    save_docx(x = tbl),
    error = TRUE
  )

  expect_snapshot(
    save_docx(x = tbl, path = 123),
    error = TRUE
  )

  expect_snapshot(
    save_docx(x = "not_a_table", path = tempfile(fileext = ".docx")),
    error = TRUE
  )

  expect_snapshot(
    save_docx(list("a", "b"), "a"),
    error = TRUE
  )
})

test_that("save_docx() works with gt_tbl-list", {
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    gtsummary::as_gt(tbl) |>
      gt::cols_width(
        dplyr::everything() ~ gt::px(100)
      ) |>
      save_docx(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})

test_that("save_docx() works with figures", {
  library(ggplot2)
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  file_path <- tempfile(fileext = ".docx")
  expect_error(
    save_docx(p1, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # test with grid object
  p2 <- grid::circleGrob()
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    save_docx(p2, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # list of mixed objects
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    list(p1, p2) |> save_docx(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})
