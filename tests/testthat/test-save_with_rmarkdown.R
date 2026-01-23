test_that("save_with_rmarkdown() works with flextable", {
  tbl <-
    cards::ADAE[1:150,] |>
    gtsummary::tbl_hierarchical(
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID,
    )

  # test with a single table
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    gtsummary::as_flex_table(tbl) |>
      save_with_rmarkdown(, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # test with a list of tables
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    gtsummary::tbl_split_by_rows(tbl, row_numbers = 20) |>
      map(gtsummary::as_flex_table) |>
      save_with_rmarkdown(, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})

test_that("save_with_rmarkdown() works with gtsummary table", {
  tbl <-
    cards::ADAE[1:150,] |>
    gtsummary::tbl_hierarchical(
      variables = c(AESOC, AETERM),
      by = TRTA,
      denominator = cards::ADSL,
      id = USUBJID,
    )

  # test with a single table
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    tbl |>
      save_with_rmarkdown(, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # test with a list of tables
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    gtsummary::tbl_split_by_rows(tbl, row_numbers = 20) |>
      save_with_rmarkdown(, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})

test_that("save_with_rmarkdown() works with figures", {
  library(ggplot2)
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

  file_path <- tempfile(fileext = ".docx")
  expect_error(
    save_with_rmarkdown(p1, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # test with grid object
  p2 <- grid::circleGrob()
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    save_with_rmarkdown(p2, path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # list of mixed objects
  file_path <- tempfile(fileext = ".docx")
  expect_error(
    list(p1, p2) |> save_with_rmarkdown(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})
