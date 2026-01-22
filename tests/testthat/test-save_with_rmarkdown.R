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
