gt_tbl_single <- gt::gt(head(mtcars))
gt_tbl_list <- list(gt::gt(head(mtcars)), gt::gt(tail(mtcars)))

test_that("save_txt() works with a single gt_tbl", {
  file_path <- tempfile(fileext = ".txt")
  expect_error(
    res <- gt_tbl_single |>
      save_txt(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # YAML header uses md_document output
  expect_match(res[3], "md_document", fixed = TRUE)
  # setup chunk attaches gt
  expect_match(res[9], "library(gt)", fixed = TRUE)
  # first table chunk renders correctly
  expect_match(res[19], "as_kable_txt(x[[1]])", fixed = TRUE)
})

test_that("save_txt() works with a list of gt_tbl objects", {
  file_path <- tempfile(fileext = ".txt")
  expect_error(
    res <- gt_tbl_list |>
      save_txt(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # YAML header uses md_document output
  expect_match(res[3], "md_document", fixed = TRUE)
  # setup chunk attaches gt
  expect_match(res[9], "library(gt)", fixed = TRUE)
  # both table chunks rendered
  expect_match(res[19], "as_kable_txt(x[[1]])", fixed = TRUE)
  expect_match(res[25], "as_kable_txt(x[[2]])", fixed = TRUE)
})

test_that("save_txt() fails with incorrect inputs", {
  expect_snapshot(
    save_txt(),
    error = TRUE
  )

  expect_snapshot(
    save_txt(x = gt_tbl_single),
    error = TRUE
  )

  expect_snapshot(
    save_txt(x = gt_tbl_single, path = 123),
    error = TRUE
  )

  expect_snapshot(
    save_txt(x = "not_a_table", path = tempfile(fileext = ".txt")),
    error = TRUE
  )

  expect_snapshot(
    save_txt(list("a", "b"), path = tempfile(fileext = ".txt")),
    error = TRUE
  )
})

test_that("save_txt() fails with non-gt_tbl objects", {
  # flextable is not supported for txt output
  skip_if_not_installed("flextable")
  ft <- flextable::flextable(head(mtcars))
  expect_snapshot(
    save_txt(ft, path = tempfile(fileext = ".txt")),
    error = TRUE
  )
})

test_that("save_txt() works with .md extension", {
  file_path <- tempfile(fileext = ".md")
  expect_error(
    gt_tbl_single |>
      save_txt(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))
})
