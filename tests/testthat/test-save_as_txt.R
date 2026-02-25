gt_tbl_single <- gt::gt(head(mtcars))
gt_tbl_list <- list(gt::gt(head(mtcars)), gt::gt(tail(mtcars)))
gtsummary_single <- gtsummary::tbl_summary(gtsummary::trial[, c("age", "trt")])
gtsummary_list <- list(
  gtsummary::tbl_summary(gtsummary::trial[, c("age", "trt")]),
  gtsummary::tbl_summary(gtsummary::trial[, c("grade", "trt")])
)

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
  expect_match(res[23], "as_kable_txt(x[[1]])", fixed = TRUE)
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
  expect_match(res[23], "as_kable_txt(x[[1]])", fixed = TRUE)
  expect_match(res[29], "as_kable_txt(x[[2]])", fixed = TRUE)
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

test_that("save_txt() fails with non-gt_tbl/gtsummary objects", {
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

test_that("save_txt() works with a single gtsummary object", {
  skip_if_not_installed("gtsummary")
  file_path <- tempfile(fileext = ".txt")
  expect_error(
    res <- gtsummary_single |>
      save_txt(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # YAML header uses md_document output
  expect_match(res[3], "md_document", fixed = TRUE)
  # setup chunk attaches both gt and gtsummary
  expect_match(res[9], "library(gt)", fixed = TRUE)
  expect_match(res[10], "library(gtsummary)", fixed = TRUE)
  # as_kable_txt dispatches on gtsummary class
  expect_match(res, "gtsummary::as_kable(obj)", fixed = TRUE, all = FALSE)
  # first table chunk renders correctly
  expect_match(res[length(res) - 1L], "as_kable_txt(x[[1]])", fixed = TRUE)
})

test_that("save_txt() works with a list of gtsummary objects", {
  skip_if_not_installed("gtsummary")
  file_path <- tempfile(fileext = ".txt")
  expect_error(
    res <- gtsummary_list |>
      save_txt(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # both gt and gtsummary attached
  expect_match(res[9], "library(gt)", fixed = TRUE)
  expect_match(res[10], "library(gtsummary)", fixed = TRUE)
  # both table chunks rendered
  expect_match(res, "as_kable_txt(x[[1]])", fixed = TRUE, all = FALSE)
  expect_match(res, "as_kable_txt(x[[2]])", fixed = TRUE, all = FALSE)
})

test_that("save_txt() works with a mixed list of gt_tbl and gtsummary objects", {
  skip_if_not_installed("gtsummary")
  mixed_list <- list(gt_tbl_single, gtsummary_single)
  file_path <- tempfile(fileext = ".txt")
  expect_error(
    res <- mixed_list |>
      save_txt(path = file_path),
    NA
  )
  expect_true(file.exists(file_path))

  # both packages attached because list contains a gtsummary object
  expect_match(res[9], "library(gt)", fixed = TRUE)
  expect_match(res[10], "library(gtsummary)", fixed = TRUE)
  # both chunks present
  expect_match(res, "as_kable_txt(x[[1]])", fixed = TRUE, all = FALSE)
  expect_match(res, "as_kable_txt(x[[2]])", fixed = TRUE, all = FALSE)
})

test_that("save_txt() only attaches gt when no gtsummary objects present", {
  res <- save_txt(gt_tbl_single, path = tempfile(fileext = ".txt"))

  # only gt is attached — no gtsummary library() call in the header
  expect_match(res[9], "library(gt)", fixed = TRUE)
  expect_false(any(grepl("library(gtsummary)", res, fixed = TRUE)))
})
