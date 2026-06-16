test_that("get_reference_docx() returns a path for valid selections", {
  expect_true(fs::file_exists(get_reference_docx()))
  expect_true(fs::file_exists(get_reference_docx(orientation = "landscape")))
})

test_that("get_all_reference_docx() lists the bundled reference documents", {
  refs <- get_all_reference_docx()
  expect_setequal(refs$orientation, c("portrait", "landscape"))
  expect_true(all(fs::file_exists(refs$path)))
})

test_that("get_reference_docx() aborts when no reference document matches", {
  # font_size is integerish (passes the input check) but has no matching row
  expect_snapshot(
    get_reference_docx(font_size = 99L),
    error = TRUE
  )
})
