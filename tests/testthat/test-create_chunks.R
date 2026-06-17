test_that("create_chunks() builds the docx chunks with page breaks", {
  expect_snapshot(cat(create_chunks(1L), sep = "\n"))
  expect_snapshot(cat(create_chunks(3L), sep = "\n"))
})

test_that("create_chunks_html() builds chunks and dispatches grobs", {
  expect_snapshot(
    cat(create_chunks_html(2L, classes = list("gt_tbl", "grob")), sep = "\n")
  )
})

test_that("create_chunks_txt() builds chunks separated by rules", {
  expect_snapshot(cat(create_chunks_txt(2L), sep = "\n"))
})
