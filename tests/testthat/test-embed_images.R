test_that("save_docx() embeds flextable images so the document is self-contained", {
  skip_on_cran()
  skip_if_not_installed("flextable")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("zip")

  # a flextable with an inline ggplot image reproduces the scenario where pandoc
  # references the image by its temporary on-disk path (e.g. crane::add_forest())
  df <- data.frame(group = c("A", "B"), plot = c("", ""))
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  ft <- flextable::flextable(df) |>
    flextable::mk_par(
      j = "plot",
      value = flextable::as_paragraph(
        flextable::gg_chunk(value = list(p, p), height = 0.4, width = 1.5)
      )
    )

  path <- withr::local_tempfile(fileext = ".docx")
  save_docx(ft, path)

  extract_dir <- withr::local_tempdir()
  zip::unzip(path, exdir = extract_dir)
  doc <- paste(
    readLines(fs::path(extract_dir, "word", "document.xml"), warn = FALSE),
    collapse = "\n"
  )

  # images are packaged into the archive
  expect_length(fs::dir_ls(fs::path(extract_dir, "word", "media")), 2L)
  # the drawings reference relationship ids, not on-disk file paths
  expect_match(doc, 'r:embed="rId[0-9]+"')
  expect_no_match(doc, 'r:embed="/')
})

test_that("image_content_type() maps extensions to OOXML content types", {
  expect_identical(image_content_type("png"), "image/png")
  expect_identical(image_content_type("PNG"), "image/png")
  expect_identical(image_content_type("jpg"), "image/jpeg")
  expect_identical(image_content_type("jpeg"), "image/jpeg")
  expect_identical(image_content_type("gif"), "image/gif")
  expect_identical(image_content_type("svg"), "image/svg+xml")
  expect_identical(image_content_type("bmp"), "application/octet-stream")
})

test_that("embed_ondisk_images() leaves an image-free document unchanged", {
  skip_on_cran()
  skip_if_not_installed("zip")

  path <- withr::local_tempfile(fileext = ".docx")
  save_docx(gt::gt(head(mtcars)), path)
  before <- readBin(path, "raw", file.info(path)$size)

  embed_ondisk_images(path)
  after <- readBin(path, "raw", file.info(path)$size)

  expect_identical(before, after)
})

test_that("save_docx() warns when a gt table carries inline images", {
  skip_on_cran()
  skip_if_not_installed("zip")

  # build a docx whose document.xml contains the escaped data-URI signature that
  # gt produces for inline plots, then confirm the post-processor warns
  path <- withr::local_tempfile(fileext = ".docx")
  save_docx(gt::gt(head(mtcars)), path)

  extract_dir <- withr::local_tempdir()
  zip::unzip(path, exdir = extract_dir)
  doc_path <- fs::path(extract_dir, "word", "document.xml")
  doc <- readLines(doc_path, warn = FALSE)
  doc[1] <- paste0(doc[1], '<w:t>&lt;img src="data:image/png;base64,AAAA"/&gt;</w:t>')
  writeLines(doc, doc_path)
  files <- list.files(extract_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  withr::with_dir(extract_dir, zip::zip(path, files = files))

  expect_snapshot(embed_ondisk_images(path))
  # the file is left in place so the user can still inspect it
  expect_true(fs::file_exists(path))
})
