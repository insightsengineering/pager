test_that("create_yaml_header() builds the docx YAML header", {
  expect_snapshot(
    cat(
      create_yaml_header(
        object_path = "object.rds",
        pkg_to_attach = "gt",
        reference_docx = "reference.docx"
      ),
      sep = "\n"
    )
  )
})

test_that("create_yaml_header_html() builds the HTML YAML header", {
  expect_snapshot(
    cat(
      create_yaml_header_html(object_path = "object.rds", pkg_to_attach = "gt"),
      sep = "\n"
    )
  )
})

test_that("create_yaml_header_txt() builds the txt YAML header", {
  expect_snapshot(
    cat(
      create_yaml_header_txt(object_path = "object.rds", pkg_to_attach = "gt"),
      sep = "\n"
    )
  )
})
