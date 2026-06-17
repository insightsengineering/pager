# pager (development version)

* Set a real package `Title` and `Description`, and added `URL` and `BugReports` fields to `DESCRIPTION`. (#13)

* Added a pull request template. (#16)

* Added tests for previously uncovered error paths, raising package coverage to 97.6%. (#26)

* Added snapshot tests covering the R Markdown chunk and YAML header builders for the Word, HTML, and text outputs. (#21)

* `save_docx()` now embeds inline `flextable` images (such as the forest-plot column from `crane::add_forest()`) into the Word document, so the output opens correctly. Tables built with the `gt` engine emit a warning, as their inline plots cannot be embedded in Word. (#28)

