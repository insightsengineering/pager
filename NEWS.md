# pager (development version)

* `save_docx()` now embeds inline `flextable` images (such as the forest-plot column from `crane::add_forest()`) into the Word document, so the output opens correctly. Tables built with the `gt` engine error early, as their inline plots cannot be embedded in Word. (#28)
