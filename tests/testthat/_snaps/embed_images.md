# save_docx() warns when a gt table carries inline images

    Code
      embed_ondisk_images(path)
    Condition
      Warning in `embed_ondisk_images()`:
      The table contains inline images (for example a `crane::add_forest()` plot built with `table_engine = "gt"`) that cannot be embedded in a Word document.
      i The images will not display correctly.
      i Use `save_html()` instead, or build the table with the default `table_engine = "flextable"`.

