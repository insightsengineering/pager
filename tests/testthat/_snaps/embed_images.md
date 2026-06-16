# save_docx() warns when a gt table carries inline images

    Code
      embed_ondisk_images(path)
    Condition
      Warning:
      Inline images built with `table_engine = "gt"` cannot be embedded in Word and will not display correctly.
      i Build the table with `table_engine = "flextable"` in `crane::add_forest()`, or use gt's native Word export (see `gt::gtsave()`).

