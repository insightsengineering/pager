# save_docx() errors when a gt table carries inline images

    Code
      embed_ondisk_images(path)
    Condition
      Error in `embed_ondisk_images()`:
      ! Tables with inline images built with `table_engine = "gt"` cannot be saved to Word.
      i Build the table with `table_engine = "flextable"` in `crane::add_forest()`, or use gt's native Word export (see `gt::gtsave()`).

