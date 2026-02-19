# save_docx() fails with incorrect inputs

    Code
      save_docx()
    Condition
      Error in `save_docx()`:
      ! The `x` argument cannot be missing.

---

    Code
      save_docx(x = tbl)
    Condition
      Error in `save_docx()`:
      ! The `path` argument cannot be missing.

---

    Code
      save_docx(x = tbl, path = 123)
    Condition
      Error in `save_docx()`:
      ! The `path` argument must be a string, not a number.

---

    Code
      save_docx(x = "not_a_table", path = tempfile(fileext = ".docx"))
    Condition
      Error in `save_docx()`:
      ! The `x` argument must be class <gg/ggplot/grob/gtsummary/gt_tbl/flextable/list>, not a string.

---

    Code
      save_docx(list("a", "b"), "a")
    Condition
      Error in `save_docx()`:
      ! When argument `x` is a list, each list element must be one of the following classes: <gg/ggplot/grob/gtsummary/gt_tbl/flextable>.

