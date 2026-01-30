# save_with_rmarkdown() fails with incorrect inputs

    Code
      save_with_rmarkdown()
    Condition
      Error in `save_with_rmarkdown()`:
      ! The `x` argument cannot be missing.

---

    Code
      save_with_rmarkdown(x = tbl)
    Condition
      Error in `save_with_rmarkdown()`:
      ! The `path` argument cannot be missing.

---

    Code
      save_with_rmarkdown(x = tbl, path = 123)
    Condition
      Error in `save_with_rmarkdown()`:
      ! The `path` argument must be a string, not a number.

---

    Code
      save_with_rmarkdown(x = "not_a_table", path = tempfile(fileext = ".docx"))
    Condition
      Error in `save_with_rmarkdown()`:
      ! The `x` argument must be class <gg/ggplot/grob/gtsummary/gt_tbl/flextable/list>, not a string.

---

    Code
      save_with_rmarkdown(list("a", "b"), "a")
    Condition
      Error in `save_with_rmarkdown()`:
      ! When argument `x` is a list, each list element must be one of the following classes: <gg/ggplot/grob/gtsummary/gt_tbl/flextable>.

