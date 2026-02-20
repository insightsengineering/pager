# save_html() fails with incorrect inputs

    Code
      save_html()
    Condition
      Error in `save_html()`:
      ! The `x` argument cannot be missing.

---

    Code
      save_html(x = tbl)
    Condition
      Error in `save_html()`:
      ! The `path` argument cannot be missing.

---

    Code
      save_html(x = tbl, path = 123)
    Condition
      Error in `save_html()`:
      ! The `path` argument must be a string, not a number.

---

    Code
      save_html(x = "not_a_table", path = tempfile(fileext = ".html"))
    Condition
      Error in `save_html()`:
      ! The `x` argument must be class <gg/ggplot/grob/gtsummary/gt_tbl/flextable/list>, not a string.

---

    Code
      save_html(list("a", "b"), "a")
    Condition
      Error in `save_html()`:
      ! When argument `x` is a list, each list element must be one of the following classes: <gg/ggplot/grob/gtsummary/gt_tbl/flextable>.

---

    Code
      save_html(x = gt_tbl, path = tempfile(fileext = ".html"), css = 123)
    Condition
      Error in `save_html()`:
      ! The `css` argument must be a string, not a number.

