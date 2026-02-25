# save_txt() fails with incorrect inputs

    Code
      save_txt()
    Condition
      Error in `save_txt()`:
      ! The `x` argument cannot be missing.

---

    Code
      save_txt(x = gt_tbl_single)
    Condition
      Error in `save_txt()`:
      ! The `path` argument cannot be missing.

---

    Code
      save_txt(x = gt_tbl_single, path = 123)
    Condition
      Error in `save_txt()`:
      ! The `path` argument must be a string, not a number.

---

    Code
      save_txt(x = "not_a_table", path = tempfile(fileext = ".txt"))
    Condition
      Error in `save_txt()`:
      ! The `x` argument must be class <gt_tbl/list>, not a string.

---

    Code
      save_txt(list("a", "b"), path = tempfile(fileext = ".txt"))
    Condition
      Error in `save_txt()`:
      ! When argument `x` is a list, each list element must be of class <gt_tbl>.

# save_txt() fails with non-gt_tbl objects

    Code
      save_txt(ft, path = tempfile(fileext = ".txt"))
    Condition
      Error in `save_txt()`:
      ! The `x` argument must be class <gt_tbl/list>, not a <flextable> object.

