# create_chunks() builds the docx chunks with page breaks

    Code
      cat(create_chunks(1L), sep = "\n")
    Output
      ```{r}
      print_obj(x[[1]])
      ```

---

    Code
      cat(create_chunks(3L), sep = "\n")
    Output
      ```{r}
      print_obj(x[[1]])
      ```
      
      \newpage
      
      ```{r}
      print_obj(x[[2]])
      ```
      
      \newpage
      
      ```{r}
      print_obj(x[[3]])
      ```

# create_chunks_html() builds chunks and dispatches grobs

    Code
      cat(create_chunks_html(2L, classes = list("gt_tbl", "grob")), sep = "\n")
    Output
      ```{r}
      x[[1]]
      ```
      
      <hr/>
      
      ```{r}
      grid::grid.draw(x[[2]])
      ```

# create_chunks_txt() builds chunks separated by rules

    Code
      cat(create_chunks_txt(2L), sep = "\n")
    Output
      ```{r}
      as_kable_txt(x[[1]])
      ```
      
      ---
      
      ```{r}
      as_kable_txt(x[[2]])
      ```

