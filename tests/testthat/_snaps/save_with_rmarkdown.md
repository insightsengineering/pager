# save_with_rmarkdown() works with gtsummary table

    Code
      temp_file_rmd_content
    Output
       [1] "---"                                                                                              
       [2] "output:"                                                                                          
       [3] "  word_document:"                                                                                 
       [4] "    reference_docx: /home/melkiades/NEST/pager/inst/ref_docs/arial7_page-num-footer_portrait.docx"
       [5] "---"                                                                                              
       [6] ""                                                                                                 
       [7] "```{r setup, include=FALSE}"                                                                      
       [8] "knitr::opts_chunk$set(echo = FALSE, message = FALSE)"                                             
       [9] "library(gtsummary)"                                                                               
      [10] "x <- readRDS(file = \"path/to/data.rds\")"                                                        
      [11] "if (!inherits(x, 'list')) x <- list(x)"                                                           
      [12] "```"                                                                                              
      [13] ""                                                                                                 
      [14] ""                                                                                                 
      [15] "```{r}"                                                                                           
      [16] "x[[1]]"                                                                                           
      [17] "```"                                                                                              
      [18] ""                                                                                                 
      [19] "\\newpage"                                                                                        
      [20] ""                                                                                                 
      [21] "```{r}"                                                                                           
      [22] "x[[2]]"                                                                                           
      [23] "```"                                                                                              

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
      ! The `x` argument must be class <gtsummary/gt_tbl/flextable/list>, not a string.

---

    Code
      save_with_rmarkdown(list("a", "b"), "a")
    Condition
      Error in `save_with_rmarkdown()`:
      ! When argument `x` is a list, each list element must be one of the
      following classes: <gtsummary/gt_tbl/flextable>.

