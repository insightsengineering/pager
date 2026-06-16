# create_yaml_header() builds the docx YAML header

    Code
      cat(create_yaml_header(object_path = "object.rds", pkg_to_attach = "gt",
        reference_docx = "reference.docx"), sep = "\n")
    Output
      ---
      output:
        word_document:
          reference_docx: reference.docx
      ---
      
      ```{r setup, include=FALSE}
      knitr::opts_chunk$set(echo = FALSE, message = FALSE)
      library(gt)
      x <- readRDS(file = "object.rds")
      if (!is_simple_list(x)) x <- list(x)
      print_obj <- function(obj) {
        if (inherits(obj, 'grob')) grid::grid.draw(obj) else obj
      }
      ```
      

# create_yaml_header_html() builds the HTML YAML header

    Code
      cat(create_yaml_header_html(object_path = "object.rds", pkg_to_attach = "gt"),
      sep = "\n")
    Output
      ---
      title: " "
      output:
        html_document:
          self_contained: true
      ---
      
      ```{r setup, include=FALSE}
      knitr::opts_chunk$set(echo = FALSE, message = FALSE)
      library(gt)
      x <- readRDS(file = "object.rds")
      if (!inherits(x, 'list') || inherits(x, 'gt_tbl')) x <- list(x)
      ```
      

# create_yaml_header_txt() builds the txt YAML header

    Code
      cat(create_yaml_header_txt(object_path = "object.rds", pkg_to_attach = "gt"),
      sep = "\n")
    Output
      ---
      output:
        md_document:
          variant: markdown
      ---
      
      ```{r setup, include=FALSE}
      knitr::opts_chunk$set(echo = FALSE, message = FALSE)
      library(gt)
      x <- readRDS(file = "object.rds")
      if (!inherits(x, 'list') || inherits(x, 'gt_tbl') || inherits(x, 'gtsummary')) x <- list(x)
      as_kable_txt <- function(obj) {
        if (inherits(obj, 'gtsummary')) {
          gtsummary::as_kable(obj)
        } else {
          knitr::kable(gt:::dt_data_get(obj))
        }
      }
      ```
      

