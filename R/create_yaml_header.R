#' Create YAML Header
#'
#' Creates the R markdown header, and first setup chunk.
#' The setup chunk quiets messages, attaches any needed packages,
#' and loads the object to be printed.
#'
#' @param object_path (`path`)\cr
#'   path to saved RDS object file.
#' @param pkg_to_attach (`character`)\cr
#'   character vector of packages that are attached at the top of the R markdown
#'   document. These are needed to access the appropraite print methods
#'   for the loaded table object.
#' @param reference_docx (`path`)\cr
#'   path the reference Word document.
#'
#' @returns YAML character vector
#' @export
#'
#' @examples
#' create_yaml_header(
#'   object_path = tempfile(fileext = ".rds"),
#'   pkg_to_attach = "flextable",
#'   reference_docx = tempfile(fileext = ".docx")
#' ) |>
#'   cat(sep = "\n")
create_yaml_header <- function(object_path, pkg_to_attach, reference_docx) {
  c("---",
    "output:",
    "  word_document:",
    if (!is_empty(reference_docx)) glue::glue("    reference_docx: {reference_docx}"),
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE)",
    paste0("library(", pkg_to_attach, ")"),
    paste("x <-", rlang::call2("readRDS", file = as.character(object_path)) |> rlang::expr_deparse(width = Inf)),
    "if (!inherits(x, 'list')) x <- list(x)",
    "```",
    "")
}
