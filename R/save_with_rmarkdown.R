#' Save as docx
#'
#' Save Input via R markdown.
#'
#' @param x (`gtsummary`/`gt_tbl`/`flextable`/`list`)\cr
#'   table object of class `'gtsummary'` or `'flextable'`, or a list
#'   of table objects.
#' @param path (`path`)\cr
#'   path to save file to, e.g. "rendered_table.docx"
#' @param output (`string`)
#'   Output format. For now, only `docx` output type is supported.
#' @param reference_docx (`path`)\cr
#'   Path to reference document that when not `NULL` is passed to the
#'   `reference_docx:` R markdown field.
#'
#' @returns x (invisibly)
#' @export
#'
#' @examples
#' # create table
#' library(gtsummary)
#'
#' tbl <-
#'   cards::ADAE[1:150,] |>
#'   tbl_hierarchical(
#'     variables = c(AESOC, AETERM),
#'     by = TRTA,
#'     denominator = cards::ADSL,
#'     id = USUBJID,
#'   ) |>
#'   as_flex_table()
#'
#' # save as docx
#' save_with_rmarkdown(x = tbl, path = tempfile(fileext = "docx"))
save_with_rmarkdown <- function(x, path, reference_docx = NULL, output = "docx") {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(path)
  check_string(path)
  check_string(reference_docx, allow_empty = TRUE)
  check_class(x, cls = c(accepted_table_classes(), "list"))
  # check each object in the list is a table
  if (inherits(x, "list") && some(x, ~!inherits(.x, accepted_table_classes()))) {
    cli::cli_abort(
      "When argument {.arg x} is a list, each list element must be one of the following classes: {.cls {accepted_table_classes()}}.",
      call = get_cli_abort_call()
    )
  }
  output <- arg_match(output)

  # set temp files -------------------------------------------------------------
  temp_file_x <- tempfile(fileext = "rds")
  temp_file_rmd <- tempfile(fileext = "rmd")
  # save the input object to a tempfile (which will be loaded in the rmd file) -
  saveRDS(x, file = temp_file_x)

  # preparing for r markdown code vector ---------------------------------------
  pkg_to_attach <-
    ifelse(inherits(x, "list"), map(x, class), list(x)) |>
    map_chr(class) |>
    unlist() |>
    intersect(accepted_table_classes())
  pkg_to_attach <- ifelse(pkg_to_attach == "gt_tbl", "gt", pkg_to_attach)

  # string of the yaml header
  chr_rmarkdown_yaml <- .create_yaml_header(temp_file_x, pkg_to_attach, reference_docx)
  chr_rmarkdown_chunk <- .create_chunks(ifelse(inherits(x, "list"), length(x), 1L))

  chr_rmarkdown <- c(chr_rmarkdown_yaml, "", chr_rmarkdown_chunk)

  # write file via R markdown --------------------------------------------------
  writeLines(chr_rmarkdown, con = temp_file_rmd)
  # render R markdown document and save result
  cli::cli_alert_success("Writing {.path {path}}")
  tryCatch(
    rmarkdown::render(input = temp_file_rmd, output_file = path, quiet = TRUE),
    error = \(e) {
      cli::cli_abort(
        c("There was an error rendering the document.",
          x = conditionMessage(e)),
        call = get_cli_abort_call()
      )
    }
  )


  invisible(x)
}

.create_yaml_header <- function(object_path, pkg_to_attach, reference_docx) {
  c("---",
    "output:",
    "  word_document:",
    if (!is_empty(reference_docx)) glue::glue("    reference_docx: !expr {reference_docx}"),
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

.create_chunks <- function(length) {
  map(
    seq_len(length),
    \(i) {
      str_chunk <- c("```{r}", glue::glue("x[[{i}]]"), "```")

      # add page break between tables (but not to the last table)
      if (i < length) {
        str_chunk <- c(str_chunk, "", "\newpage", "")
      }

      str_chunk
    }
  ) |>
    unlist()
}
