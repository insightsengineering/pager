#' Save as docx
#'
#' Save Input via R markdown.
#'
#' @param x (`gtsummary`/`gt_tbl`/`flextable`/`list`)\cr
#'   table object of class `'gtsummary'`, `'gt_tbl'` (gt table) or `'flextable'`, or a list
#'   of table objects.
#' @param path (`path`)\cr
#'   path to save file to, e.g. "rendered_table.docx"
#' @param reference_docx (`path`)\cr
#'   path to reference document that when not `NULL` is passed to the
#'   `reference_docx:` R markdown field.
#' @param temp_file_rmd (`path`)\cr
#'   path to temporary R markdown file used in rendering. Principally used for testing.
#'
#' @returns (invisibly) a `string` corresponding to the content of the intermediate `.rmd` file that is rendered as
#'   `.docx` or a message explaining the error encountered during rendering.
#'
#' @examples
#' # create table
#' tbl <-
#'   cards::ADAE[1:150, ] |>
#'   gtsummary::tbl_hierarchical(
#'     variables = c(AESOC, AETERM),
#'     by = TRTA,
#'     denominator = cards::ADSL,
#'     id = USUBJID,
#'   )
#'
#' # save as docx with flextable
#' gtsummary::as_flex_table(tbl) |>
#'   flextable::set_table_properties(layout = "autofit") |> # otherwise is going too wide
#'   save_with_rmarkdown(path = tempfile(fileext = ".docx"))
#'
#' # save as docx with gt
#' tbl |>
#'   save_with_rmarkdown(path = tempfile(fileext = ".docx"))
#'
#' # split the table and save paginated table
#' gtsummary::tbl_split_by_rows(tbl, row_numbers = seq(20, nrow(tbl), by = 20)) |>
#'   save_with_rmarkdown(path = tempfile(fileext = ".docx"))
#'
#' @export
save_with_rmarkdown <- function(x,
                                path,
                                reference_docx = get_reference_docx("portrait"),
                                temp_file_rmd = tempfile(fileext = ".rmd")) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(path)
  check_string(path)
  check_string(reference_docx, allow_empty = TRUE)

  # convert path to absolute path to ensure output is created in the correct location
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  check_class(x, cls = c(accepted_table_classes(), "list"))
  # check each object in the list is a table
  if (inherits(x, "list") && some(x, ~ !inherits(.x, accepted_table_classes()))) {
    cli::cli_abort(
      c(
        "When argument {.arg x} is a list, each list element must be one of the ",
        "following classes: {.cls {accepted_table_classes()}}."
      ),
      call = get_cli_abort_call()
    )
  }

  # set temp files -------------------------------------------------------------
  temp_file_x <- tempfile(fileext = ".rds")
  # save the input object to a tempfile (which will be loaded in the rmd file) -
  saveRDS(x, file = temp_file_x)

  # preparing for r markdown code vector ---------------------------------------
  pkg_to_attach <-
    ifelse(inherits(x, "list"), x, list(x)) |>
    map(\(xi) intersect(class(xi), accepted_table_classes())) |>
    unlist()
  pkg_to_attach <- ifelse(pkg_to_attach == "gt_tbl", "gt", pkg_to_attach)

  # string of the yaml header
  chr_rmarkdown_yaml <- create_yaml_header(temp_file_x, pkg_to_attach, reference_docx)
  chr_rmarkdown_chunk <- create_chunks(ifelse(inherits(x, "list"), length(x), 1L))

  chr_rmarkdown <- c(chr_rmarkdown_yaml, "", chr_rmarkdown_chunk)
  msg <- NULL

  # write file via R markdown --------------------------------------------------
  writeLines(chr_rmarkdown, con = temp_file_rmd)
  # render R markdown document and save result
  cli::cli_alert_success("Writing {.path {path}}")
  tryCatch(
    rmarkdown::render(input = temp_file_rmd, output_file = path, quiet = TRUE),
    error = \(e) {
      cli::cli_abort(
        c("There was an error rendering the document.",
          msg = conditionMessage(e)
        ),
        call = get_cli_abort_call()
      )
    }
  )

  invisible(msg %||% chr_rmarkdown)
}
