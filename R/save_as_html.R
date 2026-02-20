#' Create YAML Header for HTML output (internal)
#'
#' @inheritParams create_yaml_header
#' @param css (`string`)\cr
#'   optional CSS style block to embed in the HTML output. When provided, the
#'   string is written to a temporary `.css` file and referenced via the
#'   `css:` R markdown field. Default is `NULL` (no custom CSS).
#' @keywords internal
create_yaml_header_html <- function(object_path, pkg_to_attach, css = NULL) {
  # write css to a temp file if provided
  if (!is.null(css)) {
    temp_file_css <- tempfile(fileext = ".css")
    writeLines(css, con = temp_file_css)
    css_line <- glue::glue("    css: {temp_file_css}")
  } else {
    css_line <- NULL
  }

  c(
    "---",
    "output:",
    "  html_document:",
    "    self_contained: true",
    css_line,
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE)",
    paste0("library(", pkg_to_attach, ")"),
    paste("x <-", rlang::call2("readRDS", file = as.character(object_path)) |> rlang::expr_deparse(width = Inf)),
    "if (!inherits(x, 'list') || inherits(x, 'gt_tbl')) x <- list(x)",
    "```",
    ""
  )
}

#' Create Code Chunks for HTML output (internal)
#'
#' Creates R markdown chunks that render each `gt_tbl` object as HTML using
#' `gt::as_raw_html()`.
#'
#' @inheritParams create_chunks
#' @keywords internal
create_chunks_html <- function(length) {
  map(
    seq_len(length),
    \(i) {
      str_chunk <- c(
        "```{r}",
        glue::glue("gt::as_raw_html(x[[{i}]]) |> cat()"),
        "```"
      )

      # add a horizontal rule between tables (but not after the last one)
      if (i < length) {
        str_chunk <- c(str_chunk, "", "<hr/>", "")
      }

      str_chunk
    }
  ) |>
    unlist()
}

#' Save as HTML via R Markdown (internal)
#'
#' Internal function that saves a `gt_tbl` object as an `.html` file via
#' R markdown. Use [save_html()] instead.
#'
#' @inheritParams save_html
#' @keywords internal
save_html_with_rmarkdown <- function(x,
                                     path,
                                     css = NULL) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(path)
  check_string(path)
  if (!is.null(css)) check_string(css)

  # convert path to absolute path to ensure output is created in the correct location
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  accepted_obj <- c(accepted_plot_classes(), accepted_table_classes())

  check_class(x, cls = c(accepted_obj, "list"))
  if (is_simple_list(x) && some(x, ~ !inherits(.x, accepted_obj))) {
    cli::cli_abort(
      "When argument {.arg x} is a list, each list element must be one of the following classes: {.cls {accepted_obj}}.",
      call = get_cli_abort_call()
    )
  }

  # set temp files -------------------------------------------------------------
  temp_file_x <- tempfile(fileext = ".rds")
  temp_file_rmd <- tempfile(fileext = ".rmd")
  # save the input object to a tempfile (which will be loaded in the rmd file) -
  saveRDS(x, file = temp_file_x)

  # preparing for r markdown code vector ---------------------------------------
  pkg_to_attach <-
    ifelse(is_simple_list(x), map(x, class), list(class(x))) |>
    unlist() |>
    intersect(x = _, accepted_obj)

  pkg_to_attach <-
    dplyr::recode_values(
      pkg_to_attach,
      "gtsummary" ~ "gtsummary",
      "gt_tbl"    ~ "gt",
      "gg"        ~ "ggplot2",
      "ggplot"    ~ "ggplot2",
      "grob"      ~ "grid"
    ) |>
    unique()

  # string of the yaml header and chunks
  chr_rmarkdown_yaml <- create_yaml_header_html(temp_file_x, pkg_to_attach, css)
  chr_rmarkdown_chunk <- create_chunks_html(ifelse(is_simple_list(x), length(x), 1L))

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
          x = conditionMessage(e)
        ),
        call = get_cli_abort_call()
      )
    }
  )

  invisible(chr_rmarkdown)
}

#' Save as HTML
#'
#' Save input in a self-contained `.html` file via R markdown.
#'
#' @param x (`gtsummary`/`gt_tbl`/`flextable`/`ggplot`/`grob`/`list`)\cr
#'   object of class `'gtsummary'`, `'gt_tbl'` (gt table), `'flextable'`, `'ggplot'`,
#'   `'grob'`, or a list of these objects.
#' @param path (`path`)\cr
#'   path to save file to, e.g. `"rendered_table.html"`.
#' @param css (`string`)\cr
#'   optional CSS style block to embed in the HTML output. Default is `NULL`
#'   (no custom CSS). Example: `"body { font-family: Arial; font-size: 12px; }"`.
#'
#' @returns (invisibly) a `string` corresponding to the content of the intermediate
#'   `.rmd` file that is rendered as `.html`.
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
#' # save as html with gt
#' tbl |>
#'   gtsummary::as_gt() |>
#'   save_html(path = tempfile(fileext = ".html"))
#'
#' # save as html with custom CSS
#' tbl |>
#'   gtsummary::as_gt() |>
#'   save_html(
#'     path = tempfile(fileext = ".html"),
#'     css = "body { font-family: Arial; font-size: 12px; }"
#'   )
#'
#' # save a paginated table as html
#' gtsummary::tbl_split_by_rows(tbl, row_numbers = seq(20, nrow(tbl), by = 20)) |>
#'   save_html(path = tempfile(fileext = ".html"))
#'
#' @export
save_html <- function(x, path, css = NULL) {
  set_cli_abort_call()
  save_html_with_rmarkdown(x = x, path = path, css = css)
}
