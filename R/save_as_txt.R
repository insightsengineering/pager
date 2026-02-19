#' Create YAML Header for txt/markdown output (internal)
#'
#' @inheritParams create_yaml_header
#' @keywords internal
create_yaml_header_txt <- function(object_path, pkg_to_attach) {
  c(
    "---",
    "output:",
    "  md_document:",
    "    variant: markdown",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE)",
    paste0("library(", pkg_to_attach, ")"),
    paste("x <-", rlang::call2("readRDS", file = as.character(object_path)) |> rlang::expr_deparse(width = Inf)),
    "if (!inherits(x, 'list') || inherits(x, 'gt_tbl')) x <- list(x)",
    "as_kable_txt <- function(obj) {",
    "  if (inherits(obj, 'gtsummary')) {",
    "    gtsummary::as_kable(obj)",
    "  } else if (inherits(obj, 'gt_tbl')) {",
    "    knitr::kable(gt:::dt_data_get(obj))",
    "  } else if (inherits(obj, 'flextable')) {",
    "    knitr::kable(obj$body$dataset)",
    "  } else {",
    "    print(obj)",
    "  }",
    "}",
    "```",
    ""
  )
}

#' Create Code Chunks for txt/markdown output (internal)
#'
#' Creates R markdown chunks that render each object as plain text using
#' `knitr::kable()` for tables (via `as_kable_txt()` defined in the setup chunk).
#'
#' @inheritParams create_chunks
#' @keywords internal
create_chunks_txt <- function(length) {
  map(
    seq_len(length),
    \(i) {
      str_chunk <- c(
        "```{r}",
        glue::glue("as_kable_txt(x[[{i}]])"),
        "```"
      )

      # add separator between items (but not after the last one)
      if (i < length) {
        str_chunk <- c(str_chunk, "", "---", "")
      }

      str_chunk
    }
  ) |>
    unlist()
}

#' Save as txt (plain text / Markdown) via R Markdown (internal)
#'
#' Internal function that saves table input as a plain text (Markdown) file
#' via R markdown. Use [save_txt()] instead.
#'
#' @inheritParams save_txt
#' @keywords internal
save_txt_with_rmarkdown <- function(x,
                                    path,
                                    encoding = "UTF-8") {
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(path)
  check_string(path)

  # convert path to absolute path to ensure output is created in the correct location
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  # txt output only supports table classes (plots cannot be rendered as text)
  accepted_obj <- accepted_table_classes()

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
    dplyr::case_match(
      pkg_to_attach,
      "gtsummary" ~ "gtsummary",
      "gt_tbl" ~ "gt",
      .default = pkg_to_attach
    ) |>
    unique()

  # string of the yaml header and chunks
  chr_rmarkdown_yaml <- create_yaml_header_txt(temp_file_x, pkg_to_attach)
  chr_rmarkdown_chunk <- create_chunks_txt(ifelse(is_simple_list(x), length(x), 1L))

  chr_rmarkdown <- c(chr_rmarkdown_yaml, "", chr_rmarkdown_chunk)

  # write rmd file and render --------------------------------------------------
  writeLines(chr_rmarkdown, con = temp_file_rmd)

  # rmarkdown renders md_document to a .md file; we then copy it to `path`
  temp_file_md <- paste0(tools::file_path_sans_ext(temp_file_rmd), ".md")

  cli::cli_alert_success("Writing {.path {path}}")
  tryCatch(
    {
      rmarkdown::render(input = temp_file_rmd, output_file = temp_file_md, quiet = TRUE)
      file.copy(from = temp_file_md, to = path, overwrite = TRUE)
    },
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

#' Save as txt (plain text / Markdown)
#'
#' Save table input as a plain text (Markdown-formatted) file via R markdown.
#' Plots are not supported for plain text output.
#'
#' @param x (`gtsummary`/`gt_tbl`/`flextable`/`list`)\cr
#'   object of class `'gtsummary'`, `'gt_tbl'` (gt table), `'flextable'`,
#'   or a list of these objects.
#' @param path (`path`)\cr
#'   path to save file to, e.g. `"rendered_table.txt"` or `"rendered_table.md"`.
#' @param encoding (`string`)\cr
#'   character encoding to use when writing the output file. Default is `"UTF-8"`.
#'
#' @returns (invisibly) a `string` corresponding to the content of the intermediate
#'   `.rmd` file that is rendered.
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
#' # save as txt with gtsummary
#' tbl |>
#'   save_txt(path = tempfile(fileext = ".txt"))
#'
#' # save as txt with flextable
#' gtsummary::as_flex_table(tbl) |>
#'   save_txt(path = tempfile(fileext = ".txt"))
#'
#' # save as txt with gt
#' tbl |>
#'   gtsummary::as_gt() |>
#'   save_txt(path = tempfile(fileext = ".txt"))
#'
#' # save a paginated table as txt
#' gtsummary::tbl_split_by_rows(tbl, row_numbers = seq(20, nrow(tbl), by = 20)) |>
#'   save_txt(path = tempfile(fileext = ".txt"))
#'
#' @export
save_txt <- function(x, path, encoding = "UTF-8") {
  set_cli_abort_call()
  save_txt_with_rmarkdown(x = x, path = path, encoding = encoding)
}
