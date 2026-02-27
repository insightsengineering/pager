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
    "if (!inherits(x, 'list') || inherits(x, 'gt_tbl') || inherits(x, 'gtsummary')) x <- list(x)",
    "as_kable_txt <- function(obj) {",
    "  if (inherits(obj, 'gtsummary')) {",
    "    gtsummary::as_kable(obj)",
    "  } else {",
    "    knitr::kable(gt:::dt_data_get(obj))",
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

  # txt output supports gt_tbl and gtsummary objects
  check_class(x, cls = c("gt_tbl", "gtsummary", "list"))
  if (is_simple_list(x) && some(x, ~ !inherits(.x, c("gt_tbl", "gtsummary")))) {
    cli::cli_abort(
      "When argument {.arg x} is a list, each list element must be of class {.cls gt_tbl} or {.cls gtsummary}.",
      call = get_cli_abort_call()
    )
  }

  # set temp files -------------------------------------------------------------
  temp_file_x <- tempfile(fileext = ".rds")
  temp_file_rmd <- tempfile(fileext = ".rmd")
  # save the input object to a tempfile (which will be loaded in the rmd file) -
  saveRDS(x, file = temp_file_x)

  # determine which packages need to be attached based on object class(es)
  x_list <- if (is_simple_list(x)) x else list(x)
  has_gtsummary <- some(x_list, ~ inherits(.x, "gtsummary"))
  pkg_to_attach <- if (has_gtsummary) c("gt", "gtsummary") else "gt"

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
#' Save a `gt_tbl` or `gtsummary` object as a plain text (Markdown-formatted)
#' file via R markdown. Both `gt_tbl` objects (from the gt package) and
#' `gtsummary` objects are supported for plain text output.
#'
#' @param x (`gt_tbl`/`gtsummary`/`list`)\cr
#'   object of class `'gt_tbl'` (gt table) or `'gtsummary'`, or a list of
#'   such objects. Lists may contain a mix of `'gt_tbl'` and `'gtsummary'`
#'   objects.
#' @param path (`path`)\cr
#'   path to save file to, e.g. `"rendered_table.txt"` or `"rendered_table.md"`.
#' @param encoding (`string`)\cr
#'   character encoding to use when writing the output file. Default is `"UTF-8"`.
#'
#' @returns (invisibly) a `string` corresponding to the content of the intermediate
#'   `.rmd` file that is rendered.
#'
#' @examples
#' # save a gt table as txt
#' gt::gt(head(mtcars)) |>
#'   save_txt(path = tempfile(fileext = ".txt"))
#'
#' # save a gtsummary table as txt
#' gtsummary::tbl_summary(gtsummary::trial[, c("age", "trt")]) |>
#'   save_txt(path = tempfile(fileext = ".txt"))
#'
#' # save a list of gt tables as txt — tables are separated by a horizontal rule
#' list(gt::gt(head(mtcars)), gt::gt(tail(mtcars))) |>
#'   save_txt(path = tempfile(fileext = ".txt"))
#'
#' @export
save_txt <- function(x, path, encoding = "UTF-8") {
  set_cli_abort_call()
  save_txt_with_rmarkdown(x = x, path = path, encoding = encoding)
}
