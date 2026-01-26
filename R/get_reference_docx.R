#' Get Reference Document Path
#'
#' @param orientation (`string`)\cr
#'   Must be one of `r cli::cli_format(unique(get_all_reference_docx()$orientation))`
#' @param font (`string`)\cr
#'   Font of the page numbers.
#'   Must be one of `r cli::cli_format(unique(get_all_reference_docx()$font))`
#' @param font_size (`integer`)\cr
#'   Font size of the page numbers.
#'   Must be one of `r cli::cli_format(unique(get_all_reference_docx()$font_size))`
#'
#' @returns path
#' @name get_reference_docx
#'
#' @examples
#' get_reference_docx()
NULL

#' @name get_reference_docx
#' @export
get_reference_docx <- function(orientation = "portrait",
                               font = "Arial",
                               font_size = 7) {
  # process inputs -------------------------------------------------------------
  orientation <- arg_match(orientation, values = unique(get_all_reference_docx()$orientation))
  font <- arg_match(font, values = unique(get_all_reference_docx()$font))
  check_integerish(font_size)


  df_ref_docx <-
    get_all_reference_docx() |>
    dplyr::filter(
      .data$orientation %in% .env$orientation,
      .data$font %in% .env$font,
      .data$font_size %in% .env$font_size
    )
  if (nrow(df_ref_docx) == 0L) {
    cli::cli_abort(c("No reference Word document selected.",
      i = "Run {.fun get_all_reference_docx} to print possible selections."
    ))
  }

  df_ref_docx$path[1]
}

#' @name get_reference_docx
#' @export
get_all_reference_docx <- function() {
  dplyr::tribble(
    ~orientation, ~font, ~font_size, ~path,
    "portrait", "Arial", 7, fs::path_package("pager", "ref_docs", "arial7_page-num-footer_portrait.docx"),
    "landscape", "Arial", 7, fs::path_package("pager", "ref_docs", "arial7_page-num-footer_landscape.docx")
  )
}
