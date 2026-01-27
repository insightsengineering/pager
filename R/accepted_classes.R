#' Accepted Classes
#'
#' Return the acceptable classes for different types of output.
#'
#' @returns character vector
#' @name accepted_classes
NULL

#' Accepted Table Classes
#'
#' @rdname accepted_classes
#' @export
#'
#' @examples
#' accepted_table_classes()
accepted_table_classes <- function() {
  c("gtsummary", "gt_tbl", "flextable")
}

#' Accepted Plot Classes
#'
#' @rdname accepted_classes
#' @export
#'
#' @examples
#' accepted_plot_classes()
accepted_plot_classes <- function() {
  c("gg", "ggplot", "grob")
}
