#' Accepted Object Classes
#' 
#' @returns character vector
#' @name accepted_object_classes
#' @examples
#' accepted_table_classes()
NULL

#' Accepted Table Classes
#' 
#' @rdname accepted_table_classes
#' @export
#'
#' @examples
#' accepted_table_classes()
accepted_table_classes <- function() {
  c("gtsummary", "gt_tbl", "flextable")
}

#' Accepted Plot Classes
#'
#' @rdname accepted_plot_classes
#' @export
#'
#' @examples
#' accepted_plot_classes()
accepted_plot_classes <- function() {
  c("gg", "ggplot", "grob")
}
