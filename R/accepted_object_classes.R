#' Accepted Object S3 Classes
#'
#' @returns character vector
#' @export
#'
#' @examples
#' accepted_object_classes()
accepted_object_classes <- function() {
  c("gtsummary", "gt_tbl", "flextable", "gg", "ggplot", "grob")
}
