#' Create Code Chunks
#'
#' Creates a series of R markdown code chunks that print the `x` object.
#'
#' @param length (`integer`)\cr
#'   integer specifying the number of code chunks to create.
#'
#' @returns Code chunk character vector
#' @export
#'
#' @examples
#' create_chunks(2L) |>
#'   cat(sep = "\n")
create_chunks <- function(length) {
  map(
    seq_len(length),
    \(i) {
      str_chunk <- c("```{r}", glue::glue("x[[{i}]]"), "```")

      # add page break between tables (but not to the last table)
      if (i < length) {
        str_chunk <- c(str_chunk, "", "\\newpage", "")
      }

      str_chunk
    }
  ) |>
    unlist()
}
