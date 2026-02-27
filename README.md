---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# pager

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/pager)](https://CRAN.R-project.org/package=pager)
[![R-CMD-check](https://github.com/insightsengineering/pager/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/pager/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/insightsengineering/pager/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/pager)
<!-- badges: end -->

The pager package makes it simple to save tables and plots as Word, HTML, or plain text documents.
Tables of class `gtsummary`, `gt`, and `flextable`, as well as `ggplot` and `grob` plots, are supported.
This is accomplished by rendering the objects via R Markdown.

The package also supports lists of objects.
When a list is passed, each element is placed on a separate page (Word), separated by a horizontal rule (HTML), or separated by a horizontal rule (plain text).

## Installation

You can install the development version of pager from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("insightsengineering/pager")
```

## Example

To begin, let's create a summary table.

``` r
library(pager)

# create table
tbl <-
  cards::ADAE[1:150, ] |>
  gtsummary::tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
  )
```

### Word (.docx)

The code below will save the table as a Word document using the default portrait orientation reference document.

``` r
gtsummary::as_flex_table(tbl) |>
  save_docx(path = tempfile(fileext = ".docx"))
```

The example below first splits the summary table into a list of tables.
Each table is saved to a separate page in the resulting Word document.

``` r
gtsummary::tbl_split_by_rows(tbl, row_numbers = seq(20, nrow(tbl), by = 20)) |>
  save_docx(path = tempfile(fileext = ".docx"))
```

### HTML (.html)

The code below will save the table as a self-contained HTML file.

``` r
tbl |>
  gtsummary::as_gt() |>
  save_html(path = tempfile(fileext = ".html"))
```

A paginated table can also be saved as HTML — each page is separated by a horizontal rule.

``` r
gtsummary::tbl_split_by_rows(tbl, row_numbers = seq(20, nrow(tbl), by = 20)) |>
  save_html(path = tempfile(fileext = ".html"))
```

### Plain text (.txt)

The code below will save the table as a plain text (Markdown-formatted) file.

``` r
tbl |>
  save_txt(path = tempfile(fileext = ".txt"))
```

`save_txt()` also accepts `gt` tables directly.

``` r
gt::gt(head(mtcars)) |>
  save_txt(path = tempfile(fileext = ".txt"))
```

A list of tables can also be saved as plain text — each table is separated by a horizontal rule.

``` r
list(gt::gt(head(mtcars)), gt::gt(tail(mtcars))) |>
  save_txt(path = tempfile(fileext = ".txt"))
```
