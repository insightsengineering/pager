
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pager

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/pager)](https://CRAN.R-project.org/package=pager)
[![R-CMD-check](https://github.com/insightsengineering/pager/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/pager/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/pager/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/pager)
<!-- badges: end -->

The pager package makes it simple to save tables of class gtsummary, gt,
and flextable as a Word document using a reference document. This is
accomplished by creating the document via R markdown using the
`reference_docx:` field.

The package also supports lists of table objects. When a list is passed,
a page break is placed between each table in the list.

## Installation

You can install the development version of pager from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("insightsengineering/pager")
```

## Example

To begin, let’s create a summary table.

``` r
library(pager)

# create table
tbl <-
  cards::ADAE[1:150,] |>
  gtsummary::tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = TRTA,
    denominator = cards::ADSL,
    id = USUBJID,
  )
```

The code below will save the table as Word document using the default
portrait orientation reference document.

``` r
gtsummary::as_flex_table(tbl) |>
  save_with_rmarkdown(path = tempfile(fileext = ".docx"))
#> ✔ Writing '/var/folders/6f/gdjf_vxj2wl3jhmxdkd1hd_w0000gn/T//RtmpcdsRdZ/filee351225cee86.docx'
```

The example below first splits the summary table into a list of tables.
Each table is saved to a separate page in the resulting Word document.

``` r
gtsummary::tbl_split_by_rows(tbl, row_numbers = seq(20, nrow(tbl), by = 20)) |>
  save_with_rmarkdown(path = tempfile(fileext = ".docx"))
#> ✔ Writing '/var/folders/6f/gdjf_vxj2wl3jhmxdkd1hd_w0000gn/T//RtmpcdsRdZ/filee35112a6c071.docx'
```
