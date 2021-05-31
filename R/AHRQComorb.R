#' AHRQComorb: Convert diagnosis codes to table of comorbidities
#'
#' Convert ICD-10 diagnosis codes to tables of Elixhauser comorbidities
#' using the comorbidity definitions from the US Agency for Healthcare
#' Research and Quality (AHRQ), version 2021.1.  This package is intended
#' for use with the Standard Analytical Files (SAF) distributed by the
#' Centers for Medicare and Medicaid Services (CMS); however, the
#' interface is flexible enough that it should work with most other
#' datasets containing similar data.
#'
#'
#' @section Basic functionality:
#'
#' The main function for converting diagnosis codes is \code{\link{comorb}}. It takes
#' a long-format table of diagnosis codes, one code (and POA indicator)
#' per row.  For example:
#'
#' ```
#' id          diagnosis   poa
#' <chr>       <chr>     <dbl>
#' "15\u001f1" I2510         1
#' "15\u001f1" I959          1
#' "15\u001f1" J449          1
#' "15\u001f1" E119          1
#' "15\u001f1" R001          1
#'  ```
#'
#' The diagnosis column must contain ICD-10 codes, and the poa column
#' must be a numeric column consisting entirely of 1 (present on
#' admission) and 0 (not present on admission) values.  The id column
#' indicates which rows belong to a single case (e.g., a patient, or an
#' encounter).  If this table were stored in `dtbl`, you would run:
#' ```
#' comorb(dtbl, idcol='id')
#' ```
#' The result would be a table with a row for each unique value of the id
#' variable (just one, in this short example), and a series of columns,
#' one for each of the 39 comorbidities.  Ones in these columns indicate
#' the presence of the comorbidities, while zeros indicate absence.
#'
#' The R manual page for the `comorb` function contains further details
#' about additional options and some notes on topics like groups of
#' clinically similar comorbidities.
#'
#' @section Additional functionality:
#' Medical records, including the SAF, are often organized with cases in
#' rows, and diagnoses and other information in columns.  The
#' \code{\link{claimpivot}} function pivots this wide-format data into long format,
#' taking care to keep claims together with their corresponding POA
#' indicators, and discarding unused columns.
#'
#' A table of the 39 comorbidities tracked in the standard, along with an
#' indicator for whether or not each one is required to be present on
#' admission to be counted, is provided in the `comorb_description`
#' table.  The \code{comorbidity} column of this table contains the
#' abbreviations for the various comorbidities, and is therefore useful
#' for decoding the abbreviations, as well as for iterating over all of
#' the possible comorbidities.
#'
#' @docType package
#' @name AHRQComorb
#' @md
NULL
