#' Description of comorbidities
#'
#' Table of comorbidity abbreviations, and a description of each comorbidity,
#' as of AHRQ version 2023.1
#'
#' @format Data frame with 3 columns:
#' \describe{
#' \item{comorbidity}{Abbreviation for the comorbidity.}
#' \item{description}{Description of the comorbidity.}
#' \item{poa}{Flag indicating whether or not the comorbidity must be present on
#' arrival to be counted.}
#' }
#' @source US Agency for Healthcare Research and Quality
#' \url{https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp#down}
"comorb_description"
