#' Functions for decoding POA flags
#'
#' There are a variety of conventions for representing the true/false/other
#' values that come up in the POA fields.  They differ somewhat in what they
#' use for true and false values, but the types of "other" allowed (if any) are
#' all over the map.  These functions decode several conventions in common use.
#'
#' In some cases the distinction between the different types of "other" results is
#' fuzzy.  For example, there is a case to be made that any entry other than a
#' clear "true" should be treated as "false", since the condition was not in fact
#' reported as POA.  These functions provide some flexibility in this treatment.
#'
#' @param poa Vector of POA values to be decoded.
#' @return Integer vector of 1 (true), 0 (false), and NA (other) values.
#' @name poa_functions
NULL

#' @describeIn poa_functions CMS POA convention, strict interpretation
#'
#' @details
#' \itemize{
#' \item \code{poa_cms} treats Y as true (\emph{uppercase only}), and any other result as false.
#' }
#' @export
poa_cms <- function(poa)
{
  ifelse(poa=='Y', 1L, 0L)
}

#' @describeIn poa_functions CMS POA convention
#'
#' @details
#' \itemize{
#' \item \code{poa_cms2} treats Y as true, N as false (\emph{uppercase only}), and anything else as "other"
#' }
#' @export
poa_cms2 <- function(poa)
{
  ifelse(poa=='Y',
         1L,
         ifelse(poa=='N', 0L,
                NA_integer_))
}

#' @describeIn poa_functions A default convention that reflects common usages.
#'
#' @details
#' \itemize{
#' \item \code{poa_natural} looks at the starting character and treats Y, T, and 1
#' as true; N, F, and 0 as false, and everything else as "other".
#' }
#' @export
poa_natural <- function(poa)
{
  ifelse(grepl('^[TY1]', poa, ignore.case=TRUE),
         1L,
         ifelse(grepl('^[NF0]', poa, ignore.case=TRUE), 0L,
                NA_integer_))
}
