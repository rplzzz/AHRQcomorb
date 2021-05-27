#' Pivot a record of secondary diagnoses into long format
#'
#' Pivot a record into long format, keeping paired diagnosis and POA fields together
#' in the new format.
#'
#' Often a record for which we want to find the comorbidities will be organized
#' with one row per record, and the diagnoses and POA indicators as fields within
#' the row.  This function pivots such tables into a long format with one row per
#' diagnosis, and fields for the identifier, diagnosis, and POA flag.  (The difference
#' between this and an ordinary pivot is the pairing of the diagnosis and POA fields.)
#'
#' The diagnosis and POA fields are identified by a stem, followed by a number. For
#' example, "ICD_DGNS_CD13" for the 13th diagnosis code and "CLM_POA_IND_SW13" for
#' the corresponding POA flag.  In the output, each diagnosis will be matched up
#' with its POA, and the ID will be replicated for each row.
#'
#' If the POA column is of type character, it will be converted to an integer 0/1
#' value.  This behavior can be suppressed by setting \code{poaconv} to \code{FALSE}.
#' By default, strings starting with 'T' or 'Y' will be treated as true (1), and
#' strings starting with 'F' or 'N' will be treated as false (0).  If you happen
#' to know the strings used to represent true and false in your dataset, you can
#' pass them as \code{poastrings} (vector of length 2, with the string for true first).
#' Either way, entries that match neither will be assigned the value NA.  Optionally,
#' the function can raise an error if this happens (pass \code{\link[stats]{na.fail}}
#' in the \code{nahandle} parameter.)
#'
#' @param tbl Table of data in wide format
#' @param idcols Name of the columns used as identifiers. If more than one, they will
#' be joined into a single synthetic key called 'id' (using \code{\link{mergekeys}}) because
#' \code{\link{comorb}} only accepts one key column.
#' @param diagstem Character stem for the names of the diagnosis fields (default is
#' \code{'ICD_DGNS_CD'}).
#' @param poastem Character stem for the names of the POA fields (default is
#' \code{'CLM_POA_IND_SW'})
#' @param poastrings Strings used to represent true and false in the POA field. If
#' supplied, it should be in the form \code{c(trueval, falseval)}.  If omitted a
#' reasonable guess will be used.
#' @param nahandle What to do with POA values that don't match either the true
#' or false strings.  Default is to leave them as \code{NA} in the output.
#' @importFrom stats na.pass
#' @export
claimpivot <- function(tbl, idcols, diagstem='ICD_DGNS_CD', poastem='CLM_POA_IND_SW',
                       poastrings=NULL, poaconv=TRUE, nahandle=na.pass)
{
  if(length(idcols) > 1) {
    tbl <- mergekeys(tbl, idcols)
    idcols <- 'id'
  }

  diagpat <- paste0('^', diagstem, '[0-9]+')
  diagcols <- grep(diagpat, names(tbl))
  poapat <- paste0('^', poastem, '[0-9]+')
  poacols <- grep(poapat, names(tbl))
  idcoln <- which(names(tbl) == idcols)

  diagtbl <- tidyr::pivot_longer(tbl[c(idcoln, diagcols)], tidyr::starts_with(diagstem),
                                 names_to = 'num', names_prefix = diagstem,
                                 values_to = 'diagnosis', values_drop_na = TRUE)
  diagtbl$diagnosis <- trimws(diagtbl$diagnosis, 'both')
  diagtbl <- diagtbl[diagtbl$diagnosis != '',]      # Drop empty diagnoses

  ## Ok, so CMS represents true/false as "Y/0", meaning that it's possible to have
  ## some of the columns end up as a numeric type, and that causes pivot_longer to barf.
  ## Guard against this by forcing all to character
  for(coln in poacols) {
    tbl[[coln]] <- as.character(tbl[[coln]])
  }
  poatbl <- tidyr::pivot_longer(tbl[c(idcoln, poacols)], tidyr::starts_with(poastem),
                                names_to = 'num', names_prefix = poastem,
                                values_to = 'poa', values_drop_na = TRUE)

  if(poaconv) {
    ## Convert POA strings to boolean values, coded as integers
    if(is.null(poastrings)) {
      truevals <- grepl('^[ty1]', poatbl$poa, ignore.case = TRUE)
      falsevals <- grepl('^[fn0]', poatbl$poa, ignore.case = TRUE)
    }
    else {
      truevals <- poatbl$poa == poastrings[1]
      falsevals <- poatbl$poa == poastrings[2]
    }

    poatbl$poa <- ifelse(truevals, 1L,
                         ifelse(falsevals, 0L,
                                NA_integer_))
  }

  ## join the two tables and check for NA values, if required.
  nahandle(dplyr::left_join(diagtbl, poatbl, by=c(idcols, 'num')))[c(idcols, 'diagnosis', 'poa')]
}