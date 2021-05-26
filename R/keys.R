#' Convenience functions for merging and separating keys
#'
#' @param tbl Input table
#' @param idcols Names of the columns with the separated keys
#' @param synthid Name of the column with the synthetic key
#' @param drop If \code{TRUE}, remove the input column(s) from the output
#' @param sepchar Character to use to separate elements in the synthetic key
#' (default is ASCII Unit Separator (\code{^_})
#'
#' @name keyfuns
NULL

#' @describeIn keyfuns Merge two or more columns into a synthetic key
#' @export
mergekeys <- function(tbl, idcols, synthid='id', drop=TRUE, sepchar=intToUtf8(31))
{
  stopifnot(length(idcols) > 1)
  stopifnot(all(idcols %in% names(tbl)))

  keys <- lapply(idcols, function(n) {tbl[[n]]})
  synthkey <- do.call(paste, c(keys, sep=sepchar))

  if(drop) {
    ## drop the original id columns if requested
    tbl <- tbl[-match(idcols, names(tbl))]
  }

  tbl[[synthid]] <- synthkey
  tbl[unique(c(synthid, names(tbl)))]   # Move the new key to the first column.
}

#' @describeIn keyfuns Separate synthetic key into two or more columns
#' @export
splitkeys <- function(tbl, idcols, synthid='id', drop=TRUE, sepchar=intToUtf8(31))
{
  tidyr::separate(tbl, synthid, idcols, sepchar, drop)
}
