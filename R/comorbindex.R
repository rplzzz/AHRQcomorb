#' Compute the AHRQ comorbidity index for a dataset.
#'
#' Given a table of comorbidity indicator variables, compute the AHRQ index
#' for risk of in-hospital mortality.
#'
#' AHRQ has produced risk indices for two outcomes:  in-hospital mortality
#' ('mort') and all-cause readmission ('readm').  In the future we may add
#' some indices for USNWR outcomes 30-day survival and discharge to home.
#' Adding these isn't entirely straightforward because they are calculated for
#' individual specialties and conditions.  The default is 'mort'.
#'
#' @param df A data frame of comorbidity indicators (1 if the comorbidity is
#' present, 0 if not).  There must be a column for each of the comorbidities
#' in the index.
#' @param indextype Which index to calculate, see details
#' @return A vector of comorbidity index values
#' @export
comorbindex <- function(df, indextype='mort')
{
  df <- as.data.frame(df)

  indexwgts <- list(
    mort = c(-4, -1, -3, -1, -4,  9, 6, 23,  0, 10,
              5,  0, 15,  5, -9, -2, 0, -7, 15,  1,
              0,  2, 17,  2, -1, 23, 2, -7,  4,  3,
             -9,  4,  3,  8, -3, -8, 0,  0, 14),
    readm = c(5, 3,  5, 2, 2, 10, 7, 11, 0, 7,
              0, 0,  3, 1, 2, 4,  0,  6, 7, 0,
              0, 3, 10, 4, 1, 2,  5, -2, 3, 1,
              6, 3,  4, 8, 0, 0,  2,  0, 6)
  )
  stopifnot(indextype %in% names(indexwgts))
  wgts <- matrix(indexwgts[[indextype]], ncol = 1)
  comorbs <- comorb_description$comorbidity

  comorb_mat <- as.matrix(df[comorbs])

  as.vector(comorb_mat %*% wgts)
}
