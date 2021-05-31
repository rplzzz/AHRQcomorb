#' Logical reductions, with integer return
#'
#' Perform a logical reduction on a numeric values and return the result as
#' an integer.
#'
#' So far we've only implemented the combinations (vector/parallel x and/or) that
#' are actually used in package functions.
#'
#' @name integer_logic
#' @param x,y Vectors of numeric values to be interpreted as logical values
#' @keywords internal
NULL

#' @describeIn integer_logic Vector integer or
intor <- function(x) {
  as.integer(sum(x!=0) > 0)
}

#' @describeIn integer_logic Parallel integer and
pintand <- function(x,y) {
  r <- (x != 0) * (y != 0)
  as.integer(r)
}
