# ==============================================================================
#' Binary Indexes consecutive leading values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the leading group are kept otherwise
#' @return (boolean) vector (same length as `x`): TRUE for consecutive leading values complying to the condition, FALSE otherwise
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_leading_values_if(x, "<", 8)
#' index_leading_values_if(x, "<", 8, keep.na.values=TRUE)
#' @export
# ==============================================================================
index_leading_values_if <- function(x, operator="==", ref.value=0, keep.na.values=FALSE) {
  op.res <- is.na(x)
  if(operator %in% c("==", "<", "<=", ">", ">=")){
    op.res <- get(operator)(x, ref.value)
  }
  na.mask <- op.res
  if(!keep.na.values){na.mask <- TRUE}
  my.index=na.mask & normalized_cumsum(!op.res, normalize = TRUE, na.replace = 0)==0
  my.index

  return(my.index)
}


# ==============================================================================
#' Binary Indexes consecutive trailing values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the trailing group are kept otherwise
#' @return (boolean) vector (same length as `x`): TRUE for consecutive trailing values complying to the condition, FALSE otherwise
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_trailing_values_if(x, "==", 4)
#' index_trailing_values_if(x, "==", 4, keep.na.values=TRUE)
#' index_trailing_values_if(x, ">=", 2, keep.na.values=TRUE)
#' @export
# ==============================================================================
index_trailing_values_if <- function(x, operator="==", ref.value=0, keep.na.values=FALSE) {
  my.index <- rev(index_leading_values_if(x=rev(x), operator=operator, ref.value=ref.value, keep.na.values=keep.na.values))
  return(my.index)
}
