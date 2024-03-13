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
#' index_leading_values_if(x=x, operator="<", ref.value=8)
#' index_leading_values_if(x=x, operator="<", ref.value=8, keep.na.values=TRUE)
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
#' Counts consecutive leading values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the leading group are kept otherwise
#' @return (numeric): number of consecutive leading values complying to the condition
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_leading_values_if(x=x, operator="<", ref.value=8)
#' n_leading_values_if(x=x, operator="<", ref.value=8)
#' @export
# ==============================================================================
n_leading_values_if <- function(x, operator="==", ref.value=0, keep.na.values=FALSE) {
  my.index <- index_leading_values_if(x, operator, ref.value, keep.na.values)
  return(sum(my.index, na.rm=TRUE))
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
#' index_trailing_values_if(x=x, operator="==", ref.value=4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' index_trailing_values_if(x=x, operator=">=", ref.value=2, keep.na.values=TRUE)
#' @export
# ==============================================================================
index_trailing_values_if <- function(x, operator="==", ref.value=0, keep.na.values=FALSE) {
  my.index <- rev(index_leading_values_if(x=rev(x), operator=operator, ref.value=ref.value, keep.na.values=keep.na.values))
  return(my.index)
}


# ==============================================================================
#' Counts consecutive trailing values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the trailing group are kept otherwise
#' @return (numeric): number of consecutive trailing values complying to the condition
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' n_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' @export
# ==============================================================================
n_trailing_values_if <- function(x, operator="==", ref.value=0, keep.na.values=FALSE) {
  my.index <- index_trailing_values_if(x, operator, ref.value, keep.na.values)
  return(sum(my.index, na.rm=TRUE))
}


# ==============================================================================
#' Binary Indexes the first value in a vector complying to a condition.
#' The value is from a given numerical vector and the condition is tested by
#' comparing the vector values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param complete.to.end (bool) change the output by returning TRUE for the
#' first value complying to the condition and also for all values after the first,
#' FALSE otherwise
#' @return (boolean) vector (same length as `x`): TRUE for the first value complying to the condition, FALSE otherwise
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=FALSE)
#' index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=TRUE)
#' index_first_value_if(x=x, operator="is.na")
#' @export
# ==============================================================================
index_first_value_if <- function(x, operator="==", ref.value=0, complete.to.end=FALSE) {
  op.res <- is.na(x)
  if(operator %in% c("==", "<", "<=", ">", ">=")){
    op.res <- get(operator)(x, ref.value)
  }
  my.index <- which(op.res)
  y <- x & FALSE
  if(!length(my.index)==0){
    my.first.val.index <- min(my.index)
    if(!complete.to.end){
      y[my.first.val.index] <- TRUE
    } else{
      y[my.first.val.index:length(y)] <- TRUE
    }
  }
  return(y)
}

