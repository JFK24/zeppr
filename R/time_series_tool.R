# ==============================================================================
#' Binary Indexes consecutive leading values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the leading group are kept otherwise
#' @return (boolean) vector (same length as `x`): TRUE for consecutive leading values complying to the condition, FALSE otherwise
#' @examples
#' y <- c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04")
#' y <- rev(y)
#' x <- c(1, 7, 8, 4)
#' index_leading_values_if(x=x, operator="==", ref.value=1, order.by=NA)
#' index_leading_values_if(x=x, operator="<=", ref.value=1, order.by=NA)
#' x[order(y)]
#' index_leading_values_if(x=x, operator="<=", ref.value=7, order.by=y)
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_leading_values_if(x=x, operator="<", ref.value=8)
#' index_leading_values_if(x=x, operator="<", ref.value=8, keep.na.values=TRUE)
#' # Using a second vector y from which we take the order
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=FALSE)
#' index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=TRUE)
#' @export
# ==============================================================================
index_leading_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  # sum and length are equal to 1 if order.by=NA, to 0 if order.by=c(), to 2 order.by=c(NA, NA)
  if(sum(is.na(order.by))==length(order.by)){order.by=1:length(x)}
  my.order <- order(order.by)
  x_ordered <- x[my.order]

  op.res <- is.na(x_ordered)
  if(operator %in% c("==", "<", "<=", ">", ">=")){
    op.res <- get(operator)(x_ordered, ref.value)
  }
  na.mask <- op.res
  if(!keep.na.values){na.mask <- TRUE}
  my.index=na.mask & normalized_cumsum(!op.res, normalize = TRUE, na.replace = 0)==0

  return(my.index[order(my.order)])
}

# # Original !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# index_leading_values_if <- function(x, operator="==", ref.value=0, keep.na.values=FALSE) {
#   op.res <- is.na(x)
#   if(operator %in% c("==", "<", "<=", ">", ">=")){
#     op.res <- get(operator)(x, ref.value)
#   }
#   na.mask <- op.res
#   if(!keep.na.values){na.mask <- TRUE}
#   my.index=na.mask & normalized_cumsum(!op.res, normalize = TRUE, na.replace = 0)==0
#   my.index
#
#   return(my.index)
# }


# ==============================================================================
#' Counts consecutive leading values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the leading group are kept otherwise
#' @return (numeric): number of consecutive leading values complying to the condition
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_leading_values_if(x=x, operator="<", ref.value=8)
#' n_leading_values_if(x=x, operator="<", ref.value=8)
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_leading_values_if(x=x[order(y)], operator="<", ref.value=8, order.by=NA)
#' index_leading_values_if(x=x, operator="<", ref.value=8, order.by=y)
#' n_leading_values_if(x=x, operator="<", ref.value=8, order.by=y)
#' @export
# ==============================================================================
n_leading_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  my.index <- index_leading_values_if(x=x, operator=operator, ref.value=ref.value, order.by=order.by, keep.na.values=keep.na.values)
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
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the trailing group are kept otherwise
#' @return (boolean) vector (same length as `x`): TRUE for consecutive trailing values complying to the condition, FALSE otherwise
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' index_trailing_values_if(x=x, operator=">=", ref.value=2, keep.na.values=TRUE)
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_trailing_values_if(x=x, operator="<=", ref.value=4, order.by=y, keep.na.values=FALSE)
#' @export
# ==============================================================================
index_trailing_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  if(sum(is.na(order.by))==length(order.by)){order.by=1:length(x)}
  my.order <- order(order.by)
  x_ordered <- x[my.order]
  # my.index <- rev(index_leading_values_if(x=rev(x), operator=operator, ref.value=ref.value, order.by=order.by, keep.na.values=keep.na.values))
  my.index <- rev(index_leading_values_if(x=rev(x_ordered), operator=operator, ref.value=ref.value, order.by=NA, keep.na.values=keep.na.values))
  # return(my.index)
  return(my.index[order(my.order)])
}


# ==============================================================================
#' Counts consecutive trailing values complying to a condition.
#' The values are from a given numerical vector and the condition is tested by
#' comparing the values to a reference value using a common logical operator.
#'
#' @param x (numeric) vector of numeric values
#' @param operator (chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"
#' @param ref.value (numeric) the reference value
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param keep.na.values (bool) NAs are left as is if TRUE, NAs in the trailing group are kept otherwise
#' @return (numeric): number of consecutive trailing values complying to the condition
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' n_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_trailing_values_if(x=x, operator="<=", ref.value=4, order.by=y, keep.na.values=FALSE)
#' n_trailing_values_if(x=x, operator="<=", ref.value=4, order.by=y, keep.na.values=FALSE)
#' @export
# ==============================================================================
n_trailing_values_if <- function(x, operator="==", ref.value=0, order.by=NA, keep.na.values=FALSE) {
  my.index <- index_trailing_values_if(x=x, operator=operator, ref.value=ref.value, order.by=order.by, keep.na.values=keep.na.values)
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
#' @param order.by (numeric) optional vector (same length as `x`): from which to take the ascending order to process `x`
#' @param complete.to.end (bool) change the output by returning TRUE for the
#' first value complying to the condition and also for all values after the first,
#' FALSE otherwise
#' @return (boolean) vector (same length as `x`): TRUE for the first value complying to the condition, FALSE otherwise
#' @examples
#' x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
#' index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=FALSE)
#' index_first_value_if(x=x, operator=">", ref.value=6, complete.to.end=TRUE)
#' index_first_value_if(x=x, operator="is.na")
#' y <- c(1,3,5,6,7,9,2,4,8)
#' x[order(y)]
#' index_first_value_if(x=x[order(y)], operator=">", ref.value=6, order.by=NA, complete.to.end=FALSE)
#' index_first_value_if(x=x[order(y)], operator=">", ref.value=6, order.by=NA, complete.to.end=TRUE)
#' index_first_value_if(x=x, operator=">", ref.value=6, order.by=y, complete.to.end=FALSE)
#' index_first_value_if(x=x, operator=">", ref.value=6, order.by=y, complete.to.end=TRUE)
#' @export
# ==============================================================================
index_first_value_if <- function(x, operator="==", ref.value=0, order.by=NA, complete.to.end=FALSE) {
  # sum and length are equal to 1 if order.by=NA, to 0 if order.by=c(), to 2 order.by=c(NA, NA)
  if(sum(is.na(order.by))==length(order.by)){order.by=1:length(x)}
  my.order <- order(order.by)
  x_ordered <- x[my.order]

  op.res <- is.na(x_ordered)
  if(operator %in% c("==", "<", "<=", ">", ">=")){
    op.res <- get(operator)(x_ordered, ref.value)
  }
  my.index <- which(op.res)
  y <- x_ordered & FALSE
  if(!length(my.index)==0){
    my.first.val.index <- min(my.index)
    if(!complete.to.end){
      y[my.first.val.index] <- TRUE
    } else{
      y[my.first.val.index:length(y)] <- TRUE
    }
  }
  # return(y)
    return(y[order(my.order)])
}

# # Original
# index_first_value_if <- function(x, operator="==", ref.value=0, complete.to.end=FALSE) {
#   op.res <- is.na(x)
#   if(operator %in% c("==", "<", "<=", ">", ">=")){
#     op.res <- get(operator)(x, ref.value)
#   }
#   my.index <- which(op.res)
#   y <- x & FALSE
#   if(!length(my.index)==0){
#     my.first.val.index <- min(my.index)
#     if(!complete.to.end){
#       y[my.first.val.index] <- TRUE
#     } else{
#       y[my.first.val.index:length(y)] <- TRUE
#     }
#   }
#   return(y)
# }


