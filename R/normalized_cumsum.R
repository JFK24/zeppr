# ==============================================================================
#' Normalized Cumulative Sum
#'
#' From an input vector of numerical values, this function first calculates the
#' cumulative sum for each element of the vector. An element is summed to all
#' its predecessors in the vector (the order of the input vector is used).
#' Then, it divides the new values by the sum of all values.
#' The resulting values will range from 0 to 1 as percentages.
#'
#' @param values (numeric) vector of numerical values
#' @param normalize (boolean) returns normalized cumulative sum if `TRUE`, simple cumulative sum otherwise
#' @param na.replace (dbl) numeric value used to replace `NA` values (default=`NA` to not replace)
#' @return (numeric) vector of the cumulative sum normalized by default from 0 to 1
#' @examples
#' # Simple case:
#' normalized_cumsum(c(1,2,3,4,5))
#' # not-normalized cumulative sum:
#' normalized_cumsum(c(1,2,3,4, 5), normalize=FALSE)
#' # NAs are not processed by default:
#' normalized_cumsum(c(1,2,3,4, NA, 5))
#' # replace NAs by 0 before summing:
#' normalized_cumsum(c(1,2,3,4, NA, 5), na.replace=0)
#' @export
# ==============================================================================
normalized_cumsum <- function(values, normalize=TRUE, na.replace=NA){
  my.values <- values
  if(!is.na(na.replace) & is.numeric(na.replace)){
    my.values[is.na(my.values)] <- na.replace
  }
  if(normalize & length(values)>1){
    return(cumsum(my.values) / sum(my.values, na.rm=TRUE))
  }
  return(cumsum(my.values))
}
