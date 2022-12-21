# ==============================================================================
#' Normalized Cumulative Sum
#'
#' First The cumulative sum includes all values starting from the 1st element.
#' For example, cumulative sum of element i is equal to
#' sum(element 1, element 2, ... element i). Then, each value is normalized as a
#' percentage by dividing the value by the sum of all elements.
#'
#' @param values vector of numerical values (numeric)
#' @param normalize returns normalized cumulative sum if TRUE, simple cumulative sum otherwise (boolean)
#' @param na.replace numeric value used to replace NA values,
#' @return numeric vector of the normalized cumulative sum (from 0 to 1)
#' @examples
#' # Simple case
#' normalized_cumsum(c(1,2,3,4,5))
#' # not-normalized cumulative sum
#' normalized_cumsum(c(1,2,3,4, 5), normalize=FALSE)
#' # NAs are not processed by default
#' normalized_cumsum(c(1,2,3,4, NA, 5))
#' # replace NAs by 0 before summing
#' normalized_cumsum(c(1,2,3,4, NA, 5), na.replace=0)
#' @export
# ==============================================================================
normalized_cumsum <- function(values, normalize=TRUE, na.replace=NA){
  my.values <- values
  if(!is.na(na.replace) & is.numeric(na.replace)){
    my.values[is.na(my.values)] <- na.replace
  }
  if(normalize){
    return(cumsum(my.values) / sum(my.values, na.rm=TRUE))
  }
  return(cumsum(my.values))
}
