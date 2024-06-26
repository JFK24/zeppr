% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_series_tool.R
\name{index_leading_values_if}
\alias{index_leading_values_if}
\title{Binary Indexes consecutive leading values complying to a condition.
The values are from a given numerical vector and the condition is tested by
comparing the values to a reference value using a common logical operator.}
\usage{
index_leading_values_if(
  x,
  operator = "==",
  ref.value = 0,
  order.by = NA,
  keep.na.values = FALSE
)
}
\arguments{
\item{x}{(numeric) vector of numeric values}

\item{operator}{(chr) type of comparison to perform among: "==", "<", "<=", ">", ">=", "is.na"}

\item{ref.value}{(numeric) the reference value}

\item{order.by}{(numeric) optional vector (same length as \code{x}): from which to take the ascending order to process \code{x}}

\item{keep.na.values}{(bool) NAs are left as is if TRUE, NAs in the leading group are kept otherwise}
}
\value{
(boolean) vector (same length as \code{x}): TRUE for consecutive leading values complying to the condition, FALSE otherwise
}
\description{
Binary Indexes consecutive leading values complying to a condition.
The values are from a given numerical vector and the condition is tested by
comparing the values to a reference value using a common logical operator.
}
\examples{
y <- c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04")
y <- rev(y)
x <- c(1, 7, 8, 4)
index_leading_values_if(x=x, operator="==", ref.value=1, order.by=NA)
index_leading_values_if(x=x, operator="<=", ref.value=1, order.by=NA)
x[order(y)]
index_leading_values_if(x=x, operator="<=", ref.value=7, order.by=y)
x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
index_leading_values_if(x=x, operator="<", ref.value=8)
index_leading_values_if(x=x, operator="<", ref.value=8, keep.na.values=TRUE)
# Using a second vector y from which we take the order
y <- c(1,3,5,6,7,9,2,4,8)
x[order(y)]
index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=FALSE)
index_leading_values_if(x=x, operator="<=", ref.value=2, order.by=y, keep.na.values=TRUE)
}
