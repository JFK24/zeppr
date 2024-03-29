% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_series_tool.R
\name{index_trailing_values_if}
\alias{index_trailing_values_if}
\title{Binary Indexes consecutive trailing values complying to a condition.
The values are from a given numerical vector and the condition is tested by
comparing the values to a reference value using a common logical operator.}
\usage{
index_trailing_values_if(
  x,
  operator = "==",
  ref.value = 0,
  keep.na.values = FALSE
)
}
\arguments{
\item{x}{(numeric) vector of numeric values}

\item{operator}{(chr) type of comparison to perform among "==", "<", "<=", ">", ">=", "is.na"}

\item{ref.value}{(numeric) the reference value}

\item{keep.na.values}{(bool) NAs are left as is if TRUE, NAs in the trailing group are kept otherwise}
}
\value{
(boolean) vector (same length as \code{x}): TRUE for consecutive trailing values complying to the condition, FALSE otherwise
}
\description{
Binary Indexes consecutive trailing values complying to a condition.
The values are from a given numerical vector and the condition is tested by
comparing the values to a reference value using a common logical operator.
}
\examples{
x <- c(1, NA, 7, 8, 4, 1, 2, NA, 4)
index_trailing_values_if(x=x, operator="==", ref.value=4)
index_trailing_values_if(x=x, operator="==", ref.value=4, keep.na.values=TRUE)
index_trailing_values_if(x=x, operator=">=", ref.value=2, keep.na.values=TRUE)
}
