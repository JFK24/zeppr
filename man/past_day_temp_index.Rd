% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weather_indices.R
\name{past_day_temp_index}
\alias{past_day_temp_index}
\title{Past Day Temperature Index}
\usage{
past_day_temp_index(
  date,
  t.min,
  t.max,
  n.days = 1,
  from.month.day = NA,
  from.month = NA,
  from.date = NA,
  longest_period = FALSE,
  index = "heating"
)
}
\arguments{
\item{date}{(Date) vector of consecutive days (no missing values allowed)}

\item{t.min}{(numeric) vector of minimum day temperatures (same length as \code{date}) (no missing values allowed)}

\item{t.max}{(numeric) vector of maximum day temperatures (same length as \code{date}) (no missing values allowed)}

\item{n.days}{(integer) time window size in days (past and current days)}

\item{from.month.day}{(integer) past day in month to start
counting (overwrites \code{n.days} if not \code{NA}). To be defined together with \code{from.month}.
Year is not taken into account (may count past indices across several years).}

\item{from.month}{(integer) past month to start counting
(overwrites \code{n.days} if not \code{NA}). To be defined together with \code{from.month.day}.
Year is not taken into account (may count past indices across several years).}

\item{from.date}{(chr) past date to start counting (format: YYYY-MM-DD)
(overwrites \code{n.days}, \code{from.month.day} and \code{from.month} if not \code{NA})}

\item{longest_period}{(boolean) returns the maximal number of consecutive days
matching the index definition within the time window if TRUE, the number of
days matching the index definition within the time window otherwise.}

\item{index}{(chr) type of temperature index to calculate in English or German.
One of the following in English:
"ice", "frost", "vegetation", "heating", "summer", "tropical", "desertic".
Or one of the corresponding in German:
"Eistag", "Frosttag", "Vegetationstag", "Heiztag", "Sommertag",
"Tropentag", or "Wuestentag".}
}
\value{
(integer) a vector of values counting the number of days matching the
index definition. First values may be equal to \code{NA} if the time window can
not fit the number of available days (same length as \code{t.min}).
}
\description{
For a set of consecutive days (represented by vectors of dates, min and max
temperatures) and a given time window (smaller than the set), this function
calculates for each day either the number of days matching the definition of
a temperature index, or the longest period (smaller than the window) where
days matched the index definition. By default, it computes the index of each
day independently from other days (time window of 1 day). The daily data must
be complete (no missing or duplicated days) and sorted by date.
}
\details{
The time window is defined either by a number of days (accounting for the
current day and past days if required), a date in a year (a month and a
day of month; whatever the year), or a given full date (e.g. "2022-01-12").

Temperature indices are calculated by the function \code{\link[=day_temp_index]{day_temp_index()}}.
The following indices can be used:
ice (\code{t.max}<0), frost (\code{t.min}<0), vegetation ((\code{t.max}+\code{t.min})/2)>=5),
heating ((\code{t.max}+\code{t.min})/2)<15), summer (\code{t.max}>=25),
tropical (\code{t.max}>=30) or desertic day (\code{t.max}>=35).
}
\examples{
data <- data.frame(
  Date=as.Date(c("2022-01-10", "2022-01-11", "2022-01-12", "2022-01-13", "2022-01-14")),
  Tmin=c(4, 6, 11, 10, 20),
  Tmax=c(12, 14, 20, 12, 25))
print(data)
# Annotate each day (1 if heating day, 0 otherwise)
past_day_temp_index(data$Date, data$Tmin, data$Tmax, index="heating")
# Count past heating days in the last 2 days
past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=2, index="heating")
# Longest period of heating days in the last 5 days
past_day_temp_index(data$Date, data$Tmin, data$Tmax, n.days=5,
                    index="heating", longest_period=TRUE)
# Count days matching index definition since a given date in year (all years together)
past_day_temp_index(data$Date, data$Tmin, data$Tmax,
                    index="vegetation", from.month=1, from.month.day=11)
# Count days matching index definition since a given date
past_day_temp_index(data$Date, data$Tmin, data$Tmax,
                    index="vegetation", from.date="2022-01-12")
}
