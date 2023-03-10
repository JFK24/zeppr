# ==============================================================================
#' Day Temperature Index
#'
#' Calculates weather indices from daily weather data:
#' ice, frost, vegetation, heating, summer, tropical, hot or desertic day.
#' It is based on definitions provided by the German Weather Service (see sources).
#'
#' Sources in German:
#' https://de.wikipedia.org/wiki/Klimatologie
#' https://www.dwd.de/DE/service/lexikon/Functions/glossar.html?nn=103346&lv2=101334&lv3=101452
#'
#' @param t.min (numeric) vector of minimum day temperatures
#' @param t.max (numeric) vector of maximum day temperatures (same length as `t.min`)
#' @param index (chr) type of temperature index to calculate in English or German.
#' One of the following in English:
#' "ice", "frost", "vegetation", "heating", "summer", "tropical", "hot", "desertic".
#' Or one of the corresponding in German:
#' "Eistag", "Frosttag", "Vegetationstag", "Heiztag", "Sommertag", "Tropennacht",
#' "Heisser_Tag", or "Wuestentag".
#' @return  (boolean) vector of TRUE or FALSE values if  (same length as `t.min`)
#' @examples
#' day_temperature_index(t.min=12, t.max=32, index="hot")
#' day_temperature_index(t.min=c(12, 15), t.max=c(15, 30), index="heating")
#' @export
# ==============================================================================
day_temperature_index <- function(t.min, t.max, index="cold") {
  if(index=="ice"        | index=="Eistag"){return(t.max<0)}
  if(index=="frost"      | index=="Frosttag"){return(t.min<0)}
  if(index=="vegetation" | index=="Vegetationstag"){return(((t.max+t.min)/2)>=5)}
  if(index=="heating"    | index=="Heiztag"){return(((t.max+t.min)/2)<15)}
  if(index=="summer"     | index=="Sommertag"){return(t.max>=25)}
  if(index=="tropical"   | index=="Tropennacht"){return(t.min>=20)}
  if(index=="hot"        | index=="Heisser_Tag"){return(t.max>=30)}
  if(index=="desertic"   | index=="Wuestentag"){return(t.max>=35)}
  warning("Unknown temperature index")
  return(FALSE)
}


# ==============================================================================
#' Adds Columns for Daily Temperature Indices to a Data Frame
#'
#' The input data frame must contain daily weather data and include columns for
#' min and max temperatures. A new column will be added for each requested
#' temperature index. The new columns will contain boolean values: TRUE if the
#' day temperatures match the index definition, FALSE otherwise.
#'
#' It is based on definitions provided by the German Weather Service. Sources:
#'
#' https://de.wikipedia.org/wiki/Klimatologie
#'
#' https://www.dwd.de/DE/service/lexikon/Functions/glossar.html?nn=103346&lv2=101334&lv3=101452
#'
#' @param df (data.frame) table with columns for min and max day temperatures
#' @param t.min non-quoted column name containing minimum day temperatures (numeric column)
#' @param t.max non-quoted column name containing maximum day temperatures (numeric column)
#' @param german.output (boolean) returns column names in German if TRUE, in English otherwise
#' @param indices (chr vector) vector of index names in English to add to the
#' data frame. All available indices are added by Default. Available indices:
#' "ice", "frost", "vegetation", "heating", "summer", "tropical", "hot", "desertic".
#' @return (data.frame) table copying `df` but adding a boolean column for
#' each requested index. TRUE if the day temperatures match the index definition,
#' FALSE otherwise.
#' @examples
#' # given a simple data.frame with date, min and max day temperatures
#' data <- data.frame(
#'   Date=as.Date(c("2022-01-01", "2022-01-02", "2022-01-02")),
#'   Tmin=c(4, 6, 11),
#'   Tmax=c(12, 14, 20))
#' # we add columns for all or selected temperature indices:
#' mutate_day_temperature_index(data, Tmin, Tmax)
#' mutate_day_temperature_index(data, Tmin, Tmax, indices=c("frost", "heating"))
#' mutate_day_temperature_index(data, Tmin, Tmax, german.output=TRUE)
#' # With pipes from magrittr:
#' library(magrittr)
#' data %>% mutate_day_temperature_index(Tmin, Tmax)
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
# ==============================================================================
mutate_day_temperature_index <- function(
    df, t.min, t.max, german.output=FALSE,
    indices=c("ice", "frost", "vegetation", "heating", "summer", "tropical", "hot", "desertic")){

  if(german.output){
    german <- c("Eistag", "Frosttag", "Vegetationstag", "Heiztag", "Sommertag", "Tropennacht", "Heisser_Tag", "Wuestentag")
    german <- stats::setNames(german, c("ice", "frost", "vegetation", "heating", "summer", "tropical", "hot", "desertic"))
    indices <- german[indices]
  }

  for (index in indices) {
    df <- df %>%
      dplyr::mutate({{index}}:=day_temperature_index({{t.min}}, {{t.max}}, index))
  }
  return(df)
}
