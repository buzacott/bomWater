#' @title
#' Get formatted timeseries data
#'
#' @description
#' This function returns a formatted timeseries from Water Data Online.
#'
#' @details
#' This function returns a timeseries within the chosen date window. The units of the timeseries values are as per
#' Water Data Online and can be viewed on page 39 of the Water Data Online SOS manual (linked in See Also below).
#'
#' @param parameterType The water data parameter type (e.g. Water Course Discharge). See below for
#' a list of available parameters
#' @param stationNumber The AWRC station number.
#' @param startDate Start date formatted as a string or date class (YYYY-MM-DD)
#' @param endDate End date formatted as a string or date class (YYYY-MM-DD)
#' @param tz Optional: the desired time zone for the output timeseries. Input must be an Olson Name
#' (see `OlsonNames()`). By default the the timeseries is returned in an offset timezone
#' (e.g. `Etc/GMT-10` for NSW) as the timeseries do not observe DST.
#' @param returnFields Optional: columns to be returned from Water Data Online. By default Timestamp
#' Value and Quality Code are returned.
#'
#' @details
#' The following parameters (`parameterType`) can be requested:
#'
#' * Water Course Discharge
#' * Water Course Level
#' * Storage Volume
#' * Storage Level
#' * Ground Water Level
#' * Rainfall
#' * Evaporation
#' * Dry Air Temperature
#' * Relative Humidity
#' * Wind Speed
#' * Electrical conductivity at 25C
#' * Turbidity
#' * Water pH
#' * Water Temperature
#' @md
#
#' @return A tibble with the requested return fields, which by default are `Timestamp`, `Value` and `Quality Code`.
#' Zero row tibbles are returned if no data is available for the requested dates. The aggregation of data
#' is generally the mean for most variables, except for rainfall and evaporation which is the sum over the chosen period.
#'
#' @seealso
#' \url{http://www.bom.gov.au/waterdata/}
#' \url{http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf}
#'
#' @author Alexander Buzacott
#'
#' @examples
#' \dontrun{
#' getHourlyData(parameterType = 'Water Course Discharge',
#'               stationNumber = '410730',
#'               startDate     = '2020-01-01',
#'               endDate       = '2020-01-31')
#' }
#' \dontrun{
#' getDailyData(parameterType = 'Water Course Discharge',
#'              stationNumber = '410730',
#'              startDate     = '2020-01-01',
#'              endDate       = '2020-01-31',
#'              var           = 'Mean',
#'              aggregation   = '09HR')
#' }
#' \dontrun{
#' getYearly(parameterType = 'Rainfall',
#'           stationNumber = '570946',
#'           startYear     = 2016,
#'           endYear       = 2020)
#' }


