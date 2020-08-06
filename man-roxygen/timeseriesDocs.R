#' @title
#' Get quality-checked timeseries data
#'
#' @description
#' This function returns a quality-checked timeseries from Water Data Online.
#'
#' @details
#' For the parameters (and their units) that can be requested, see
#' \link{parameters}. More information can be found in the Water Data Online
#' SOS manual (URL in See Also below).
#'
#' @param parameter_type The water data parameter type (e.g. Water Course
#' Discharge). See below for a list of available parameters
#' @param station_number The AWRC station number.
#' @param start_date Start date formatted as a string or date class
#' (YYYY-MM-DD).
#' @param end_date End date formatted as a string or date class (YYYY-MM-DD).
#' @param tz Optional: the desired time zone for the output timeseries. Input
#' must be an Olson Name (see `OlsonNames()`). By default the the timeseries
#' is returned in an offset timezone (e.g. `Etc/GMT-10` for NSW) as the
#' timeseries do not observe DST.
#' @param return_fields Optional: columns to be returned from Water Data Online.
#' By default Timestamp Value and Quality Code are returned.
#' @md
#'
#' @return A tibble with the requested return fields, which by default are
#' `Timestamp`, `Value` and `Quality Code`. Zero row tibbles are returned if
#' no data is available for the requested dates. The aggregation of data is
#' generally the mean for most variables, except for rainfall and evaporation
#' which is the sum over the chosen period.
#'
#' @seealso
#' * \url{http://www.bom.gov.au/waterdata/}
#' * \url{http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sens
#'       or\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%
#'       20Online\%20v1.0.1.pdf}
#'
#' @author Alexander Buzacott
#'
#' @examples
#' get_hourly(parameter_type = 'Water Course Discharge',
#'            station_number = '410730',
#'            start_date     = '2020-01-01',
#'            end_date       = '2020-01-31')
#'
#' get_daily(parameter_type = 'Water Course Discharge',
#'           station_number = '410730',
#'           start_date     = '2020-01-01',
#'           end_date       = '2020-01-31',
#'           var            = 'Mean',
#'           aggregation    = '09HR')
#'
#' get_yearly(parameter_type = 'Rainfall',
#'            station_number = '570946',
#'            start_date     = 2016,
#'            end_date       = 2020)
#'
