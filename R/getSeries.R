#' @title Get time series
#' @md
#' @description Get timeseries data from Water Data online
#' @details This function can be used if you want to retrieve a specific
#' timeseries that is not the default quality checked one.
#'
#' Common valid return fields are:
#'
#' * Timestamp
#' * Value
#' * Quality Code
#' * Interpolation Type
#'
#' Other valid return fields (depending on the parameter requested) may be:
#'
#' * Absolute Value
#' * AV Interpolation
#' * AV Quality Code
#' * Runoff Value
#' * RV Interpolation
#' * RV Quality Code
#' * Aggregation
#' * Accuracy
#'
#' If the request is not valid it will fail.
#' @param parameter_type The water data parameter type (e.g. Water Course
#' Discharge). See \code{\link{parameters()}} for a full list.
#' @param station_number The AWRC station number.
#' @param start_date Start date formatted as a string or date class
#' (YYYY-MM-DD).
#' @param end_date End date formatted as a string or date class (YYYY-MM-DD).
#' @param tz Optional: the desired time zone for the output timeseries. Input
#' must be an Olson Name (see `OlsonNames()`). By default the timeseries are
#' returned in non-DST time zones (AEST, ACST or AWST) depending on the
#' station location.
#' @param return_fields Optional: columns to be returned from Water Data Online.
#' By default Timestamp, Value and Quality Code are returned.
#' @param ts_name The timeseries name (e.g. DMQaQc.Merged.DailyMean.24HR) that
#' is desired.
#' @return
#' A tibble with columns with the requested return_fields. A zero row tibble is
#' returned if no data is returned  from the query. The columns of the tibble
#' are returned as character classes and have not been formatted to more
#' appropriate correct classes (this happens in other functions).
#' @seealso
#' * \url{http://www.bom.gov.au/waterdata/}
#' * [BoM Guide to Sensor Observation Services (SOS2) for Water Data Online](http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf)
#'
#' @examples
#' # Accessible dam storage, as shown on the BoM Water Storage dashboard
#' \dontrun{
#' get_timeseries(
#'   parameter_type = "Storage Volume",
#'   "G8150011",
#'   "2020-01-01",
#'   "2020-01-31",
#'   ts_name = "PR02AVQaQc.Merged.DailyMean.24HR",
#'   tz = NULL,
#'   return_fields = c("Timestamp", "Value", "Quality Code")
#' )
#' }
#' # See the linked SOS2 manual in See Also to find more timeseries names
#' @author Alexander Buzacott
#' @export

get_timeseries <- function(parameter_type,
                           station_number,
                           start_date,
                           end_date,
                           tz,
                           return_fields,
                           ts_name) {

  # If no tz supplied, get from jurisdiction
  if (is.null(tz)) {
    # From BoM website:
    # Which time zones are the data displayed in?
    #   Time of day is presented in local standard time. Coordinated Universal Timezones (UTC) are:
    #
    #   Eastern States (QLD, NSW, ACT, VIC, TAS) - UTC +10:00
    #   Central States (NT, SA) - UTC +09:30
    #   Western Australia - UTC +08:00.

    # Get the station list and custom attributes to determine correct time zone
    station_list <- get_station_list(parameter_type,
                                     station_number,
                                     return_fields="custom_attributes")
    if (nrow(station_list) == 0) {
      stop(paste("Station number", station_number, "is invalid"))
    }

    jurisdiction <- stringr::str_split_fixed(station_list$DATA_OWNER_NAME, " -", n = 2)[1]

    # Time zones are selected where there is no DST
    if (jurisdiction %in% c("ACT", "ACTNSW", "NSW", "QLD", "TAS", "VIC")) {
      tz <- "Australia/Queensland" # AEST
    } else if (jurisdiction %in% c("SA", "NT")) {
      tz <- "Australia/Darwin" # ACST
    } else if (jurisdiction == "WA") {
      tz <- "Australia/Perth" # AWST
    } else {
      message("Jurisdiction not found, returning datetimes in UTC")
      tz <- "UTC"
    }
  } else {
    # Check if tz is valid
    if (!tz %in% OlsonNames()) {
      stop("Invalid tz argument. Check it is in OlsonNames().")
    }
    station_list <- get_station_list(parameter_type, station_number)
    if (nrow(station_list) == 0) {
      stop(paste("Station number", station_number, "is invalid"))
    }
  }

  # Check string date input is valid
  if (is.character(start_date)) {
    start_date <- lubridate::as_date(start_date, format = "%Y-%m-%d")
  }
  if (is.character(end_date)) {
    end_date <- lubridate::as_date(end_date, format = "%Y-%m-%d")
  }
  if (anyNA(c(start_date, end_date))) {
    stop("Dates must be formatted as %Y-%m-%d (e.g. 2000-01-01)")
  }
  # Ensure start is less than end
  if (start_date > end_date) {
    stop("start_date must be less than end_date")
  }

  # Coerce to datetime
  if (lubridate::is.Date(start_date)) {
    start_date <- lubridate::force_tz(lubridate::as_datetime(start_date), tz = tz)
    end_date <- lubridate::force_tz(lubridate::as_datetime(end_date), tz = tz)
  } else if (!lubridate::is.POSIXt(start_date)) {
    stop("Provide dates as a character, date, or datetime object")
  }

  # Fix the offset because BoM expects 00:00 style
  # despite 0000 conforming to ISO8601
  start_date <- sub(
    "(.*\\+)(\\d{2})(\\d{2})", "\\1\\2:\\3",
    lubridate::format_ISO8601(start_date, usetz = TRUE)
  )
  end_date <- sub(
    "(.*\\+)(\\d{2})(\\d{2})", "\\1\\2:\\3",
    lubridate::format_ISO8601(end_date, usetz = TRUE)
  )

  # Only accept one station at a time for now
  if (length(station_number) > 1) {
    stop("Only a single station can be requested at a time")
  }

  # If return_fields are missing return Timestamp, Value and Quality Code
  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_id <- get_timeseries_id(
    parameter_type,
    station_number,
    ts_name
  )

  timeseries_values <- get_timeseries_values(
    timeseries_id$ts_id[1],
    start_date,
    end_date,
    return_fields
  )

  # Only process data if it exists
  if (nrow(timeseries_values) > 0) {
    if ("Timestamp" %in% colnames(timeseries_values)) {
      # nolint start
      suppressMessages({
        timeseries_values$Timestamp <- lubridate::as_datetime(timeseries_values$Timestamp, tz = tz)
      })
      timeseries_values <- dplyr::mutate_at(timeseries_values,
        dplyr::vars(-"Timestamp"),
        utils::type.convert,
        as.is = TRUE
      )
      # nolint end
    } else {
      timeseries_values <- dplyr::mutate_all(timeseries_values,
        utils::type.convert,
        as.is = TRUE
      )
    }
  }
  return(timeseries_values)
}

#' @template timeseriesDocs
#' @examples
#' # Groundwater level as stored by the BoM
#' # PLUMB RD @ NARRABRI'
#' \dontrun{
#' get_as_stored(
#'   parameter_type = "Ground Water Level",
#'   station_number = "GW971623.3.3",
#'   start_date = "2020-03-01",
#'   end_date = "2020-03-01"
#' )
#' }
#' @export
get_as_stored <- function(parameter_type,
                          station_number,
                          start_date,
                          end_date,
                          tz,
                          return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
  }

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter_type,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    "DMQaQc.Merged.AsStored.1"
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @examples
#' # Hourly streamflow Cotter River at Gingera Gauge
#' \dontrun{
#' get_hourly(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31"
#' )
#' }
#' @export
get_hourly <- function(parameter_type,
                       station_number,
                       start_date,
                       end_date,
                       tz,
                       return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]

  if (!parameter_type %in% c(
    "Water Course Discharge",
    "Water Course Level",
    "Storage Level",
    "Storage Volume"
  )) {
    stop(
      paste("Hourly data is not available for parameter_type", parameter_type)
    )
  }

  if (missing(tz)) tz <- NULL
  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code", "Interpolation Type")
  }

  timeseries_values <- get_timeseries(
    parameter_type,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    "DMQaQc.Merged.HourlyMean.HR"
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @param var The daily variable of interest. Valid inputs are `mean`, `min`,
#' `max` for continuous series such as discharge and `total` for discrete
#' series such as rainfall and evaporation.
#' @param aggregation Whether the data is to be aggregated midnight to
#' midnight (`24HR`) or from 9am-9am (`09HR`). The default is `24HR`. `09HR`
#' is only available for mean discharge and total rainfall and evaporation.
#' @examples
#' # Download daily mean aggregated over the standard day
#' \dontrun{
#' get_daily(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "mean",
#'   aggregation = "24HR"
#' )
#' }
#'
#' # Download daily mean aggregated between 9am to 9am
#' \dontrun{
#' get_daily(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "mean",
#'   aggregation = "09HR"
#' )
#' }
#'
#' # Download the daily max over the standard day
#' \dontrun{
#' get_daily(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "max",
#'   aggregation = "24HR"
#' )
#' }
#'
#' @export
get_daily <- function(parameter_type,
                      station_number,
                      start_date,
                      end_date,
                      var,
                      aggregation,
                      tz,
                      return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
  }

  # Handle possible formats of var input
  if (missing(var)) {
    if (parameter_type %in% parameters("discrete")) {
      var <- "Total"
    } else {
      var <- "Mean"
    }
  } else {
    var <- stringr::str_to_title(var)
  }
  if (missing(aggregation)) {
    aggregation <- "24HR"
  } else {
    aggregation <- toupper(aggregation)
  }

  ts_name <- paste0("DMQaQc.Merged.Daily", var, ".", aggregation)

  if (parameter_type %in% parameters("continuous")) {
    valid_daily_ts <- c(
      "DMQaQc.Merged.DailyMean.24HR",
      "DMQaQc.Merged.DailyMax.24HR",
      "DMQaQc.Merged.DailyMin.24HR"
    )
    if (parameter_type == "Water Course Discharge") {
      valid_daily_ts <- c(
        valid_daily_ts,
        "DMQaQc.Merged.DailyMean.09HR"
      )
    }
  }

  if (parameter_type %in% parameters("discrete")) {
    valid_daily_ts <- c(
      "DMQaQc.Merged.DailyTotal.09HR",
      "DMQaQc.Merged.DailyTotal.24HR"
    )
  }

  if (!ts_name %in% valid_daily_ts) {
    stop("Invalid combination of parameter_type, var and aggregation")
  }

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter_type,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    ts_name
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @examples
#' # Monthly average dry air temperature at Corin Dam
#' \dontrun{
#' get_monthly(
#'   parameter_type = "Dry Air Temperature",
#'   station_number = "570947",
#'   start_date = "2016-01-01",
#'   end_date = "2016-06-01"
#' )
#' }
#' @export
get_monthly <- function(parameter_type,
                        station_number,
                        start_date,
                        end_date,
                        tz,
                        return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
  }

  if (parameter_type %in% parameters("continuous")) {
    ts_name <- "DMQaQc.Merged.MonthlyMean.CalMonth"
  }

  if (parameter_type %in% parameters("discrete")) {
    ts_name <- c("DMQaQc.Merged.MonthlyTotal.CalMonth")
  }

  if (!exists("ts_name")) stop("Invalid parameter_type")

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter_type,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    ts_name
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @param start_date Start date (formatted as YYYY-MM-DD) or just the
#' year (YYYY)
#' @param end_date End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @examples
#' # Download annual rainfall for Cotter Hut
#' \dontrun{
#' get_yearly(
#'   parameter_type = "Rainfall",
#'   station_number = "570946",
#'   start_date = 2016,
#'   end_date = 2020
#' )
#' }
#'
#' @export
get_yearly <- function(parameter_type,
                       station_number,
                       start_date,
                       end_date,
                       tz,
                       return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
  }

  start_date <- paste0(stringr::str_sub(start_date, 1, 4), "-01-01")
  end_date <- paste0(stringr::str_sub(end_date, 1, 4), "-12-31")

  if (parameter_type %in% parameters("continuous")) {
    ts_name <- "DMQaQc.Merged.YearlyMean.CalYear"
  }

  if (parameter_type %in% parameters("discrete")) {
    ts_name <- c("DMQaQc.Merged.YearlyTotal.CalYear")
  }

  if (!exists("ts_name")) stop("Invalid parameter_type")

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter_type,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    ts_name
  )

  return(timeseries_values)
}

#' @title Available water parameters
#' @aliases parameters()
#' @description
#' `parameters` returns a vector of parameters that can be retrieved from
#' Water Data Online.
#' @param pars Optional: if empty all available parameters will be returned.
#' Alternatively, a vector of the continuous or discrete parameters can be
#' requested.
#' @return
#' A vector of parameters.
#' @details
#' The units of the parameters are as follows:
#'
#' * Water Course Discharge (m3/s)
#' * Water Course Level (m)
#' * Electrical conductivity at 25C (µS/cm)
#' * Turbidity (NTU)
#' * pH
#' * Water Temperature (ºC)
#' * Storage Volume (ML)
#' * Storage Level (m)
#' * Ground Water Level (m)
#' * Rainfall (mm)
#' * Evaporation (mm)
#' * Dry Air Temperature (ºC)
#' * Relative Humidity (%)
#' * Wind Speed (m/s)
#' @md
#' @seealso
#' * \url{http://www.bom.gov.au/waterdata/}
#' * [BoM Guide to Sensor Observation Services (SOS2) for Water Data Online](http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf)
#' @author Alexander Buzacott
#' @examples
#' parameters()
#' parameters("continuous")
#' parameters("discrete")
#' @export
parameters <- function(pars) {
  continuous <- c(
    "Dry Air Temperature",
    "Relative Humidity",
    "Wind Speed",
    "Electrical Conductivity At 25C",
    "Turbidity",
    "pH",
    "Water Temperature",
    "Ground Water Level",
    "Water Course Level",
    "Water Course Discharge",
    "Storage Level",
    "Storage Volume"
  )
  discrete <- c(
    "Rainfall",
    "Evaporation"
  )

  if (missing(pars)) {
    return(c(discrete, continuous))
  } else {
    if (!tolower(pars) %in% c("continuous", "discrete")) {
      stop("Invalid parameter category entered")
    }
    return(get(pars))
  }
}
