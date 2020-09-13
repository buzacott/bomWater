#' @title Query the BoM WISKI API
#' @description
#' This function queries the Bureau of Meteorology Water Data KISTERS API.
#' A parameter list is passed to make request and the JSON return is parsed
#' depending on what is requested. This function can be used if you want to
#' build your own JSON queries.
#' @param params A named list of parameters.
#' @return
#' A tibble is returned with the columns depending on the request. For
#' \code{get_timeseries} requests, a tibble with zero rows is returned
#' if there is no data available for that query.
#' @author Alexander Buzacott
make_bom_request <- function(params) {
  bom_url <- "http://www.bom.gov.au/waterdata/services"

  base_params <- list(
    "service" = "kisters",
    "type" = "QueryServices",
    "format" = "json"
  )

  r <- tryCatch(
    {
      r <- httr::GET(bom_url, query = c(base_params, params))
      httr::stop_for_status(r, task = "request water data form BoM")
      httr::warn_for_status(r, task = "request water data from BoM")
    },
    error = function(e) {
      message(strwrap(
        prefix = " ", initial = "",
        "Request for water data failed. Check your request and make sure
         http://www.bom.gov.au/waterdata/ is online"
      ))
      message("Error message:")
      message(e$message)
    },
    warning = function(w) {
      message("Request for water data raised a warning. Warning message:")
      message(w$message)
    }
  )
  json <- jsonlite::fromJSON(httr::content(r, "text"))

  if (params$request %in% c(
    "getParameterList",
    "getSiteList",
    "getStationList",
    "getTimeseriesList"
  )) {
    if (json[1] == "No matches.") {
      stop("No parameter type and station number match found")
    }
    colnames(json) <- json[1, ]
    tbl <- dplyr::slice(tibble::as_tibble(json), -1)
  } else if (params$request == "getTimeseriesValues") {
    column_names <- unlist(stringr::str_split(json$columns, ","))
    if (length(json$data[[1]]) == 0) {
      tbl <- tibble::tibble(
        Timestamp = character(),
        Value = character(),
        `Quality Code` = character()
      )
    } else {
      colnames(json$data[[1]]) <- column_names
      tbl <- tibble::as_tibble(json$data[[1]])
    }
  }
  return(tbl)
}

#' @title Retrieve water observation stations
#' @md
#' @description
#' `get_station_list` queries Water Data Online and returns station details.
#' Queries can be input with the desired `parameter_type` to find all the
#' stations on record. If you already have a vector of station numbers, you can
#' pass the vector to `station_number` and return the details of those
#' stations.
#' `return_fields` can be customised to return various data about the stations.
#'
#' @param parameter_type The parameter for the station request (e.g. Water
#' Course Discharge, Storage Level)
#' @param station_number Optional: a single or multiple vector of AWRC station
#' numbers.
#' @param return_fields  Station details to be returned. By default the columns
#' returned are station name, number, ID, latitude and longitude. Can be
#' customised with a vector of parameters.
#' @return
#' With the default return fields, a tibble with columns station_name,
#' station_no, station_id, station_latitude, station_longitude.
#'
#' @details
#' Possible return fields for `get_station_list()` are:
#'
#' * station_name
#' * station_longname
#' * station_no
#' * station_id
#' * station_latitude
#' * station_longitude
#' * station_carteasting
#' * station_cartnorthing
#' * stationparameter_name
#' * station_georefsystem
#' * catchment_no
#' * catchment_id
#' * catchment_name
#' * site_no
#' * site_id
#' * site_name
#' * parametertype_id
#' * parametertype_name
#' * object_type
#' * custom_attributes
#'
#' @author Alexander Buzacott
#'
#' @examples
#' # Get all Water Course Discharge Stations
#' \dontrun{
#' get_station_list()
#' }
#' # Just the details for Cotter River at Gingera
#' \dontrun{
#' get_station_list(station_number = "410730")
#' }
#' # Rainfall stations
#' \dontrun{
#' get_station_list(parameter_type = "Rainfall")
#' }
#' # Vector of return_fields
#' return_fields <- c(
#'   "station_name",
#'   "station_longname",
#'   "station_no",
#'   "station_id",
#'   "station_latitude",
#'   "station_longitude",
#'   "station_carteasting",
#'   "station_cartnorthing",
#'   "stationparameter_name",
#'   "station_georefsystem",
#'   "catchment_no",
#'   "catchment_id",
#'   "catchment_name",
#'   "site_no",
#'   "site_id",
#'   "site_name",
#'   "parametertype_id",
#'   "parametertype_name",
#'   "object_type",
#'   "custom_attributes"
#' )
#' # Get all attributes for one station
#' \dontrun{
#' get_station_list("Water Course Discharge", "410730", return_fields)
#' }
#' @export
get_station_list <- function(parameter_type, station_number, return_fields) {
  params <- list("request" = "getStationList")

  # Set default to return all Water Course Discharge stations
  if (missing(parameter_type)) {
    parameter_type <- "Water Course Discharge"
  }
  params[["parameterType_name"]] <- parameter_type

  if (!missing(station_number)) {
    # Support multiple stations
    station_number <- paste(station_number, collapse = ",")
    params[["station_no"]] <- station_number
  }

  # Set the default return fields
  if (missing(return_fields)) {
    params[["returnfields"]] <- paste(c(
      "station_name",
      "station_no",
      "station_id",
      "station_latitude",
      "station_longitude"
    ),
    collapse = ","
    )
  } else {
    params[["returnfields"]] <- paste(return_fields, collapse = ",")
  }

  get_bom_request <- make_bom_request(params)

  # Convert types
  station_list <- dplyr::mutate_all(
    get_bom_request,
    utils::type.convert,
    as.is = TRUE
  )

  return(station_list)
}

#' @title Retrieve the timeseries ID
#' @description
#' `get_timeseries_id` retrieves the timeseries ID that can be used to obtain
#' values for a parameter type, station and timeseries combination.
#' @param parameter_type The parameter of interest (e.g. Water Course
#' Discharge).
#' @param station_number The AWRC station number.
#' @param ts_name The BoM time series name (e.g. DMQaQc.Merged.DailyMean.24HR).
#' @return
#' Returns a tibble with columns station_name, station_no, station_id, ts_id,
#' ts_name, parametertype_id, parametertype_name.
#' @author Alexander Buzacott
get_timeseries_id <- function(parameter_type, station_number, ts_name) {
  params <- list(
    "request" = "getTimeseriesList",
    "parametertype_name" = parameter_type,
    "ts_name" = ts_name,
    "station_no" = station_number
  )

  get_bom_request <- make_bom_request(params)

  return(get_bom_request)
}

#' @title Retrieve timeseries values
#' @description
#' `get_timeseries_values` returns the timeseries values between a start and end
#' date for given timeseries ID.
#' @param ts_id The timeseries ID for the values of interest. Can be found using
#' the function `get_timeseries_id`.
#' @param start_date The start date formatted as 'YYYY-MM-DD'.
#' @param end_date The end date formatted as 'YYYY-MM-DD'.
#' @param return_fields A vector of the variables that are to be returned.
#' @return
#' A tibble with columns with the requested return_fields. A zero row tibble is
#' returned if no data is returned  from the query. The columns of the tibble
#' are returned as character classes and have not been formatted to more
#' appropriate correct classes (this happens in other functions).
#' @author Alexander Buzacott
get_timeseries_values <- function(ts_id, start_date, end_date, return_fields) {
  params <- list(
    "request" = "getTimeseriesValues",
    "ts_id" = ts_id,
    "from" = start_date,
    "to" = end_date,
    "returnfields" = paste(return_fields, collapse = ",")
  )

  get_bom_request <- make_bom_request(params)

  return(get_bom_request)
}

#' @title Retrieve available parameters for stations
#' @md
#' @description
#' `get_parameter_list` returns the parameters that can be retrieved at one or
#' more stations.
#' @param station_number A single or multiple vector of AWRC station
#' numbers.
#' @param return_fields (Optional) Station parameter details to be returned.
#' By default the return fields are: station_no, station_id, station_name,
#' parametertype_id, parametertype_name, parametertype_unitname
#' parametertype_shortunitname.
#' @details
#' The default return fields have been selected to generally return the most
#' useful fields while reducing duplication of metadata.
#' The full list of return fields:
#' * station_no',
#' * station_id
#' * station_name
#' * stationparameter_id
#' * stationparameter_no
#' * stationparameter_name
#' * stationparameter_longname
#' * site_id
#' * site_no
#' * site_name
#' * parametertype_id
#' * parametertype_name
#' * parametertype_longname
#' * parametertype_unitname
#' * parametertype_shortunitname
#' @return
#' A tibble with columns for each of the return fields.
#' @examples
#' # Return parameters for a single station
#' \dontrun{
#' get_parameter_list(station_number = "410730")
#' }
#' # Return available parameters for multiple stations
#' \dontrun{
#' get_parameter_list(station_number = c("410730", "570946"))
#' }
#' @export
get_parameter_list <- function(station_number, return_fields) {
  params <- list("request" = "getParameterList")

  if (!missing(station_number)) {
    # Support multiple stations
    station_number <- paste(station_number, collapse = ",")
    params[["station_no"]] <- station_number
  } else {
    stop("No station number provided")
  }

  # Set the default return fields
  if (missing(return_fields)) {
    params[["returnfields"]] <- paste(c(
      "station_no",
      "station_id",
      "station_name",
      "parametertype_id",
      "parametertype_name",
      "parametertype_unitname",
      "parametertype_shortunitname"
    ),
    collapse = ","
    )
  }

  get_bom_request <- make_bom_request(params)

  # Convert types
  parameter_list <- dplyr::mutate_all(
    get_bom_request,
    utils::type.convert,
    as.is = TRUE
  )

  return(parameter_list)
}
