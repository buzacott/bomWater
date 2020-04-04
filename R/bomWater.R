#' @title Query the BoM WISKI API
#' @description
#' This function queries the Bureau of Meteorology Water Data KISTERS API.
#' A parameter list is passed to make request and the JSON return is parsed depending
#' on what is requested. This function can be used if you want to build your own JSON queries.
#' @param params A named list of parameters.
#' @return
#' A tibble is returned with the columns depending on the request. For \code{getTimeSeries}
#' requests, a tibble with zero rows is returned if the request if there is no data
#' available for that query.
#' @author Alexander Buzacott
#' @examples
#' # Getting the stations for Water Course Discharge
#' \dontrun{
#' params = list('request' = 'getStationList',
#'               'parameterType_name' = 'Water Course Discharge',
#'               'returnfields' = paste(c('station_name',
#'                                        'station_no',
#'                                        'station_id',
#'                                        'station_latitude',
#'                                        'station_longitude'),
#'                                      collapse=','))
#' makeBOMRequest(params)
#' }
makeBOMRequest = function(params) {

  bomURL = 'http://www.bom.gov.au/waterdata/services'

  baseParams = list('service' = 'kisters',
                    'type'    = 'QueryServices',
                    'format'  = 'json')

  r = httr::GET(bomURL, query=c(baseParams, params))
  json = jsonlite::fromJSON(httr::content(r, 'text'))

  if(params$request %in% c('getParameterList',
                           'getParameterTypeList',
                           'getSiteList',
                           'getStationList',
                           'getTimeseriesList')) {

    if(json[1]=='No matches.') stop('No parameterType and stationNumber match found')

    colnames(json) = json[1,]
    tbl = tibble::as_tibble(json) %>%
      dplyr::slice(-1)

  } else if(params$request == 'getTimeseriesValues') {

    columnNames = unlist(stringr::str_split(json$columns, ','))
    if(length(json$data[[1]])==0) {
      tbl = tibble::tibble(Timestamp=character(), Value=character(), `Quality Code`=character())
    } else {
      colnames(json$data[[1]]) = columnNames
      tbl = tibble::as_tibble(json$data[[1]])
    }

  }
  return(tbl)
}

#' @title Retrieve water observation stations
#'
#' @description
#' `getStationList` queries Water Data Online and returns station details. Queries can be
#' input with the desired `parameterType` to find all the stations on record. If you already
#' have a vector of station numbers, you can pass the vector to `stationNumber` and return the
#' details of those stations. `returnFields` can be customised to return various data about
#' the stations.
#'
#' @param parameterType The parameter for the station request (e.g. Water Course Discharge,
#' Storage Level)
#' @param stationNumber Optional: a single or multiple vector of AWRC station numbers.
#' @param returnFields  Station details to be returned. By default the columns returned are
#' station name, number, ID, latitude and longitude.
#'  Can be customised with a vector of parameters.
#' @return
#' With the default return fields, a tibble with columns station_name, station_no, station_id,
#' station_latitude, station_longitude.
#'
#' @author Alexander Buzacott
#'
#' @examples
#' # Get all Water Course Discharge Stations
#' getStationList()
#' # Just the details for Cotter River at Gingera
#' getStationList(stationNumber='410730')
#' # Rainfall stations
#' getStationList(parameterType = 'Rainfall')
#'
#' @export
getStationList = function(parameterType, stationNumber, returnFields) {

  params = list('request' = 'getStationList')

  # Set default to return all Water Course Discharge stations
  if(missing(parameterType)) {
    parameterType = 'Water Course Discharge'
  }
  params[['parameterType_name']] = parameterType

  if(!missing(stationNumber)) {
    # Support multiple stations
    stationNumber = paste(stationNumber, collapse=',')
    params[['station_no']] = stationNumber
  }

  # Set the default return fields
  if(missing(returnFields)) {
    params[['returnfields']] = paste(c('station_name',
                                       'station_no',
                                       'station_id',
                                       'station_latitude',
                                       'station_longitude'),
                                     collapse=',')
  }

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}

#' @title Retrieve the timeseries ID
#' @description
#' `getTimeseriesID` retrieves the timeseries ID that can be used to obtain values for a parameter type,
#' station and timeseries combination.
#' @param parameterType The parameter of interest (e.g. Water Course Discharge).
#' @param stationNumber The AWRC station number.
#' @param tsName The BoM time series name (e.g. DMQaQc.Merged.DailyMean.24HR).
#' @return
#' Returns a tibble with columns station_name, station_no, station_id, ts_id, ts_name,
#' parametertype_id, parametertype_name.
#' @author Alexander Buzacott
#' @examples
#' \dontrun{
#' getTimeseriesID(parameterType = 'Water Course Discharge',
#'                 stationNumber = '410730',
#'                 tsName = 'DMQaQc.Merged.DailyMean.24HR')
#' }
getTimeseriesID = function(parameterType, stationNumber, tsName) {

  params = list('request'            = 'getTimeseriesList',
                'parametertype_name' = parameterType,
                'ts_name'            = tsName,
                'station_no'         = stationNumber)

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}

#' @title Retrieve timeseries values
#' @description
#' `getTimeseriesValues` returns the timeseries values between a start and end date for given
#' timeseries ID.
#' @param tsID The timeseries ID for the values of interest. Can be found using the function
#' `getTimeseriesID`.
#' @param startDate The start date formatted as 'YYYY-MM-DD'.
#' @param endDate The end date formatted as 'YYYY-MM-DD'.
#' @param returnFields A vector of the variables that are to be returned.
#' @return
#' A tibble with columns with the requested returnFields. A zero row tibble is returned if no data
#' is returned  from the query. The columns of the tibble are returned as character classes and
#' have not been formatted to more appropriate correct classes (this happens in other functions).
#' @examples
#' \dontrun{
#' # Get the timeseries ID
#' timeseriesID = getTimeseriesID(parameterType = 'Water Course Discharge',
#'                                stationNumber = '410730',
#'                                tsName = 'DMQaQc.Merged.DailyMean.24HR')
#'
#' getTimeseriesValues(timeseriesID$ts_id,
#'                     startDate,
#'                     endDate,
#'                     returnFields = c('Timestamp',
#'                                      'Value',
#'                                      'Quality Code',
#'                                      'Interpolation Type'))
#' }
getTimeseriesValues = function(tsID, startDate, endDate, returnFields) {

  params = list('request'      = 'getTimeseriesValues',
                'ts_id'        = tsID,
                'from'         = startDate,
                'to'           = endDate,
                'returnfields' = paste(returnFields, collapse=','))

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}
