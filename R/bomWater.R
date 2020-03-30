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
    tbl = tibble::as_tibble(json, .name_repair=~json[1,]) %>%
      dplyr::slice(-1)
  } else if(params$request == 'getTimeseriesValues') {
    columnNames = unlist(stringr::str_split(json$columns, ','))
    if(length(json$data[[1]])==0) {
      tbl = tibble::tibble(Timestamp=character(), Value=character(), `Quality Code`=character())
    } else {
      tbl = tibble::as_tibble(json$data[[1]], .name_repair=~columnNames)
    }
  }
  return(tbl)
}

getStationList = function(parameterType, stationNumber, returnFields) {

  params = list('request' = 'getStationList')

  if(!missing(stationNumber)) {
    # Support multiple stations
    stationNumber = paste(stationNumber, collapse=',')
    params[['station_no']] = stationNumber
  }

  # Set default to return all Water Course Discharge stations
  if(missing(parameterType)) {
    parameterType = 'Water Course Discharge'
  }
  params[['parameterType_name']] = parameterType

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

getTimeseriesID = function(stationNumber, tsName, parameterType) {

  params = list('request'            = 'getTimeseriesList',
                'parametertype_name' = parameterType,
                'ts_name'            = tsName,
                'station_no'         = stationNumber)

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}

getTimeseriesValues = function(tsID, startDate, endDate, returnFields=NULL) {

  params = list('request' = 'getTimeseriesValues',
                'ts_id'   = tsID,
                'from'    = startDate,
                'to'      = endDate)

  if(!is.null(returnFields)) {
    params[['returnfields']] = paste(returnFields, collapse=',')
  }

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}
