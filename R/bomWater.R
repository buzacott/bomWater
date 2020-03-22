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
    tbl = tibble::as_tibble(json, .name_repair=~json[1,]) %>%
      dplyr::slice(-1)
  } else if(params$request == 'getTimeseriesValues') {
    columnNames = unlist(stringr::str_split(json$columns, ','))
    tbl = tibble::as_tibble(json$data[[1]], .name_repair=~columnNames)
  }
  return(tbl)
}

getStationList = function(stationNumber) {

  stationNumber = paste(stationNumber, collapse=',')

  params = list('request' = 'getStationList',
                'station_no' = stationNumber)

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}

getTimeseriesID = function(stationNumber, tsName, parameterType) {

  params = list('request' = 'getTimeseriesList',
                'parametertype_name' = parameterType,
                'ts_name' = tsName,
                'station_no' = stationNumber)

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}

getTimeseriesValues = function(tsID, startDate, endDate, returnFields=NULL) {

  params = list('request' = 'getTimeseriesValues',
                'ts_id' = tsID,
                'from' = startDate,
                'to'   = endDate)

  if(!is.null(returnFields)) {
    params[['returnfields']] = paste(returnFields, collapse=',')
  }

  getBOMRequest = makeBOMRequest(params)

  return(getBOMRequest)
}

getFlow = function(tsName, stationNumber, startDate, endDate) {
  # Inputs
  # tsName: timeseries name
  # Station number: character/integer
  # Start date: character/date formatted to %Y-%m-%d
  # End date: character/date formatted to %Y-%m-%d
  # tz: Olson name string that the user wants to return the timezone to.
  #     Default is to return an offset timezone as the data do not
  #     observe DST.
  # Returns
  # A tibble

  # Check date input is valid
  if(anyNA(lubridate::as_date(c(startDate, endDate), '%Y-%m-%d', tz=''))) {
    stop('Dates must be formatted as %Y-%m-%d (e.g. 2000-01-01)')
  }

  stationList = getStationList(stationNumber)
  if(nrow(stationList)==0) {
    stop(paste('Station number', stationNumber, 'is invalid'))
  }

  timeseriesID = getTimeseriesID(stationNumber,
                                 tsName,
                                 'Water Course Discharge')

  timeSeriesValues = getTimeseriesValues(timeseriesID$ts_id,
                                         startDate,
                                         endDate,
                                         returnFields = c('Timestamp,Value,Quality Code'))

  return(timeSeriesValues)
}

getDailyFlow = function(stationNumber, startDate, endDate, var='mean', aggregation='24HR', tz=NULL) {
  # Inputs
  # Station number: character/integer
  # Start date: character/date formatted to %Y-%m-%d
  # End date: character/date formatted to %Y-%m-%d
  # Aggregation: whether the data is to be aggregated from midnight to midnight or 9am to 9am.
  # tz: Olson string that the user wants to return the timezone to.
  #     Default is to return an offset timezone as the data do not
  #     observe DST.
  # Returns
  # A tibble with date, discharge (Q) in m3/s and the quality code (QC)

  # Name structure of tsNames
  # DMQaQc.Merged.var.aggregation

  # Valid tsNames
  validDailyTS = c('DMQaQc.Merged.DailyMean.09HR',
                   'DMQaQc.Merged.DailyMean.24HR',
                   'DMQaQc.Merged.DailyMax.24HR',
                   'DMQaQc.Merged.DailyMin.24HR')

  # Handle possible formats of var input
  var = stringr::str_to_title(var)
  aggregation = toupper(aggregation)
  tsName = paste0('DMQaQc.Merged.Daily', var, '.', aggregation)

  if(!tsName %in% validDailyTS) {
    if(!aggregation %in% c('24HR', '09HR')) {
      stop('Invalid aggregation option provided.')
    }
    if(aggregation=='09HR' & var!='Mean') {
      stop('Only the mean is available to be aggregated from 9am to 9am')
    }
    stop('Invalid var input provided. Must be mean, max or min.')
  }

  timeSeriesValues = getFlow(tsName,
                             stationNumber,
                             startDate,
                             endDate)

  # Get the tzone offset
  if(is.null(tz)) {
    tzOffset = stringr::str_sub(timeSeriesValues$Timestamp[1], -5, -4)
    tz = paste0('Etc/GMT-', tzOffset) # For some reason a negative offset is ahead of UTC
  }

  dailyFlow = timeSeriesValues %>%
    dplyr::rename(Date = Timestamp,
                  `Q (m3/s)` = Value,
                  QC = `Quality Code`) %>%
    dplyr::mutate(Date = lubridate::as_date(lubridate::as_datetime(Date), tz=tz),
                  `Q (m3/s)` = as.numeric(`Q (m3/s)`),
                  QC = as.integer(QC))

  return(dailyFlow)
}

getHourlyFlow = function(stationNumber, startDate, endDate, tz=NULL) {
  # Inputs
  # Station number: character/integer
  # Start date: character/date formatted to %Y-%m-%d
  # End date: character/date formatted to %Y-%m-%d
  # tz: Olson string that the user wants to return the timezone to.
  #     Default is to return an offset timezone as the data do not
  #     observe DST.
  # Returns
  # A tibble with datetime, discharge (Q) in m3/s and the quality code (QC)

  timeSeriesValues = getFlow('DMQaQc.Merged.HourlyMean.HR',
                             stationNumber,
                             startDate,
                             endDate)
  # Get the tzone offset
  if(is.null(tz)) {
    tzOffset = stringr::str_sub(timeSeriesValues$Timestamp[1], -5, -4)
    tz = paste0('Etc/GMT-', tzOffset)
  }

  hourlyFlow = timeSeriesValues %>%
    dplyr::rename(Datetime = Timestamp,
           `Q (m3/s)` = Value,
           QC = `Quality Code`) %>%
    dplyr::mutate(Datetime = lubridate::as_datetime(Datetime),
                  `Q (m3/s)` = as.numeric(`Q (m3/s)`),
                  QC = as.integer(QC))
  attributes(hourlyFlow$Datetime)$tzone = tz

  return(hourlyFlow)
}



