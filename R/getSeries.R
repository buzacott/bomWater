#' @template timeseriesDocs
#' @details This function can be used if the others have missed a particular time series (e.g. the forecasting timeseries)
#' @param tsName The timeseries name (e.g. DMQaQc.Merged.DailyMean.24HR) that is desired.
#' @export
getTimeseries = function(parameterType, stationNumber, startDate, endDate, tz, returnFields, tsName) {

  # Check date input is valid
  if(anyNA(lubridate::as_date(c(startDate, endDate), '%Y-%m-%d', tz=''))) {
    stop('Dates must be formatted as %Y-%m-%d (e.g. 2000-01-01)')
  }

  # Check if tz is valid
  if(!is.null(tz)) {
    if(!tz %in% OlsonNames()) stop('Invalid tz argument. Check it is in OlsonNames().')
  }

  # Ensure start is less than end
  if(lubridate::as_date(startDate) > lubridate::as_date(endDate)) stop('startDate must be less than endDate')

  # Only accept one station at a time for now
  if(length(stationNumber)>1) stop('Only a single station can be requested at a time')

  stationList = getStationList(parameterType, stationNumber)
  if(nrow(stationList)==0) {
    stop(paste('Station number', stationNumber, 'is invalid'))
  }

  timeseriesID = getTimeseriesID(parameterType,
                                 stationNumber,
                                 tsName)

  timeSeriesValues = getTimeseriesValues(timeseriesID$ts_id,
                                         startDate,
                                         endDate,
                                         returnFields)

  # Get the tzone offset
  if(is.null(tz)) {
    tzOffset = stringr::str_sub(timeSeriesValues$Timestamp[1], -5, -4)
    tz = paste0('Etc/GMT-', tzOffset) # For some reason a negative offset is ahead of UTC
  }

  timeSeriesValues$Timestamp = lubridate::as_datetime(timeSeriesValues$Timestamp)
  timeSeriesValues$Value = as.numeric(timeSeriesValues$Value)
  timeSeriesValues$`Quality Code` = as.integer(timeSeriesValues$`Quality Code`)
  # timeSeriesValues = timeSeriesValues %>%
  #   dplyr::mutate(Timestamp = lubridate::as_datetime(.data$Timestamp),
  #                 Value = as.numeric(.data$Value),
  #                 `Quality Code` = as.integer(.data.$`Quality Code`))
  attributes(timeSeriesValues$Timestamp)$tzone = tz

  return(timeSeriesValues)
}

#' @template timeseriesDocs
#' @export
getAsStored = function(parameterType, stationNumber, startDate, endDate, tz, returnFields) {

  parameterType = parameters()[tolower(parameterType) == tolower(parameters())]
  if(length(parameterType)==0) {
    stop('Invalid parameter requested')
  }

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) {
    returnFields = 'Timestamp,Value,Quality Code'
  }

  timeSeriesValues = getTimeseries(parameterType,
                                   stationNumber,
                                   startDate,
                                   endDate,
                                   tz,
                                   returnFields,
                                   'DMQaQc.Merged.AsStored.1')

  return(timeSeriesValues)
}

#' @template timeseriesDocs
#' @export
getHourly  = function(parameterType, stationNumber, startDate, endDate, tz, returnFields) {

  parameterType = parameters()[tolower(parameterType) == tolower(parameters())]

  if(!parameterType %in% c('Water Course Discharge',
                           'Water Course Level',
                           'Storage Level',
                           'Storage Volume')) {
    stop(paste('Hourly data is not available for parameterType', parameterType))
  }

  if(missing(tz)) tz=NULL
  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'

  timeSeriesValues = getTimeseries(parameterType,
                                   stationNumber,
                                   startDate,
                                   endDate,
                                   tz,
                                   returnFields,
                                   'DMQaQc.Merged.HourlyMean.HR')

  return(timeSeriesValues)
}

#' @template timeseriesDocs
#' @param var The daily variable of interest. Valid inputs are `mean`, `min`, `max` for continuous series
#'  such as discharge and `total` for discrete series such as rainfall and evaporation.
#' @param aggregation Whether the data is to be aggregated midnight to midnight (`24HR`) or from 9am-9am (`09HR`).
#' The default is `24HR`. `09HR` is only available for mean discharge and total rainfall and evaporation.
#' @export
getDaily = function(parameterType, stationNumber, startDate, endDate, var, aggregation, tz, returnFields) {

  parameterType = parameters()[tolower(parameterType) == tolower(parameters())]
  if(length(parameterType)==0) {
    stop('Invalid parameter requested')
  }

  # Handle possible formats of var input
  if(missing(var)) {
    if(parameterType %in% parameters('discrete')) {
      var='Total'
    } else {
      var='Mean'
    }
  } else {
    var = stringr::str_to_title(var)
  }
  if(missing(aggregation)) {
    aggregation='24HR'
  } else {
    aggregation = toupper(aggregation)
  }

  tsName = paste0('DMQaQc.Merged.Daily', var, '.', aggregation)

  if(parameterType %in% parameters('continuous')) {
    validDailyTS = c('DMQaQc.Merged.DailyMean.24HR',
                     'DMQaQc.Merged.DailyMax.24HR',
                     'DMQaQc.Merged.DailyMin.24HR')
    if(parameterType == 'Water Course Discharge') {
      validDailyTS = c(validDailyTS,
                       'DMQaQc.Merged.DailyMean.09HR')
    }
  }

  if(parameterType %in% parameters('discrete')) {
    validDailyTS = c('DMQaQc.Merged.DailyTotal.09HR',
                     'DMQaQc.Merged.DailyTotal.24HR')
  }

  if(!tsName %in% validDailyTS) {
    stop('Invalid combination of parameterType, var and aggregation')
  }

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'


  timeSeriesValues = getTimeseries(parameterType,
                                   stationNumber,
                                   startDate,
                                   endDate,
                                   tz,
                                   returnFields,
                                   tsName)

  return(timeSeriesValues)
}

#' @template timeseriesDocs
#' @export
getMonthly = function(parameterType, stationNumber, startDate, endDate, tz, returnFields) {

  parameterType = parameters()[tolower(parameterType) == tolower(parameters())]
  if(length(parameterType)==0) {
    stop('Invalid parameter requested')
  }

  if(parameterType %in% parameters('continuous')) {
    tsName = 'DMQaQc.Merged.MonthlyMean.CalMonth'
  }

  if(parameterType %in% parameters('discrete')) {
    tsName = c('DMQaQc.Merged.MonthlyTotal.CalMonth')
  }

  if(!exists('tsName')) stop('Invalid parameterType')

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'

  timeSeriesValues = getTimeseries(parameterType,
                                   stationNumber,
                                   startDate,
                                   endDate,
                                   tz,
                                   returnFields,
                                   tsName)

  return(timeSeriesValues)
}

#' @template timeseriesDocs
#' @param startDate Start date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @param endDate End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @export
getYearly = function(parameterType, stationNumber, startDate, endDate, tz, returnFields) {

  parameterType = parameters()[tolower(parameterType) == tolower(parameters())]
  if(length(parameterType)==0) {
    stop('Invalid parameter requested')
  }

  startDate = paste0(stringr::str_sub(startDate, 1, 4), '-01-01')
  endDate   = paste0(stringr::str_sub(endDate, 1, 4), '-12-31')

  if(parameterType %in% parameters('continuous')) {
    tsName = 'DMQaQc.Merged.YearlyMean.CalYear'
  }

  if(parameterType %in% parameters('discrete')) {
    tsName = c('DMQaQc.Merged.YearlyTotal.CalYear')
  }

  if(!exists('tsName')) stop('Invalid parameterType')

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'

  timeSeriesValues = getTimeseries(parameterType,
                                   stationNumber,
                                   startDate,
                                   endDate,
                                   tz,
                                   returnFields,
                                   tsName)

  return(timeSeriesValues)
}

#' @title Available water parameters
#' @description
#' `parameters` returns a vector of parameters that can be retrieved from Water Data Online.
#' @param pars Optional: if empty all available parameters will be returned. Alternatively, a vector
#' of the continuous or discrete parameters can be requested.
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
#' \url{http://www.bom.gov.au/waterdata/}
#' \url{http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf}
#' @examples
#' parameters()
#' parameters('continuous')
#' parameters('discrete')
#' @export
parameters = function(pars) {
  continuous = c('Dry Air Temperature',
                 'Relative Humidity',
                 'Wind Speed',
                 'Electrical Conductivity At 25C',
                 'Turbidity',
                 'pH',
                 'Water Temperature',
                 'Groundwater Level',
                 'Water Course Level',
                 'Water Course Discharge',
                 'Storage Level',
                 'Storage Volume')
  discrete = c('Rainfall',
               'Evaporation')

  if(missing(pars)) {
    return(c(discrete, continuous))
  } else {
    x = tolower(pars)
    if(!pars %in% c('continuous', 'discrete')) stop('Invalid parameter category entered')
    return(get(pars))
  }
}
