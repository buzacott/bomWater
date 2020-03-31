#' @template timeseriesDocs
#' @details This function can be used if the others have missed a particular time series (e.g. the forecasting timeseries)
#' @param tsName The timeseries name (e.g. DMQaQc.Merged.DailyMean.24HR) that is desired.
#' @export
getTimeSeries = function(parameterType, stationNumber, startDate, endDate, tz, returnFields, tsName) {
  # Inputs
  # Station number: character/integer
  # Start date: character/date formatted to %Y-%m-%d
  # End date: character/date formatted to %Y-%m-%d
  # parameterType: e.g. Water Course Discharge
  # tsName: timeseries name
  # tz: Olson name string that the user wants to return the timezone to.
  #     Default is to return an offset timezone as the data do not
  #     observe DST.
  # Returns
  # A tibble

  # Check date input is valid
  if(anyNA(lubridate::as_date(c(startDate, endDate), '%Y-%m-%d', tz=''))) {
    stop('Dates must be formatted as %Y-%m-%d (e.g. 2000-01-01)')
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

  timeSeriesValues = timeSeriesValues %>%
    dplyr::mutate(Timestamp = lubridate::as_datetime(Timestamp),
                  Value = as.numeric(Value),
                  `Quality Code` = as.integer(`Quality Code`))
  attributes(timeSeriesValues$Timestamp)$tzone = tz

  return(timeSeriesValues)
}

#' @template timeseriesDocs
#' @export
getAsStored = function(parameterType, stationNumber, startDate, endDate, tz, returnFields) {

  parameterType = stringr::str_to_title(parameterType)

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) {
    returnFields = 'Timestamp,Value,Quality Code'
  }

  timeSeriesValues = getTimeSeries(parameterType,
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

  parameterType = stringr::str_to_title(parameterType)

  if(!parameterType %in% c('Water Course Discharge',
                           'Water Course Level',
                           'Storage Level',
                           'Storage Volume')) {
    stop(paste('Hourly data is not available for parameterType', parameterType))
  }

  if(missing(tz)) tz=NULL
  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'

  timeSeriesValues = getTimeSeries(parameterType,
                                   stationNumber,
                                   startDate,
                                   endDate,
                                   tz,
                                   returnFields,
                                   'DMQaQc.Merged.HourlyMean.HR')

  return(timeSeriesValues)
}

#' @template timeseriesDocs
#' @param var The daily variable of interest. Valid inputs are \code{mean}, \code{min}, \code{max} for continuous series
#'  such as discharge and \code{total} for discrete series such as rainfall and evaporation.
#' @param aggregation Whether the data is to be aggregated midnight to midnight (\code{24HR}) or from 9am-9am (\code{09HR}).
#' Default is \code{24HR} and \code{09HR} is only available for mean discharge and total rainfall and evaporation.
#' @export
getDaily = function(parameterType, stationNumber, startDate, endDate, var, aggregation, tz, returnFields) {

  parameterType = stringr::str_to_title(parameterType)

  # Handle possible formats of var input
  if(missing(var)) {
    if(parameterType %in% c('Rainfall', 'Evaporation')) {
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

  if(parameterType %in% c('Dry Air Temperature',
                          'Relative Humidity',
                          'Wind Speed',
                          'Electrical conductivity at 25C',
                          'Turbidity',
                          'Water pH',
                          'Water Temperature',
                          'Ground Water Level',
                          'Water Course Level',
                          'Water Course Discharge',
                          'Storage Level',
                          'Storage Volume')) {
    validDailyTS = c('DMQaQc.Merged.DailyMean.09HR',
                     'DMQaQc.Merged.DailyMean.24HR',
                     'DMQaQc.Merged.DailyMax.24HR',
                     'DMQaQc.Merged.DailyMin.24HR')
  }

  if(parameterType %in% c('Rainfall',
                          'Evaporation')) {
    validDailyTS = c('DMQaQc.Merged.DailyTotal.09HR',
                     'DMQaQc.Merged.DailyTotal.24HR')
  }

  if(!tsName %in% validDailyTS) {
    stop('Invalid combination of parameterType, var and aggregation')
  }

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'


  timeSeriesValues = getTimeSeries(parameterType,
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

  parameterType = stringr::str_to_title(parameterType)

  if(parameterType %in% c('Dry Air Temperature',
                          'Relative Humidity',
                          'Wind Speed',
                          'Electrical Conductivity At 25C',
                          'Turbidity',
                          'Water Ph',
                          'Water Temperature',
                          'Ground Water Level',
                          'Water Course Level',
                          'Water Course Discharge',
                          'Storage Level',
                          'Storage Volume')) {
    tsName = 'DMQaQc.Merged.MonthlyMean.CalMonth'
  }

  if(parameterType %in% c('Rainfall', 'Evaporation')) {
    tsName = c('DMQaQc.Merged.MonthlyTotal.CalMonth')
  }

  if(!exists('tsName')) stop('Invalid parameterType')

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'

  timeSeriesValues = getTimeSeries(parameterType,
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
getYearly = function(parameterType, stationNumber, startYear, endYear, tz, returnFields) {

  parameterType = stringr::str_to_title(parameterType)

  startDate = paste0(stringr::str_sub(startYear, 1, 4), '-01-01')
  endDate   = paste0(stringr::str_sub(endYear, 1, 4), '-12-31')

  if(parameterType %in% c('Dry Air Temperature',
                          'Relative Humidity',
                          'Wind Speed',
                          'Electrical Conductivity At 25C',
                          'Turbidity',
                          'Water Ph',
                          'Water Temperature',
                          'Groundwater Level',
                          'Water Course Level',
                          'Water Course Discharge',
                          'Storage Level',
                          'Storage Volume')) {
    tsName = 'DMQaQc.Merged.YearlyMean.CalYear'
  }

  if(parameterType %in% c('Rainfall', 'Evaporation')) {
    tsName = c('DMQaQc.Merged.YearlyTotal.CalYear')
  }

  if(!exists('tsName')) stop('Invalid parameterType')

  if(missing(tz)) tz=NULL

  if(missing(returnFields)) returnFields = 'Timestamp,Value,Quality Code'

  timeSeriesValues = getTimeSeries(parameterType,
                                   stationNumber,
                                   startDate,
                                   endDate,
                                   tz,
                                   returnFields,
                                   tsName)

  return(timeSeriesValues)
}





