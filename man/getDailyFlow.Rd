\name{getDailyFlow}
\alias{getDailyFlow}
\title{Download daily streamflow data}
\usage{
getDailyFlow(stationNumber, startDate, endDate, tz=NULL)
}
\arguments{
  \item{stationNumber}{The AWRC Station number. A full list can be found at http://www.bom.gov.au/waterdata/ on the table tab.}
  \item{startDate}{The start date for the timeseries to be extracted. Input can either be a date or string format (e.g. \code{"2020-01-01"}).}

  \item{endDate}{The end date for the timeseries to be extracted. Input can either be a date or string format (e.g. \code{'2020-01-31'}).}
  \item{var}{The timeseries variable to be extracted. Valid options are \code{'mean'}, \code{'max'} and \code{'min'}.}
  \item{aggregation}{Whether the data is to be aggregated from midnight to midnight (\code{'24HR'}) or 9am to 9am (\code{'09HR'}). Only available for the mean variable.}
  \item{tz}{The desired time zone for the output timeseries. Input must be an Olson Name (see \code{OlsonNames()}). By default the the timeseries is returned in an offset timezone (e.g. \code{'Etc/GMT-10'} for NSW) as the timeseries do not observe DST.}
}
\value{
The function returns a \code{tibble} with the columns \code{Date}, \code{`Q (m3/s)`} streamflow in cubic metres per second and \code{QC} quality code.
}
\description{
\code{getDailyFlow} downloads daily streamflow data from the Australian Bureau of Meteorology via the Water Data Online web services. The date, streamflow in cubic metres per second and quality code are extracted. The function accesses the prepared SOS2 flow series (DMQaQc.Merged.Daily) and the options are the daily mean, max and min aggregatated over the standard day, or the mean aggregated from 9am to 9am.
}
\author{
Alexander Buzacott
}
\examples{
# Download daily flow data from Cotter River at Gingera in the ACT
cotterRiver = getDailyFlow('410730', '2020-01-01', '2020-01-31')

# With a custom set time zone and extracting the daily max
# Queensland is chosen as they do not observe daylight savings time
cotterRiver = getDailyFlow('410730', '2020-01-01', '2020-01-31', var='max', tz='Australia/Queensland')

# The daily mean from 9am to 9am
cotterRiver = getDailyFlow('410730', '2020-01-01', '2020-01-31', var='mean', aggregation='09HR')
}
