\name{getFlow}
\alias{getFlow}
\title{Builds flow series request}
\description{
Function that builds the request parameters for downloading the daily and hourly flow series. If you know another SOS2 Water Course Discharge timeseries name then you can make your own request.
}
\usage{
getFlow(tsName, stationNumber, startDate, endDate, tz=NULL)
}
\arguments{
  \item{tsName}{
The SOS2 time series name.
}
  \item{stationNumber}{
The AWRC station number.
}
  \item{startDate}{
The start date for the timeseries to be extracted. Input can either be a date or string format (e.g. \code{"2020-01-01"}).
}
  \item{endDate}{
The end date for the timeseries to be extracted. Input can either be a date or string format (e.g. \code{"2020-01-31"}).
}
}

\value{
A tibble with columns Timestamp, Value and Quality Code.
}
\author{
Alexander Buzacott
}
\seealso{
http://www.bom.gov.au/waterdata/wiski-web-public/Guide to Sensor Observation Services (SOS2) for Water Data Online v1.0.1.pdf
}

\examples{
getFlow('DMQaQc.Merged.DailyMean.24HR', '410730', '2020-01-01', '2020-01-31')
}
