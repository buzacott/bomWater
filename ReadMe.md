## bomWater

This `R` package grabs data from the Australian Bureau of Meteorology Water Data online (http://bom.gov.au/waterdata/) via the WISKI API.

Several functions have been written to retrieve the quality checked timeseries. These are:

- `getAsStored`
- `getHourly`
- `getDaily`
- `getMonthly`
- `getYearly`

The following Water Data Online variables can be accessed using these functions:

| Parameter                      | Units  |
| ------------------------------ | ------ |
| Water Course Discharge         | m3/s   |
| Water Course Level             | m      |
| Electrical conductivity at 25C | µS/cm  |
| Turbidity                      | NTU    |
| pH                             | pH     |
| Water Temperature              | ºC     |
| Storage Volume                 | ML     |
| Storage Level                  | m      |
| Ground Water Level             | m      |
| Rainfall                       | mm     |
| Evaporation                    | mm     |
| Dry Air Temperature            | ºC     |
| Relative Humidity              | %      |
| Wind Speed                     | m/s    |

Make sure formatting of the parameter types is as in the table above when making requests. The function `parameters()` can be used to retrieve this from within R.

Station information can be queried using `getStationList`, as in the example below.

The SOS2 manual can be consulted for the units of the different timeseries, as well as the meanings of the different quality codes ([BoM WISKI manual](http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf)).

### Installation

You can install `bomWater` directly from this repo using `devtools`:

```r
devtools::install_github('https://github.com/a-buz/bomWater')
```

### Examples

```r
library(bomWater)

# Daily streamflow from Cotter River at Gingera (in m3/s)
cotterRiver = getDaily(parameterType = 'Water Course Discharge',
                       stationNumber = '410730',
                       startDate     = '2020-01-01',
                       endDate       = '2020-01-31')

cotterRiver
# A tibble: 31 x 3
   Timestamp           Value `Quality Code`
   <dttm>              <dbl>          <int>
 1 2020-01-01 00:00:00 0.013             10
 2 2020-01-02 00:00:00 0.013             10
 3 2020-01-03 00:00:00 0.011             10
 4 2020-01-04 00:00:00 0.009             10
 5 2020-01-05 00:00:00 0.01              10
 6 2020-01-06 00:00:00 0.015             10
 7 2020-01-07 00:00:00 0.023             10
 8 2020-01-08 00:00:00 0.019             10
 9 2020-01-09 00:00:00 0.017             10
10 2020-01-10 00:00:00 0.014             10
# … with 21 more rows

# Monthly total rainfall in mm at Cotter Hut
cotterHut = getMonthly(parameterType = 'Rainfall',
                       stationNumber = '570946',
                       startDate     = '2019-01-01',
                       endDate       = '2019-12-31')
cotterHut
# A tibble: 12 x 3
   Timestamp           Value `Quality Code`
   <dttm>              <dbl>          <int>
 1 2019-01-01 00:00:00  57.2             10
 2 2019-02-01 00:00:00  23.2             10
 3 2019-03-01 00:00:00  89.2             10
 4 2019-04-01 00:00:00  11.2             10
 5 2019-05-01 00:00:00 111.              10
 6 2019-06-01 00:00:00  44.8             10
 7 2019-07-01 00:00:00  38               10
 8 2019-08-01 00:00:00  50.8             10
 9 2019-09-01 00:00:00  50.8             10
10 2019-10-01 00:00:00  53.6             10
11 2019-11-01 00:00:00  41.2             10
12 2019-12-01 00:00:00   8               10

# Get a list of groundwater bore data available from water data online
getStationList(parameterType = 'Ground Water Level')
# A tibble: 4,433 x 5
   station_name station_no station_id station_latitude station_longitude
   <chr>        <chr>      <chr>      <chr>            <chr>            
 1 01/DD01 D    60930131   387998     -33.21743524     117.80645139     
 2 01/DD01 OB   60930132   388003     -33.217408183    117.806451142    
 3 01/DD04 S    60930135   388008     -33.217376694    117.805742621    
 4 02/DD25 OB   60930141   388013     -33.218110097    117.803914363    
 5 02/DD26 OB   60930142   388018     -33.218521858    117.80440102     
 6 02/DD27 OB   60930143   388023     -33.218898026    117.80481224     
 7 02/DD28 OB   60930144   388028     -33.219219939    117.805244428    
 8 02/DD29 OB   60930145   388033     -33.219443056    117.805611327    
 9 02/DD30 OB   60930146   388038     -33.219785507    117.80565739     
10 02/DD31 OB   60930147   388043     -33.220099232    117.805960737    
# … with 4,423 more rows
```
