## bomWater

This `R` package grabs data from the Australian Bureau of Meteorology Water Data online (http://bom.gov.au/waterdata/) via the WISKI API.

At the moment two convenience functions have been prepared to access daily and hourly mean streamflow for gauging stations. More will be implemented later on. If you know the WISKI API there other functions included that you can build your own request from for now.

### Installation


### Example

```
library(bomWater)

# Cotter River at Gingera
cotterRiver = getDailyFlow('410730', '2020-01-01', '2020-01-31')

cotterRiver
# A tibble: 31 x 3
   Date       `Q (m3/s)`    QC
   <date>          <dbl> <int>
 1 2020-01-01      0.013    10
 2 2020-01-02      0.013    10
 3 2020-01-03      0.011    10
 4 2020-01-04      0.009    10
 5 2020-01-05      0.01     10
 6 2020-01-06      0.015    10
 7 2020-01-07      0.023    10
 8 2020-01-08      0.019    10
 9 2020-01-09      0.017    10
10 2020-01-10      0.014    10
```
