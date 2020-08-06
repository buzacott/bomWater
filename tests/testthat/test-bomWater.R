test_that("I can make requests to BoM", {
  params <- list(
    "request" = "getStationList",
    "parameter_type_name" = "Water Course Discharge",
    "station_no" = "410730",
    "returnfields" = paste(c(
      "station_name",
      "station_no",
      "station_id",
      "station_latitude",
      "station_longitude"
    ), collapse = ",")
  )
  r <- make_bom_request(params)
  expect_equal(class(r)[1], "tbl_df")
  expect_equal(r$station_id, "13360")
  expect_equal(ncol(r), 5)
  expect_equal(nrow(r), 1)
})

test_that("I can get a station list", {
  r <- get_station_list("Rainfall", "570946")
  expect_equal(nrow(r), 1)
  expect_equal(r$station_name, "Cotter Hut")
  expect_equal(r$station_no, "570946")
  expect_equal(r$station_id, "13643")
  expect_equal(r$station_latitude, "-35.64947222")
  expect_equal(r$station_longitude, "148.83144444")

  r <- get_station_list("Rainfall", c("570946", "410730"))
  expect_equal(nrow(r), 2)
  expect_equal(r$station_name[2], "Cotter R. at Gingera")
})

test_that("I can get a timeseries ID", {
  r <- get_timeseries_id(
    "Water Course Discharge",
    "410730",
    "DMQaQc.Merged.DailyMean.24HR"
  )
  expect_equal(ncol(r), 7)
  expect_equal(nrow(r), 1)
  expect_equal(r$ts_id, "1573010")
})

test_that("I can get timeseries values", {
  # Berthong annual rainfall
  r <- get_timeseries_values(148131010,
    "2016-01-01",
    "2016-12-31",
    return_fields = c(
      "Timestamp",
      "Value",
      "Quality Code",
      "Interpolation Type"
    )
  )
  expect_equal(r$Value, "879")
})

test_that("I get an error", {
  expect_error(
    get_timeseries_id(
      "Water Course Discharge",
      "570946",
      "DMQaQc.Merged.DailyMean.24HR"
    )
  )
})

test_that("get timeseries puts it all together", {
  r <- get_timeseries(
    "Water Course Discharge",
    "410730",
    "2020-01-01",
    "2020-01-07",
    NULL,
    "Timestamp,Value,Quality Code",
    "DMQaQc.Merged.DailyMean.24HR"
  )
  expect_equal(ncol(r), 3)
  expect_equal(nrow(r), 7)
  expect_equal(class(r$Timestamp)[1], "POSIXct")
  expect_true(is.numeric(r$Value))
  expect_true(is.integer(r$`Quality Code`))
})
