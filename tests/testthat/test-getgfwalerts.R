library(terra)
library(httr)
library(jsonlite)
library(yaml)
library(testthat)

test_that("get.gfw.alerts works correctly", {
  config <- yaml::read_yaml("../../config.yml")
  username <- config$username
  pw <- config$password
  target.ext <- c(xmin = -11.51, xmax = -9.99,
                  ymin = 7.1, ymax = 8.15)
  auth.token <- request.token(username,pw)
  api.key <- create.api(auth.token)
  result <- get.gfw.alerts(api.key = api.key, geom.ext = target.ext, start = "2023-01-01", end = "2023-01-31")
  expect_true(inherits(result, "SpatVector"))
  
})