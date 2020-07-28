source("../find_pentad.R", chdir = TRUE)
source("../find_pentad_within.R", chdir = TRUE)
library(testthat)

test_that("Check 'find_pentad'", {

  coordinates <- data.frame(lat = 47.608013, lon = -12.335167)
  expect_equal(find_pentad(coordinates),'4760b1220')

  coordinates <- data.frame(lat = -2.21, lon = 37.1)
  expect_equal(find_pentad(coordinates),'0220_3700')

  coordinates <- data.frame(lat = 47.608013, lon = "-12.335167")
  expect_equal(find_pentad(coordinates),'4760b1220')

  coordinates <- data.frame(lat = c(10,-10,10,-10), lon = c(-10,10,10,-10))
  expect_equal(find_pentad(coordinates), c("1000b1000","1000_1000","1000c1000","1000a1000"))


})

test_that("Check 'find_pentad_within'", {

  coordinates <- data.frame(lat = c(10,-10,10,-10), lon = c(-10,10,10,-10))
  expect_error(find_pentad_within(coordinates),NA)

  coordinates <- data.frame(lat = c(38.9, 38.9, 41, 41, 38.9), lon = c(-5, -2.5, -2.5, -5, -5))
  expect_error(find_pentad_within(coordinates),NA)

  coordinates <- data.frame(
    lat = c(40.03066,39.23063, 40.14447, 38.83907, 38.14891, 38.13226, 36.81922, 38.00111, 37.79872, 38.71693, 40.03066),
    lon = c(0.86171,-0.54124,-1.59246,-1.07299,-2.33257,-0.99335,-0.5984,-0.21689,1.06545,-0.00317,0.86171))
  expect_error(find_pentad_within(coordinates,plotting=TRUE),NA)
})

