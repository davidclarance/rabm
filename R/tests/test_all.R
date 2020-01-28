source("../extract_all.R", chdir = TRUE)
source("../pull_single_all_location.R", chdir = TRUE)
source("../extract_observers.R", chdir = TRUE)
source("../pull_single_observer_location.R", chdir = TRUE)
source("../extract_species.R", chdir = TRUE)
source("../pull_single_specie_location.R", chdir = TRUE)
library(testthat)

test_that("Check 'extract_all'", {
  expect_error(extract_all(), NA)
  expect_error(extract_all(start_date = 'dummy'))
  expect_error(extract_all(region_type = 'dummy'))
  expect_error(extract_all(region_ids = c('kenya','mozambique')), NA)
  expect_error(extract_all(region_type = 'pentad', region_ids = '0115_3640'), NA)
  expect_error(extract_all(region_type = 'pentad', region_ids = '0325c3750'), NA) # Empty pentad
  expect_error(extract_all(region_type = 'group', region_ids = '40147_NrthCst-KE'), NA)
  #expect_error(extract_all(region_type = 'qdgc', region_ids = ''), NA)
})

test_that("Check 'extract_observers'", {
  expect_error(extract_observers(observer_ids = 10723), NA)
  expect_error(extract_observers(observer_ids = '10723'), NA)
  expect_error(extract_observers(observer_ids = 10723, start_date = 'dummy'))
  expect_error(extract_observers(observer_ids = 'dummy'))
  expect_error(extract_observers(observer_ids = c('10723', '40147')), NA)
  expect_error(extract_observers(observer_ids = 10723, region_ids = c('kenya','mozambique')), NA)
  expect_error(extract_observers(observer_ids = 10723, region_type = 'pentad', region_ids = '0115_3640'), NA)
  expect_error(extract_observers(observer_ids = 10723, region_type = 'pentad', region_ids = '0325c3750'), NA) # Empty pentad
  expect_error(extract_observers(observer_ids = 10723, region_type = 'group', region_ids = '40147_NrthCst-KE'), NA)
  #expect_error(extract_observers(observer_ids = 10723, region_type = 'qdgc', region_ids = ''), NA)
})

test_that("Check 'extract_species'", {
  expect_error(extract_species(species_ids = '400'), NA)
  expect_error(extract_species(species_ids = 'dummy'))
  expect_error(extract_species(species_ids = 'dummy'))
  expect_error(extract_species(species_ids = c('10723', '40147')), NA)
  expect_error(extract_species(region_ids = c('kenya','mozambique')), NA)
  expect_error(extract_species(region_type = 'pentad', region_ids = '0115_3640'), NA)
  expect_error(extract_species(region_type = 'pentad', region_ids = '0325c3750'), NA) # Empty pentad
  expect_error(extract_species(region_type = 'group', region_ids = '40147_NrthCst-KE'), NA)
  #expect_error(extract_species(region_type = 'qdgc', region_ids = ''), NA)
})
