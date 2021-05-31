test_that("test_gtfs_freq", {

  poa <- read_gtfs(system.file("extdata/poa_gtfs.zip", package="gtfstools"))
  expect_equal(test_gtfs_freq(poa), "simple")

  spo <- read_gtfs(system.file("extdata/spo_gtfs.zip", package="gtfstools"))
  expect_equal(test_gtfs_freq(spo), "frequency")

})
