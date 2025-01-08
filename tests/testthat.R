Sys.setenv(OMP_THREAD_LIMIT = 2)

library(testthat)
library(gtfstools)

if (!testthat:::on_cran()) test_check("gtfstools")
