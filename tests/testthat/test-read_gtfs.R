context("Read GTFS")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs_url  <- "https://github.com/ipeaGIT/gtfstools/raw/master/inst/extdata/spo_gtfs.zip"

gtfs <- read_gtfs(data_path)

# create gtfs with empty table

et_temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")
on.exit(file.remove(et_temp_file))

et_gtfs <- gtfs
et_gtfs$trips <- et_gtfs$trips[trip_id == "def_no_trip"]
write_gtfs(et_gtfs, et_temp_file)
et_gtfs <- read_gtfs(et_temp_file)

# create gtfs with empty file

ef_temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")
on.exit(file.remove(ef_temp_file), add = TRUE)

ef_temp_dir <- file.path(tempdir(), "test_ef_gtfsdir")
on.exit(unlink(ef_temp_dir, recursive = TRUE), add = TRUE)

zip::unzip(data_path, exdir = ef_temp_dir, overwrite = TRUE)
file.remove(file.path(ef_temp_dir, "trips.txt"))
file.create(file.path(ef_temp_dir, "trips.txt"))
zip::zipr(ef_temp_file, file.path(ef_temp_dir, list.files(ef_temp_dir)))
ef_gtfs <- read_gtfs(ef_temp_file, warnings = FALSE)

# create gtfs with file that results in parsing failure

pf_temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")
on.exit(file.remove(pf_temp_file), add = TRUE)

pf_temp_dir <- file.path(tempdir(), "test_pf_gtfsdir")
on.exit(unlink(pf_temp_dir, recursive = TRUE), add = TRUE)

zip::unzip(data_path, exdir = pf_temp_dir, overwrite = TRUE)
agency_txt <- file.path(pf_temp_dir, "agency.txt")
file.remove(agency_txt)
file.create(agency_txt)
agency_txt_con <- file(agency_txt)
writeLines(
  c(
    "agency_id,agency_name,agency_url,agency_timezone,agency_lang,extracol",
    "1,SPTRANS,http://www.sptrans.com.br/?versao=011019,America/Sao_Paulo,pt",
    "1,SPTRANS,http://www.sptrans.com.br/?versao=011019,America/Sao_Paulo,pt"
  ),
  agency_txt_con
)
close(agency_txt_con)
zip::zipr(pf_temp_file, file.path(pf_temp_dir, list.files(pf_temp_dir)))
pf_gtfs <- read_gtfs(pf_temp_file, warnings = FALSE)

# create gtfs with extra file

ext_temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")
on.exit(file.remove(ext_temp_file), add = TRUE)

ext_temp_dir <- file.path(tempdir(), "test_ext_gtfsdir")
on.exit(unlink(ext_temp_dir, recursive = TRUE), add = TRUE)

zip::unzip(data_path, exdir = ext_temp_dir, overwrite = TRUE)
calendar_txt <- file.path(ext_temp_dir, "calendar.txt")
calendar_extra_txt <- file.path(ext_temp_dir, "calendar_extra.txt")
file.copy(from = calendar_txt, to = calendar_extra_txt)
zip::zipr(ext_temp_file, file.path(ext_temp_dir, list.files(ext_temp_dir)))
ext_gtfs <- read_gtfs(ext_temp_file, warnings = FALSE)


# tests -------------------------------------------------------------------


test_that("read_gtfs raises errors due to incorrect input types", {
  expect_error(read_gtfs(as.factor(data_path)))
  expect_error(read_gtfs(data_path, files = NA))
  expect_error(read_gtfs(data_path, files = as.factor("stop_times")))
  expect_error(read_gtfs(data_path, quiet = "TRUE"))
  expect_error(read_gtfs(data_path, warnings = "TRUE"))
})

test_that("read_gtfs raises warnings and messages adequately", {
  expect_silent(read_gtfs(data_path))
  expect_silent(read_gtfs(gtfs_url))
  expect_silent(read_gtfs(ef_temp_file, warning = FALSE))
  expect_silent(read_gtfs(pf_temp_file, warning = FALSE))
  expect_silent(read_gtfs(ext_temp_file))
  expect_message(read_gtfs(data_path, quiet = FALSE))
  expect_message(read_gtfs(gtfs_url, quiet = FALSE))
  expect_message(read_gtfs(ef_temp_file, quiet = FALSE))
  expect_message(read_gtfs(pf_temp_file, quiet = FALSE))
  expect_message(read_gtfs(ext_temp_file, quiet = FALSE))
  expect_warning(read_gtfs(ef_temp_file))
  expect_warning(read_gtfs(pf_temp_file))
})

test_that("read_gtfs results in a gtfs object (if no parsing failure happens)", {

  # a gtfs object is a list with "gtfs" class

  expect_s3_class(gtfs, "gtfs")
  expect_s3_class(et_gtfs, "gtfs")
  expect_s3_class(ef_gtfs, "gtfs")
  expect_s3_class(ext_gtfs, "gtfs")
  expect_type(gtfs, "list")
  expect_type(et_gtfs, "list")
  expect_type(ef_gtfs, "list")
  expect_type(ext_gtfs, "list")

  # every object within list is a dt, even if it's originally an empty table/file

  invisible(lapply(gtfs, expect_s3_class, "data.table"))
  invisible(lapply(et_gtfs, expect_s3_class, "data.table"))
  invisible(lapply(ef_gtfs, expect_s3_class, "data.table"))
  invisible(lapply(ext_gtfs, expect_s3_class, "data.table"))

})

test_that("read_gtfs outputs parsing failures messages", {
  invisible(lapply(pf_gtfs, expect_type, "character"))
})

test_that("read_gtfs reads expected files", {

  files_in_zip <- sub(".txt", "", zip::zip_list(data_path)$filename)
  expect_equal(names(gtfs), files_in_zip)

  one_file_gtfs <- read_gtfs(data_path, "agency")
  expect_equal(names(one_file_gtfs), "agency")

  expect_error(read_gtfs(data_path, "non_existent_file"))

})

test_that("read_gtfs reads all extra files' columns as character", {

  # uses internal data gtfs_metadata - check data-raw/gtfs_metadata.R

  all_files <- names(ext_gtfs)
  ext_files <- all_files[! all_files %in% sub(".txt", "", names(gtfs_metadata))]

  for (file in ext_files) {
    invisible(lapply(ext_gtfs[[file]], expect_type, "character"))
  }

})

test_that("read_gtfs validates gtfs on read", {
  expect_true("validation_result" %in% names(attributes(gtfs)))
})
