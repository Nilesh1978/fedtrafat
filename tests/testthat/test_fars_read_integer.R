make_file_name_test_year <- function(year) {
  year <- as.integer(year)
  year
  #sprintf("accident_%d.csv.bz2", year)
}

expect_that(class(make_file_name_test_year(2003)), is_equivalent_to("integer"))
