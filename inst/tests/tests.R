# tests------------------------------------------------------------------------

# Data pre-processing tests
testthat::test_that("Data Pre-processing", {
  
  # 'cleaned_data' object should be a tibble dataframe
  expect_that(class(cleaned_data)[1], equals("tbl_df"))
  
  # number of columns in 'cleaned_data' should equal 20
  expect_that(dim(cleaned_data)[2], equals(20))
})


# Leaflet Map Tests
testthat::test_that("Leaflet Map", {
  
  # dataset for generating leaflet map should belong to class 'list'
  expect_that(data_for_leaflet_map, is_a("list"))
  
  # Number of rows in the second element of the 'data_for_leaflet_map'
  # list should equal '2'.
  expect_that(dim(data_for_leaflet_map[[2]])[1], equals(2))
  
  # 'leaflet_map_test' object should belong to class "leaflet"
  expect_that(class(leafletMapTest)[1], equals("leaflet"))
})