## Appsilon R Shiny Task master script file: Aarsh Batra=======================

# metadata---------------------------------------------------------------------
# author: Aarsh Batra
# Start Date: February 2, 2021
# R version: 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18363)
# R Studio version: 1.4.1103
# e-mail: aarshbatra.in@gmail.com

# libraries--------------------------------------------------------------------
library(tidyverse)
library(knitr)
library(devtools)
library(stringr)
library(tidyr)
library(dplyr)
library(magrittr)
library(data.table)
library(lubridate)
library(roxygen2)
library(rsconnect)
library(htmlwidgets)
library(testthat)
library(shiny)
library(ggplot2)
library(leaflet)
library(shiny.semantic)
library(leaflet.extras)
library(geodist) # for calculating distances given Lat and Long

# read in data-----------------------------------------------------------------

#' reading raw data into R
#'
#' This functions reads in raw data into R (after performing a few
#' sanity checks) and throws an error if for any reason data cannot be read.
#'
#'
#' @importFrom readr read_csv
#' @importFrom base file.exists stop
#' @param path_to_file path to the file where the data is stored
#'
#' @return This function returns an R object which contains the raw dataset.
#'
#' @examples
#' read_raw_data(path_to_file = "path/to/data.csv")
#'
#' @export

read_raw_data <- function(path_to_file){

  if(!is.character(path_to_file)){
    stop("path_to_file should be of type character")
  }

  if(!file.exists(path_to_file)){
    stop("file not present in the specified location. Please make sure
         that you point the path to the raw data csv file.")
  }
  raw_data <- readr::read_csv(file = path_to_file)
  raw_data
}


# cleaning and rearranging raw dataset for analysis----------------------------

#' cleaning and rearranging Raw Data for analysis
#'
#' This function takes in the raw dataset and does some basic cleaning (if any)
#' rearranging (if any).
#'
#' @importFrom dplyr arrange select
#' @param raw_data raw dataset that is returned by the \code{read_raw_data}
#'        function
#'
#' @return this function returns the cleaned dataset which is ready
#'         for analysis.
#'
#' @examples
#' clean_raw_data(raw_data = raw_dataset)
#'
#' @export

clean_raw_data <- function(raw_data){

  # rearrange dates into chronological order
  cleaned_data <- dplyr::arrange(raw_data, DATETIME)

  # rearrange columns such that LON column is the first column
  cleaned_data <- dplyr::select(cleaned_data, LON, everything())

  cleaned_data
}


#' filter cleaned data
#'
#'  Filters cleaned data using columns \code{ship_type} and \code{ship_name}
#'
#'  @importFrom dplyr filter
#'
#'  @param cleaned_data this is the clean dataset (which is obtained as a
#'                     result of cleaning the raw dataset that is read into R.
#'                     This dataset will be filtered in this function.
#'
#'  @param ship_type type of ship with the default value "Tanker"
#'  @param ship_name name of ship with the default value "MARINUS"
#'
#'  @return returns cleaned filtered dataset given input values to parameters
#'
#'  @examples
#'  filter_cleaned_data(cleaned_data = cleaned_dataset, ship_type = "Cargo",
#'  ship_name = "MERI")
#'
#'  @export

filter_cleaned_data <- function(cleaned_data, ship_type = "Tanker",
                                ship_name = "MARINUS"){
  filtered_cleaned_data <- cleaned_data %>%
    dplyr::filter(ship_type == ship_type,
                  SHIPNAME == ship_name)
  filtered_cleaned_data
}


#' Calculate max distance travelled between two (lat, lon) points.
#'
#' Function that calculates max distance travelled (b/w 2 consecutive points)
#' from a given ship's travel history. Ship is selected using
#' \code{filter_ship_type} and \code{filter_ship_name} filters),
#'
#' @importFrom dplyr filter
#' @importFrom base matrix colnames abs
#' @importFrom geodist geodist
#'
#'
#' @param cleaned_data this is the clean dataset (which is obtained as a
#'                     result of cleaning the raw dataset that is read into R
#'
#' @param filter_ship_type type of ship with the default value "Tanker"
#' @param filter_ship_name name of ship with the default value "MARINUS"
#'
#' @return returns a list containing 2 elements. First element returned is the
#'         \code{max_dist}: max distance. Second element returned is the
#'         \code{max_tibble}: a tibble that contains all data that
#'         correspond to the points b/w which the given ship in question
#'         travelled the maximum distance.
#'
#' @examples
#' max_dist_travelled(cleaned_data = cleaned_dataset,
#' filter_ship_type = "Cargo", filter_ship_name = "MERI")
#'
#'
#' @export

max_dist_travelled <- function(cleaned_data,
                               filter_ship_type = "Tanker",
                               filter_ship_name = "MARINUS"){

  # filter the cleaned data
  filtered_cleaned_data <- cleaned_data %>%
    dplyr::filter(ship_type == filter_ship_type,
                  SHIPNAME == filter_ship_name)


  # setting empty data structures
  num_col_needed_to_calc_dist <- 2 # LON and LAT
  max_tibble <- as.data.frame(matrix(NA, nrow = num_col_needed_to_calc_dist,
                                     ncol = ncol(filtered_cleaned_data)))
  colnames(max_tibble) <- colnames(filtered_cleaned_data)

  max_dist <- -1 # This is the default placeholder which will be updated to
  # 0 (or more, given distance calculations) once the main
  # for loop starts calculating distances. This as of now
  # just serves as a dummy. Don't get confused by the negative
  # sign as this is just a dummy placeholder variable. The unit
  # in which this variable is measured is meters.


  # The loop takes in the filtered cleaned data set (given ship_type and
  # ship_name filters). It goes through this dataset 2 consecutive rows
  # at a time, calculating distance travelled b/w those two
  # points. Then it compares this distance with the "max_dist" variable
  # and updates the 'max_dist' and the 'max_tibble' variable if the distance
  # in the current iteration is greater than or equal to the 'max_dist'
  # variable.
  for(i in 1 : (nrow(filtered_cleaned_data) - 1)){
    current_tibble <- filtered_cleaned_data[(i:(i+1)), ]
    current_tibble_dist_cols <- current_tibble[,
                                  (1 : num_col_needed_to_calc_dist)]
    curr_dist <- geodist::geodist(current_tibble_dist_cols,
                                  sequential = TRUE,
                                  measure = "geodesic")

    if((is.na(abs(curr_dist))[1] == TRUE)){
      max_dist <- max_dist
      max_tibble <- max_tibble
    } else if(abs(curr_dist) >= max_dist){
      max_dist <- curr_dist
      max_tibble <- current_tibble
    } else {
      next
    }
  }
  return(list(max_dist, max_tibble))
}

# All function definitions completed-------------------------------------------
#-----------------------------------------------------------------------------#
###############################################################################


# exploring: raw dataset-------------------------------------------------------
raw_data <- read_raw_data(path_to_file = "data-raw/ships.csv")
# dim(raw_data)
# length(unique(raw_data$SHIP_ID))
# base::summary(raw_data)



# building a test case --------------------------------------------------------
cleaned_data <- clean_raw_data(raw_data)

# finding the ship_type/ship_name group with most variation in lat and long
# to use as a work case for testing code.

cleaned_data_test_case <- cleaned_data %>%
  dplyr::group_by(ship_type, SHIPNAME) %>%
  dplyr::summarise(var_in_lat = sd(LAT), var_in_lon = sd(LON)) %>%
  dplyr::filter(var_in_lat == max(var_in_lat), var_in_lon == max(var_in_lon))

# it turns out that the duo with ship_type = "Tanker" and
# ship_name = "MARINUS" has the most variation in Latitude and Longitude
# (together), so for the filtering function I have set these values as
# the default values.

filtered_cleaned_data <- filter_cleaned_data(cleaned_data,
                                             ship_type = "Tanker",
                                             ship_name = "MARINUS")

# preparing the dataset that will be used to generate the leaflet map
data_for_leaflet_map <- max_dist_travelled(cleaned_data = cleaned_data,
                                           filter_ship_type = "Tanker",
                                           filter_ship_name = "MARINUS")

# leaflet map generated
leafletMapTest <- leaflet::leaflet() %>%
  leaflet::setView(lng = unlist(data_for_leaflet_map[[2]][(1:2), 1])[[1]][1],
          lat = unlist(data_for_leaflet_map[[2]][(1:2), 2])[[1]][1] ,
          zoom = 3) %>%
  leaflet::addProviderTiles("Esri.WorldStreetMap") %>%
  leaflet::addAwesomeMarkers(
    data = data_for_leaflet_map[[2]][, (1:2)],
   label = str_c("Shipname: ", data_for_leaflet_map[[2]]$SHIPNAME[1], "---",
               "Departing from: ", data_for_leaflet_map[[2]]$DESTINATION[1],
               "---", "Arriving at: ",
               data_for_leaflet_map[[2]]$DESTINATION[2], "---",
               "Distance Travelled (in meters): ",
               data_for_leaflet_map[[1]])) %>%
  leaflet::addPolylines(lng = unlist(data_for_leaflet_map[[2]][(1:2), 1]),
               lat = unlist(data_for_leaflet_map[[2]][(1:2), 2]))





