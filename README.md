
# appsilonRShinyTaskAarshBatraVersion2

<!-- badges: start -->
<!-- badges: end -->

The goal of appsilonRShinyTaskAarshBatraVersion2 is to visualize the
marine dataset to figure out the max distance travelled by a ship
between two consecutive points given all of its trips.

# Improvements over the previous version

-   The initial error that was showing up (for the first few seconds)
    while rendering the map, is now resolved.

-   Better UI using grid graphics from the shiny.semantic library.

## App

-   The app is deployed online at shinyapps.io. Click
    [here](https://aarsh.shinyapps.io/appsilonRShinyTaskAarshBatraVersion2/)
    to view the app.

-   To view underlying app code, look into the **app.R** file.

## Code

-   All of the functions used to make distance calculations can be found
    in the **shinyTaskMasterScript.R** file. This file is present in the
    R sub-directory.

-   Documentation for all of the functions is auto-generated by roxygen2
    and can be found in the **man** folder.

-   Note: Although I have used **roxygen2** package for commenting for
    all code files. This repository is not a R package. There is some
    more work that will go into this before this becomes a R package. I
    will work on that after submitting this task.

## Tests

-   All of the tests done using the **testthat()** package are present
    in inst/tests/tests.R

## Data

-   Raw marine data zip file that is used for all calculations is
    contained in the **data-raw** directory.

## Further Information

-   For any questions regarding the app and the code please feel free to
    email me at <aarshbatra.in@gmail.com>.
