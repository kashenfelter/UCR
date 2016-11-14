# Aaron idea - have users set parameters for how to clean
# flag outliers
# add leoka number of cops
# add crosswalk data into it
# add census data
#    labor force, marriage, demographics

# Gets agency data from UCR offenses
get_agency <- function(ORI, years = 1960:2015) {
  if (!all(years %in% 1960:2015)) {
    stop(paste0("Error: Inputted years are not available. ",
         "Please choose from 1980:2015"))
  }
  if (!all(ORI %in% unique(ucr_offenses$ORI))) {
    stop("Error: Inputted ORIs are not available.")
  }


  dataset <- ucr_offenses[ucr_offenses$ORI %in% ORI &
                            ucr_offenses$year %in% years,]
  dataset$violent_crime <- dataset$murder +
                           dataset$robbery +
                           dataset$aggravated_assault +
                           dataset$rape
  dataset$property_crime <- dataset$burglary +
                            dataset$larceny +
                            dataset$motor_vehicle_theft


  return(dataset)
}

# Gets county data from UCR offenses
get_county <- function(county, state, agg = FALSE, ORI_to_remove = "") {
  state <- tolower(state)

  offenses$state <- tolower(offenses$state)
  dataset <- offenses[offenses$state == state &
                        offenses$county == county,]
  # ORI to remove
  dataset <- dataset[!dataset$ORI %in% ORI,]


  if (agg) {
    dataset$agency_name <- NULL
    dataset$state <- NULL
    dataset$county <- NULL
    dataset$ORI <- NULL
    dataset$mailing_addressline_2 <- NULL
    dataset$all_months_reported <- NULL
    dataset <- dataset[!is.na(dataset$year),]

    for (i in 2:ncol(dataset)) {
      dataset[,i][is.na(dataset[,i])] <- 0
    }

    dataset <- aggregate(. ~ year, data = dataset, FUN = sum)
    dataset$state <- state
    dataset$county <- county
    dataset <- dataset[, c(61:62, 1:60)]
  }

  return(dataset)
}

# Gets state data from UCR offenses
get_state <- function(state, agg = FALSE, ORI_to_remove = "") {
  state <- tolower(state)

  load("C:/Users/user/Dropbox/Consent Decrees/offenses.rda")
  offenses$state <- tolower(offenses$state)
  dataset <- offenses[offenses$state == state,]
  # ORI to remove
  dataset <- dataset[dataset$ORI != ORI_to_remove,]


  if (agg) {
    dataset$agency_name <- NULL
    dataset$state <- NULL
    dataset$county <- NULL
    dataset$ORI <- NULL
    dataset$mailing_addressline_2 <- NULL
    dataset$all_months_reported <- NULL
    dataset <- dataset[!is.na(dataset$year),]


    for (i in 2:ncol(dataset)) {
      dataset[,i][is.na(dataset[,i])] <- 0
    }


    dataset <- aggregate(. ~ year, data = dataset, FUN = sum)
    dataset$state <- state
    dataset <- dataset[, c(61, 1:60)]
  }

  return(dataset)
}
