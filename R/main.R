# Aaron idea - have users set parameters for how to clean
# flag outliers
# add leoka number of cops
# add crosswalk data into it
# add census data
#    labor force, marriage, demographics
library(ggplot2)
library(ggthemes)
library(reshape2)


ucr_agency <- function(ORI = "all", years = 1960:2015, crime = "all",
                       tsfill = FALSE) {
  years <- as.numeric(years)
  ORI <- toupper(ORI)
  crime <- tolower(crime)
  crime <- gsub(" ", "_", crime)
  if (!all(years %in% 1960:2015)) {
    stop(paste0("Error: Inputted years are not available. ",
                "Please choose from 1960:2015"))
  }
  if (!all(ORI %in% unique(ucr_offenses$ORI)) & !all("ALL" %in% ORI)) {
    stop("Error: Inputted ORIs are not available.")
  }

  if (!all(crime %in% c("murder", "rape", "robbery",
                        "aggravated_assault", "burglary",
                        "larceny", "motor_vehicle_theft",
                        "violent_crime", "property_crime", "index_crime",
                        "all"))) {
    stop(paste0("Error: Please check the crime input. For all crimes,",
                " do not enter anything.",
               " Specific crimes available are: ",
               "Murder, Rape, Robbery, Aggravated Assault, Burglary, Larceny",
               " Motor Vehicle Theft, Violent Crime, Property Crime,",
               " and Index Crime."))
  }

  if (!all("ALL" %in% ORI)) {
    dataset <- ucr_offenses[ucr_offenses$ORI %in% ORI &
                              ucr_offenses$year %in% years,]
  }
  else {
    dataset <- ucr_offenses
  }
  dataset$violent_crime <- dataset$murder +
    dataset$robbery +
    dataset$aggravated_assault +
    dataset$rape
  dataset$property_crime <- dataset$burglary +
    dataset$larceny +
    dataset$motor_vehicle_theft
  dataset$index_crime <- dataset$violent_crime +
                         dataset$property_crime
  dataset$year <- as.numeric(dataset$year)

  if (crime != "all") {
    to_grep <- c("state", "ORI", "agency", "year",
                "population", "county", "months")
    to_grep <- append(to_grep, crime)
    dataset <- dataset[, grep(paste(to_grep, collapse = "|"),
                                    names(dataset))]
  }

  if (tsfill) {

    for (i in unique(dataset$ORI)) {
      all.years <- data.frame(year = seq(min(years), max(years)))
      all.years$year <- as.numeric(all.years$year)
      all.years$state <- dataset$state[dataset$ORI == i][1]
      all.years$ORI <- dataset$ORI[dataset$ORI == i][1]
      all.years$agency_type <- dataset$agency_type[dataset$ORI == i][1]
      all.years$population <- dataset$population[dataset$ORI == i][1]
      all.years$county <- dataset$county[dataset$ORI == i][1]
      all.years$agency_name <- dataset$agency_name[dataset$ORI == i][1]
      all.years$all_months_reported <-
        dataset$all_months_reported[dataset$ORI == i][1]

      all.years <- all.years[!all.years$year %in%
                               dataset$year[dataset$ORI == i],]
      all.years <- all.years[!is.na(all.years$year),]
      dataset <- dplyr::bind_rows(dataset, all.years)
    }
  }

  return(dataset)
}



ucr_graph <- function(ORI, years = 1960:2015, crime = "murder",
                      smooth = FALSE, smooth_span = 0.2, log = FALSE) {

  if (smooth_span <= 0) {
    stop("Error: smooth_span too low. Please choose a number 0 < number < 1")
  }

  if (length(ORI) > 1 & length(crime > 1)) {
    stop ("Error: Please select on a single ORI or a single crime to graph")
  }

  crime <- tolower(crime)
  dataset <- ucr_agency(ORI = ORI, years = years)
  dataset <- dataset[, c(2, 4, 9:17)]
  dataset <- melt(dataset, id.vars = c("ORI", "year"))
  dataset <- dataset[dataset$variable %in% crime,]
  dataset$year <- as.numeric(dataset$year)

  if (log) {
    dataset$value <- log(dataset$value)
  }


  if (!smooth) {
  plot <- ggplot(dataset, aes(x = year, y = value, col = variable)) +
      geom_line(size = 1.1) +
      theme_fivethirtyeight() +
      scale_colour_gdocs()
  }
  else {
  plot <- ggplot(dataset, aes(x = year, y = value, col = variable)) +
      stat_smooth(se = FALSE, span = smooth_span) +
      theme_fivethirtyeight() +
      scale_colour_gdocs()
  }

  return(plot)

}
