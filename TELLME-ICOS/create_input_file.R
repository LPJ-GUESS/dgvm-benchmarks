# Script to create an input file that is required for running ICOS_fluxdata.R
# Currently only sites in Sweden are included but the code can be easily adapted 
# to include other countries as well


create_input_file <- function() {
  library(tidyr)
  library(stringr)
  library(data.table)
  
  # **** PATHS ****
  path_ICOS <- readline(prompt="Enter path where the ICOS data is saved (or press enter to set it to the current working directory): ") %>% purrr::map(~str_trim(., side = "left"))
  path_GUESS <- readline(prompt="Enter path where the LPJ-GUESS output is saved (or press enter to set it to the current working directory): ")
  
  if (path_ICOS == "") {
    path_ICOS <- getwd()
  }
  
  if (path_GUESS == "") {
    path_GUESS <- getwd()
  }

  
  # **** SITES, LONGITUDE, LATITUDE ****
  all_ICOS_sites <- data.table(
    Country = rep("Sweden", 4),
    Id = c("Htm", "Svb", "Nor", "Deg"),
    Lon = c(13.25, 19.75, 17.25, 19.25), 
    Lat = c(56.25, 64.25, 60.25, 64.25)
  )
  
  country <- "Sweden" # currently only Sweden is included
  cat("The available sites for", country, "are: ", toString(all_ICOS_sites$Id[which(all_ICOS_sites$Country == country)]))
  sites_input <- readline(prompt="Press enter to include all of them, or any of the letter keys to continue to the selection process. \n")
  
  if (sites_input == "") {
    sites <- as.list(all_ICOS_sites$Id[which(all_ICOS_sites$Country == country)])
    longitude <- as.list(all_ICOS_sites$Lon[which(all_ICOS_sites$Country == country)])
    latitude <- as.list(all_ICOS_sites$Lat[which(all_ICOS_sites$Country == country)])
  } else {
    sites <- list()
    longitude <- list()
    latitude <- list()
    for (i in which(all_ICOS_sites$Country == country)) {
      temp <- ""
      while (tolower(temp) != "y" && tolower(temp) != "n") {
        temp <- readline(prompt=paste0("Include ", all_ICOS_sites$Id[i], "? Yes = y, no = n. \n"))
        if (tolower(temp) == "y") {
          sites <- append(sites, all_ICOS_sites$Id[i])
          longitude <- append(longitude, all_ICOS_sites$Lon[i])
          latitude <- append(latitude, all_ICOS_sites$Lat[i])
          break
        } else if (tolower(temp) == "n") {
          break
        } else {
          cat("Please enter either y or n.\n")
        }
      }
      next
    }
  }
  
  
  # **** TIME PERIOD ****
  temp <- FALSE
  while (temp == FALSE) {
    first_year <- readline(prompt="Press enter to include all available years, or enter the first year to be included.")
    if (first_year == "") {
      temp <- TRUE
    } else if (is.numeric(as.numeric(first_year)) == TRUE && nchar(first_year) == 4) {
      temp <- TRUE
    } else {
      message("Please enter a valid numerical value.")
    }
  }
  
  last_year <- ""
  if (first_year != "") {
    temp <- FALSE
    while (temp == FALSE) {
      last_year <- readline(prompt="Enter the last year to be included.")
      if (is.numeric(as.numeric(last_year)) == TRUE && nchar(last_year) == 4 && last_year >= first_year) {
        temp <- TRUE
      } else if (is.numeric(as.numeric(last_year)) == TRUE && last_year < first_year) {
        message("Last year cannot be smaller than the first year to be included!")
      } else {
        message("Please enter a valid numerical value.")
      }
    }
  }


  
  # **** FLUXNET SPECIFIC VARIABLES ****
  UT_method_input <- readline(prompt="Press enter to set the USTAR method to VUT, or hit any of the letter keys to set it to CUT.")
  if (UT_method_input == "") {
    UT_method <- "VUT"
  } else {
    UT_method <- "CUT"
  }

  suffix <- "REF"
  

  # **** FLUXNET VARIABLES SELECTION ****
  supported_fluxnet_variables <- c("GPP", "NEE", "Reco")
  cat("The supported fluxnet variables are: ", toString(supported_fluxnet_variables))
  fluxnet_variables_input <- readline(prompt="Press enter to include all of them, or any of the letter keys to continue to the selection process. \n")
  
  if (fluxnet_variables_input == "") {
    variables_fluxnet <- as.list(supported_fluxnet_variables)
  } else {
    variables_fluxnet <- list()
    for (i in supported_fluxnet_variables) {
      temp <- ""
      while (tolower(temp) != "y" && tolower(temp) != "n") {
        temp <- readline(prompt=paste0("Include ", i, "? Yes = y, no = n. \n"))
         if (tolower(temp) == "y") {
          variables_fluxnet <- append(variables_fluxnet, i)
          break
        } else if (tolower(temp) == "n") {
          break
        } else {
          cat("Please enter either y or n.\n")
        }
      }
      next
    }
  }
  
  
  # **** WRITING TO INPUT TEXT FILE ****
  data_input <- data.table(
    Variable = c("path_ICOS", "path_GUESS", "sites", "longitude", "latitude", 
                 "first_year", "last_year", "UT_method", "suffix", "variables_fluxnet"),
    Value = c(as.character(path_ICOS), as.character(path_GUESS), toString(sites), toString(longitude),
              toString(latitude), first_year, last_year, UT_method, suffix, toString(variables_fluxnet)))
  
  write.table(data_input, file = "TELLUS_input.txt", row.names = FALSE, col.names = TRUE)
  
  if (length(sites) == 0) {
    warning("No sites are selected!")
  }
}

create_input_file()

