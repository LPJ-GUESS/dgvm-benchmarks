# Script that reads LPJ-GUESS fluxdata, selects the same variables as for the 
# ICOS data, and changes it to match the format as from the DGVMTools R package
# See README file for more information about the required input and workflow


GUESS_fluxdata <- function(file_name) {
  library(DGVMTools)
  library(ggplot2)
  library(tidyr)
  library(stringr)
  library(dplyr)
  
  # **** GET INPUT VARIABLES ****
  # reads the required variables and their values from the text file given as
  # input for the ICOS_fluxdata and GUESS_fluxdata functions
  input <-
    read.delim(
      file = "TELLUS_input.txt",
      header = TRUE,
      sep = ""
    )
  
  for (i in 1:nrow(input)) {
    if (grepl(",", input$Value[i]) == TRUE) {
      assign(input$Variable[i], as.list(trimws(strsplit(input$Value[i], ",")[[1]])))
    } else {
      assign(input$Variable[i], input$Value[i])
    }
  }
  
  message("Input for LPJ-GUESS data has been read successfully.\n")
  
  
  # **** DEFINE ADDITIONAL LPJ-GUESS QUANTITIES ****
  GUESS <- defineQuantity(
    id = "dgpp",
    name = "Daily GPP",
    units = "kgC/m^2",
    add.to = GUESS
  )
  
  GUESS <- defineQuantity(
    id = "dnee",
    name = "Daily NEE",
    units = "kgC/m^2",
    add.to = GUESS
  )
  
  GUESS <- defineQuantity(
    id = "dreco",
    name = "Daily Ecosystem Respiration",
    units = "kgC/m^2",
    add.to = GUESS
  )
  
  
  # **** GET LPJ-GUESS VARIABLES IN DGVM FORMAT ****
  GUESS.run <- defineSource(
    id = "GUESS",
    dir = path_GUESS,
    format = GUESS,
    defined.layers = GUESS@predefined.layers,
    name = "LPJ-GUESS output"
  )
  
  for (i in variables_fluxnet) {
    temp <- paste0("d", i)
    assign(
      paste(GUESS.run@id, i, sep = "."),
      getField(
        source = GUESS.run,
        quant = temp,
        first.year = as.numeric(first_year),
        last.year = as.numeric(last_year)
      ),
      .GlobalEnv
    )
  }
  
  return(GUESS.run)
}


GUESS.run <- GUESS_fluxdata(file_name = "TELLUS_input.txt")
