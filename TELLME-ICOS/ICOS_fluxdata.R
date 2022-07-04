# Script that reads ICOS fluxdata, selects the desired variables, writes the
# selection to a new .csv file and reformats it to match the format as from the
# DGVMTools R package
# See README file for more information about the required input and workflow

ICOS_fluxdata <- function(file_name) {
  library(readr)
  library(ggplot2)
  library(DGVMTools)
  library(tidyr)
  library(stringr)
  library(dplyr)
  
  
  # **** GET INPUT VARIABLES ****
  # reads the required variables and their values from the text file given as
  # input for the ICOS_data function
  
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
  
  message("Input for observation data has been read successfully.\n")
  
  
  # **** GET REQUIRED ICOS FLUXDATA ****
  # reads the ICOS fluxdata for each site, extracts the required variables as
  # previously defined by the input and writes this to a new .csv file
  
  for (i in 1:length(sites)) {
    obs_data <- separate(
      data = read.csv(
        file = paste0(
          path_ICOS,
          "/",
          list.files(path = path_ICOS, pattern = sites[[i]]) %>%
            str_subset("DD") %>% str_subset("FLUXNET") %>%
            str_subset("VARINFO", negate = TRUE)
        ),
        na = c("-9999", "NA")
      ),
      col = TIMESTAMP,
      sep = c(4, 6),
      into = c("Year", "Day")
    )
    
    temp <- data.frame(
      Year = obs_data$Year,
      Day = str_remove(obs_data$Day, "^0+"),
      Lon = longitude[[i]],
      Lat = latitude[[i]]
    )
    
    for (v in variables_fluxnet) {
      obs <-
        select(obs_data, contains(UT_method)) %>% select(contains(v)) %>%
        select(contains(suffix)) %>% select(contains(c("_DT", "_NT")) |
                                              ends_with(c("DAY", "NIGHT"))) %>% rowSums() / 1000
      temp <- cbind(temp, obs)
    }
    
    # define column names for the temp data frame based on the fluxnet variables
    colnames(temp)[(ncol(temp) - length(variables_fluxnet) + 1):ncol(temp)] <-
      variables_fluxnet
    
    # writing the extracted data to the empty csv file
    if (i == 1) {
      # for the first iteration, column names are included
      write_csv(
        temp,
        file = "ICOS_FLUXNET.csv",
        col_names = TRUE
      )
      fluxdata <- temp
    } else {
      write_csv(
        temp,
        file = "ICOS_FLUXNET.csv",
        col_names = FALSE,
        append = TRUE
      )
      fluxdata <-
        data.table(rbind(as.matrix(sapply(
          fluxdata, as.numeric
        )), as.matrix(sapply(
          temp, as.numeric
        ))), row.names = NULL)
    }
  }
  
  message("Required ICOS data successfully read and written to a new .csv file.\n")
  
  
  # **** CREATE ICOS FORMAT ****
  # listing the possible quantities for the new ICOS format
  layers_obs <- list()
  
  quantities_table <- data.table(
    id = c("GPP", "NEE", "RECO"),
    name = c("GPP", "NEE", "RECO"),
    units = c("kgC/m^2", "kgC/m^2", "kgC/m^2")
  )
  
  quantities_obs <-
    vector(mode = "list", length = rep(nrow(quantities_table)))
  
  for (i in 1:nrow(quantities_table)) {
    quantities_obs[[i]] <- defineQuantity(id = quantities_table$id[i],
                                          name = quantities_table$name[i],
                                          units = quantities_table$units[i])
  }
  
  # extracting the quantities that are present in the observation dataset
  availableQuantities_obs <- function() {
    variables <- variables_fluxnet
    temp <- list()
    for (i in 1:length(quantities_obs)) {
      if (quantities_obs[[i]]@id %in% variables) {
        temp <- append(temp, quantities_obs[[i]])
      }
    }
    return(temp)
  }
  
  getField_obs <- function(source,
                           quant,
                           layers,
                           sta.info,
                           file.name,
                           verbose = FALSE,
                           ...) {
    if (quant@id %in% variables_fluxnet) {
      fluxdata %>% select(Year, Day, Lon, Lat, quant@id) -> quant_data
    }
    
    field.id <-
      makeFieldID(source = source,
                  quant.string = quant@id,
                  sta.info = sta.info)
    
    return.field <- new(
      "Field",
      id = field.id,
      quant = quant,
      data = quant_data,
      first.year = as.integer(min(quant_data$Year)),
      last.year = as.integer(max(quant_data$Year)),
      year.aggregate.method = "none",
      spatial.extent = extent(
        min(as.numeric(quant_data$Lon)),
        max(as.numeric(quant_data$Lon)),
        min(as.numeric(quant_data$Lat)),
        max(as.numeric(quant_data$Lat))
      ),
      spatial.extent.id = "Full",
      spatial.aggregate.method = "none",
      subannual.resolution = "Day",
      subannual.aggregate.method = "none",
      subannual.original = "Day",
      source = source
    )
    
    return(return.field)
  }
  
  ICOS <- new(
    "Format",
    id = "ICOS",
    predefined.layers = layers_obs,
    quantities = quantities_obs,
    availableQuantities = availableQuantities_obs,
    getField = getField_obs
  )
  
  
  # **** GET ICOS VARIABLES IN DGVM FORMAT ****
  ICOS.run <- defineSource(
    id = "ICOS",
    dir = path_ICOS,
    format = ICOS,
    name = paste("ICOS", "Data", sep = " ")
  )
  
  for (i in availableQuantities_obs()) {
    assign(
      paste(ICOS.run@id, i@id, sep = "."),
      getField(source = ICOS.run, quant = i),
      .GlobalEnv
    )
  }
  
  return(ICOS.run)
}

ICOS.run <- ICOS_fluxdata(file_name = "TELLUS_input.txt")

