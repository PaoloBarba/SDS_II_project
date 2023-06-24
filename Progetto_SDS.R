rm(list = ls())
library(dplyr)
library('jsonlite')

path <- "data/events"
files <- dir(path, pattern = "*.json")
path_file <- paste0(path,"/",files)

data_list <- list()

for (file_path in path_file) {
  data <- fromJSON(file_path)
  data_shot <-  data$shot[is.na(data$shot$outcome) == FALSE,]
  if (dim(data_shot)[2] == 11){
  data_list[[file_path]] <- data_shot
  }
    
}


combined_df <- do.call(bind_rows, data_list)
shot_dataset <- combined_df

load(file = "shotdataset.RData" )

