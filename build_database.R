# build_database.R



# helper script which constructs a dataframe from input data. This dataframe is saved to a defined location as "combined_data.csv", which can then be read by the shiny app.

# save the dataframe as a versioned item, then the shiny app can load the latest version


##############

#libraries

# read table of papers
papers <- read.csv("ALLSOURCES.csv")

# read list of source csv
input_filepaths <- list.files(path="./sourcedata/", pattern = "\\.csv$", full.names=T)
input_filenames <- list.files(path="./sourcedata/", pattern = "\\.csv$", full.names=F)

# initialize the empty list to store the dataframes
data_frames <- list()

# Loop through the files, read each file and store the data frame in the list
for (i in input_filepaths) {

  data <- read.csv(i)
  
  data$InputFilename <- i
  
  data_frames[[i]] <- data
}

# Combine all data frames into a single data frame
# Assumes all data frames have the same structure
combined_data <- do.call(rbind, data_frames)
rownames(combined_data) <- NULL

# trim input filename
combined_data$InputFilename <- substr(combined_data$InputFilename, 14, nchar(combined_data$InputFilename))

# merge paper information
combined_data <- dplyr::full_join(papers, combined_data)

# add a helper column that indicates if unusual is NA
combined_data$isUnusual <- dplyr::if_else(combined_data$Unusual != "", T, F)

# modify the Fasting column to be more clear than T or F when displayed
combined_data$Fasting <- dplyr::if_else(combined_data$Fasting == T, "Fasting", if_else(combined_data$Fasting == F, "Fed", "n/a")) 


# save the combined dataframe to a csv file
write.csv(combined_data, "./TurnoverDB/combined_data.csv")


#################################################
# TESTING save the dataframe to an SQLite file
library(RSQLite)

# make the connection (and create the db if it does not yet exist)
con <- dbConnect(SQLite(), dbname = "combined_data_test.sqlite")

# write a new table in the db using combined_data
dbWriteTable(con, "combined_data", combined_data)

# verify
dbReadTable(con, "combined_data")

# close connection
dbDisconnect(con)



