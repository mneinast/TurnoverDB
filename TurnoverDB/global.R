# global.R

# This is automatically sourced by Shiny at startup, and is executed before ui.R and server.R
#     you do not need to source global.R within app.R

# this is only executed once

# variables defined here are available in both server and ui environments






###########
# libraries
###########
# this is probably way too many libraries, but worry about culling these later
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)
library(purrr)
library(DT)


###############
# load database
###############

# read in the prebuilt database of turnover
# this is constructed by the "build_database.R" script
db <- read.csv("./combined_data.csv")

template_csv <- read.csv("./template_for_submission.csv")


# define a set of default values for newALLSOURCES entries
defaultALLSOURCES <- list(InputFilename = "", # construct this from today's date, doi, email?
                          SubmitterEmail = "",
                          PaperTitle = "",
                          FirstAuthorLastName = "",
                          Year = -1,
                          Permalink = "")


########################################
# load functions shared by ui and server
########################################

#placeholder, not sure how this would work