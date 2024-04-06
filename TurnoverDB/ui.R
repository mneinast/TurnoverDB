# ui.R


# ui object is return to app.R

# Overall, this is a navbarPage containing tabs.
# Tabs are defined in other files inside the "pages" directory.
# tabs must be sourced prior to calling the ui function

source("./pages/home_ui.R")
source("./pages/papers_ui.R")
source("./pages/compounds_ui.R")
source("./pages/submit_ui.R")
source("./pages/download_ui.R")


# Define UI
ui <- navbarPage("TurnoverDB",
                 
                 # call the function for each page ui, passing an id that will also be passed to the corresponding server function
                 home_ui("home"),
                 papers_ui("papers"),
                 compounds_ui("compounds"),
                 submit_ui("submit"),
                 download_ui("download")
                 
)