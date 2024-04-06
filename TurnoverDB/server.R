# server.R


# server object is returned to app.R

# server functions must be sourced prior to defining the server function

source("./pages/home_server.R")
source("./pages/papers_server.R")
source("./pages/compounds_server.R")
source("./pages/submit_server.R")
source("./pages/download_server.R")


# define server
server <- function(input, output) {
  
  # call the function for each server, passing the id that was provided in the ui page
  home_server("home")
  papers_server("papers")
  compounds_server("compounds")
  submit_server("submit")
  download_server("download")
  
  
  
}
