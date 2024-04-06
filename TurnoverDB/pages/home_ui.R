# home_ui.R

# define UI for the home page

# sourced by app.R

# corresponding server functions defined in home_server.R
#  the 'id' is used within app.R to link home_ui to server_ui


home_ui <- function(id) {
  
  # save the namespace id for this ui
  ns <- NS(id)
  
  tabPanel("Home",
           tags$p("TurnoverDB is a crowd-sourced compilation of circulatory turnover measurements."),
           tags$br()
  )
  
}
  



