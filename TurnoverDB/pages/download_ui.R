# download_ui.R

# define UI for Download page

# sourced by app.R

# corresponding download_server.R



download_ui <- function(id) {
  
  # save the namespace id for this ui
  ns <- NS(id)
  
  tabPanel("Download",
           tags$p("placeholder for a button that downloads all data in TurnoverDB as a csv or xlsx")
  )
  
}