# compounds_ui.R

# define UI for the Compounds page

# sourced by app.R

# corresponding server functions defined in compounds_server.R

compounds_ui <- function(id) {
  
  # save the namespace id for this ui
  ns <- NS(id)
  
  tabPanel("Compounds",
           pickerInput(inputId = "compound_name", label="Enter Compound Name:", choices=unique(db$Compound), options=list(`live-search`=T)),
           actionButton("search", "Search"),
           tags$br(),
           tags$hr(),
           titlePanel("Papers reporting this compound"),
           DT::DTOutput("paperTable"),
           tags$br(),
           tags$hr(),
           titlePanel("Summary of results"),
           tableOutput("summaryTable"),
           tags$br(),
           tags$hr(),
           titlePanel("Interactive Plots"),
           tags$p("Data may be missing from one plot or the other, because papers may not report both nmol/min and nmol/min/g."),
           fluidRow(
             column(6, plotlyOutput("plot")),
             column(6, plotlyOutput("plot_g"))
           )
  )
  
}

