# ui.R


# ui object is return to app.R

# Define UI
ui <- navbarPage("TurnoverDB",
                 
                 textOutput(getwd()),
                 
                 tabPanel("Home",
                          tags$p("TurnoverDB is a crowd-sourced compilation of circulatory turnover measurements."),
                          tags$br()
                 ),
                 
                 
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
                 ),
                 
                 tabPanel("Papers",
                          tags$p("placeholder for a page which is focused on exploring data from a single paper\n
                    Look up authors, metadata, etc and find all turnover results displayed in tables")
                 ),
                 
                 tabPanel("Submit",
                          tags$p("placeholder for a data submission page.\n
                    Users can submit new data and immediately return to the Compounds or Papers tab to explore it, but\n
                    their newly submitted data is only available during their current session on shiny app.\n\n
                    Users will paste the doi URL into the field, the rcrossref will pull key metadata.  Users clicks a button to confirm metadata.\n
                    Users then see a table which they can enter observations of turnover into.\n
                    When ready, users can click a button to submit and preview data.\n
                    This button creates a csv of the new paper (a single row corresponding to an ALLSOURCES entry) + a corresponding csv of new turnover observations, and sends this to an ingest server/folder.\n
                    Later, the full database is updated with these new values (perhaps thru a gated validation step"),
                          textInput(inputId="INdoi", label="doi:", value=""),
                          textInput(inputId="INPaperTitle", label="Paper Title:", value=""),
                          textInput(inputId="INFirstAuthorLastName", label="First Author's Last Name:", value=""),
                          textInput(inputId="INYear:", label="Year Published (YYYY):", value=""),
                          textInput(inputId="INemail:", label="Your email:", value=""),
                          tags$br(),
                          tags$hr(),
                          DT::DTOutput('IN_turnovers'),
                          actionButton("addTurnoversRow", "Add Row"),
                          tags$br(),
                          tags$hr(),
                          actionButton("Submit", "Submit the turnover data for this paper")
                          
                          
                 ),
                 
                 tabPanel("Download",
                          tags$p("placeholder for a button that downloads all data in TurnoverDB as a csv or xlsx"))
                 
                 
)