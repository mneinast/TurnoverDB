# submit_ui.R

# define UI for Submit page

# sourced by app.R

# corresponding submit_server.R



submit_ui <- function(id) {
  
  # save the namespace id for this ui
  ns <- NS(id)
  
  
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
           )
  
  
}




         
         