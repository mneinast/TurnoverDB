# submit_server.R

# define server logic for Submit page

# sourced by app.R

# corresponding submit_ui.R




submit_server <- function(id){
  moduleServer(id, function(input, output, session){
    # server-side operations for Submit
    
    ###########
    # Submit
    ##########
    
    # Create a reactive values object to hold the data frame
    values <- reactiveValues(temp_df = template_csv)
    
    observe(print(head(values$temp_df, 5)))
    
    # Render editable DataTable
    output$IN_turnovers <- renderDT({
      datatable(values$temp_df, selection = 'none', editable = 'cell',
                options = list(searching = F,
                               paging = F))
    }, server = FALSE)
    
    # Observe cell edits and update the data frame accordingly
    observeEvent(input$IN_turnovers_cell_edit, {
      info <- input$IN_turnovers_cell_edit
      str(info)  # For debugging: prints details of the edit to the console
      
      # Determine the cell that was edited
      i <- info$row
      j <- info$col
      v <- info$value
      
      # Update the data frame
      values$temp_df[i, j] <<- DT::coerceValue(v, values$temp_df[i, j])
    })
    
    # add a button which creates an additional row in the IN_turnovers table
    observeEvent(input$addTurnoversRow, {
      # Assuming template_csv has the same structure as the rows you want to add
      empty_row <- as.list(setNames(rep(NA, ncol(values$temp_df)), names(values$temp_df)))
      
      # Append the new empty row to the data frame
      values$temp_df <- rbind(values$temp_df, empty_row)
    })
    
    
    # when the submit button is clicked...
    selectedUpload <- eventReactive(input$Submit, {
      
      # build the new entry into ALLSOURCES
      newALLSOURCES <- list(InputFilename = "", # construct this from today's date, doi, email?
                            SubmitterEmail = as.character(input$INemail),
                            PaperTitle = as.character(input$INPaperTitle),
                            FirstAuthorLastName = as.character(input$INFirstAuthorLastName),
                            Year = as.integer(input$INYear),
                            Permalink = as.character(input$INdoi)
      )
      
      # add an IF statement checking that all these fields are full (else give message indicating problem)
      # if(all(map2_lgl(newALLSOURCES, defaultALLSOURCES, ~ .x, != .y))){
      #   
      #   
      #   } else {
      #   # produce error message - change this to a message in shiny app
      #   print("at least one of newly submitted entries is the same as default")
      # }
      
      
      # update db with the user's newly submitted data
      # send email to turnoverDB admin, cc user, containing new submission (csv containing new ALLSOURCES entry, csv for new data)
      
    })
  })
}