# server.R


# server object is returned to app.R

server <- function(input, output) {
  
  print(paste(getwd()))
  
  
  ###########
  # Compounds
  ##########
  
  # select a compound, perform filtering on the db
  selected_data <- eventReactive(input$search, {
    req(input$compound_name) # Ensure that a compound name is provided
    filtered <- db %>% 
      filter(Compound == input$compound_name) %>%
      mutate(Permalink = paste('<a href="', Permalink, '" target="_blank">', Permalink, '</a>', sep = "")) %>%
      mutate(tooltip_text = paste(Unusual, Fasting, PaperTitle, sep="\n"))
    
    return(filtered)
  })
  
  
  # summary table
  output$summaryTable <- renderTable({
    req(input$compound_name) # Ensure that a compound name is provided
    selected_data() %>%
      group_by(Species, Fasting, isUnusual) %>%
      summarise(
        Mean_Ra_nmol_min = mean(Ra_nmol_min, na.rm=T),
        Mean_Ra_nmol_min_g = mean(Ra_nmol_min_g, na.rm=T),
        Number_of_Experiments = n(),
        Number_of_Papers = length(unique(PaperTitle)),
        .groups = 'drop'
      )
  })
  
  # plot_g
  output$plot_g <- renderPlotly({
    req(nrow(selected_data()) > 0) # Ensure that there is data to plot
    
    max_y <- max(selected_data()$Ra_nmol_min_g, na.rm=T) * 1.5
    
    p <- ggplot(selected_data(), aes(x=TracerElements, y = Ra_nmol_min_g, text=tooltip_text)) +
      geom_boxplot() +
      geom_jitter(aes(color=substr(selected_data()$PaperTitle, 1, 25), shape=selected_data()$isUnusual), width=0.1, size=2) +
      egg::theme_article() +
      ylim(0, max_y) +
      labs(y="Ra (nmol/min/g)", x="TracerElement", title=paste(input$compound_name), color="Paper") +
      guides(shape=guide_none()) +
      facet_wrap(~Species, scales="free_y") +
      theme(legend.position = "bottom")
    ggplotly(p, tooltip="text")
  })
  
  # plot
  output$plot <- renderPlotly({
    req(nrow(selected_data()) > 0) # Ensure that there is data to plot
    
    max_y <- max(selected_data()$Ra_nmol_min, na.rm=T) * 1.5
    
    p <- ggplot(selected_data(), aes(x=TracerElements, y = Ra_nmol_min, text=tooltip_text)) +
      geom_boxplot() +
      geom_jitter(aes(color=substr(selected_data()$PaperTitle, 1, 25), shape=selected_data()$isUnusual), width=0.1, size=2) +
      egg::theme_article() +
      ylim(0, max_y) +
      labs(y = "Ra (nmol/min)", x="TracerElement", title = paste(input$compound_name), color="Paper") +
      guides(shape=guide_none()) +
      facet_wrap(~Species, scales="free_y") 
    ggplotly(p, tooltip="text")
  })
  
  
  # paper table
  output$paperTable <- DT::renderDT({
    req(input$compound_name)
    selected_data() %>%
      group_by(PaperTitle, Year, Permalink) %>%
      summarise(Species = toString(unique(Species)),
                Sex = toString(unique(Sex)),
                TracerElements = toString(unique(TracerElements)),
                Fasting = toString(unique(Fasting)),
                Unusual = toString(unique(Unusual))
      ) %>%
      DT::datatable(escape=F)
  })
  
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
  
  
  
  
  
}
