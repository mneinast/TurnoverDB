

# this is probably way too many libraries, but worry about culling these later
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)
library(purrr)
library(DT)

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


# Define UI
ui <- navbarPage("TurnoverDB",
  
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

# Define server logic
server <- function(input, output) {
  
  
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

# Run the application
shinyApp(ui = ui, server = server)

