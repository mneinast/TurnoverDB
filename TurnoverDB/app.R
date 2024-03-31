

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)

# read in the prebuilt database of turnover
# this is constructed by the "build_database.R" script
db <- read.csv("../combined_data.csv")


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
                    their newly submitted data is only available during their current session on shiny app.  I will still\n
                    curate everything for now.")
             ),
    
    tabPanel("Download",
             tags$p("placeholder for a button that downloads all data in TurnoverDB as a csv or xlsx"))
    
    
)

# Define server logic
server <- function(input, output) {
  
  # select a compound, perform filtering on the db
  selected_data <- eventReactive(input$search, {
    req(input$compound_name) # Ensure that a compound name is provided
    filtered <- db %>% 
      filter(Compound == input$compound_name) %>%
        mutate(Permalink = paste('<a href="', Permalink, '" target="_blank">', Permalink, '</a>', sep = "")) %>%
          mutate(tooltip_text = paste(Unusual, PaperTitle, sep="\n"))
    
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
      facet_wrap(~Species) +
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
      facet_wrap(~Species) 
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
}

# Run the application
shinyApp(ui = ui, server = server)

