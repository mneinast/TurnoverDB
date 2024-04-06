# compounds_server.R

# define server logic for Compounds page

# sourced by app.R

# corresponds to compounds_ui.R




compounds_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # server-side operations for Compounds
    
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
  })
}