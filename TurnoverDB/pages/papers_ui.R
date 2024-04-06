# papers_ui.R

# define UI for Papers page

# sourced by app.R

# corresponding papers_server.R


papers_ui <- function(id) {
  
  # save the namespace id for this ui
  ns <- NS(id)
  
  
  tabPanel("Papers",
           tags$p("placeholder for a page which is focused on exploring data from a single paper\n
                    Look up authors, metadata, etc and find all turnover results displayed in tables")
  )
  
}
  
