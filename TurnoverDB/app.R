# app.R

# calls ui.R and server.R



##################################
# source the ui and server scripts
##################################
# each script defines a 'ui' object and a 'server' object.  This is used by shinyApp().
source("ui.R")
source('server.R')




################
# run the app
################
shinyApp(ui = ui, server = server)




