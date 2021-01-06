library(shiny)
folder_address = getwd()
shiny::runApp(folder_address, launch.browser=F, port = 54321, host=getOption('shiny.host', "0.0.0.0"))
