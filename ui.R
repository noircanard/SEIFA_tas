########################################################################
#
#    Shiny App (ui)- DPFEM 
#
#    see server for details
#
########################################################################
# call in required packages
library(shiny)
library(leaflet)
########################################################################
# ui
ui <- fluidPage(
  
  tabPanel("Hospital Location Overview", 
           helpText("Select a hospital from the map below."),
           column(12,leafletOutput("map", height="800px"))
           ) #END tabPanel
  
) #END fluidPage
#END ui.R ##############################################################
