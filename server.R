########################################################################
#
#    Shiny App (server)- DPFEM map
#
#
########################################################################
# call in required packages
library(shiny)    # required for shiny app deployment
library(leaflet)  # required for mapping - inc tiles
library(ggplot2)  # required for plots
#
# read in data stored on github 
# github:
github <- "https://raw.githubusercontent.com/noircanard/"
# dir:
SEIFA <- "SEIFA_tas/master/"
# files:

dpfem.dat <- "Police_Fire_ER.csv"
SEIFA.dat <- "SEIFA_IRSD2011.csv"

shape <- readOGR(dsn = '.', layer = "tas_SEIFA")

DPFEM <-  read.csv(paste(github,SEIFA,dpfem.dat,sep=""), stringsAsFactors = F)
DPFEMed <- subset(DPFEM,  Group != "" & Agency %in% c("Police",  "Hospital"))

seifa <-  read.csv(paste(github,SEIFA,SEIFA.dat,sep=""), stringsAsFactors = F)
tas.seifa <- subset(seifa, seifa$Ranking.within.State.or.Territory.State == "TAS")
#####################################
  IconSet <- awesomeIconList(
    Police   = makeAwesomeIcon(icon= 'building', 
                                markerColor = 'blue', 
                                iconColor = '#000000', #HEX code black
                                library = "fa"),
    # Fire = makeAwesomeIcon(icon= 'fire-extinguisher', 
    #                          markerColor = 'red', 
    #                          iconColor = '#ffffff', #HEX code white
    #                          library = "fa"),
    Hospital = makeAwesomeIcon(icon= 'user', 
                           markerColor = 'green', 
                           iconColor = '#ffffff', #HEX code white
                           library = "fa")
    
  )
######################################

server <- function(input, output) {
  
  output$map <- renderLeaflet({
leaflet(shape) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", R__S__T_P )(R__S__T_P ),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0(shape@data$SA2_N, ": ", shape@data$R__S__T_P ))%>%
 
addAwesomeMarkers(data = DPFEMed, ~X , ~Y ,icon=~IconSet[Agency],
                  label = ~as.character(Description),
                  layerId=~Description, 
                  group = "Agency",
                  popup = ~Agency)
  }) #END renderLeaflet (map)
  
} # END server
#END server.R ##############################################################

