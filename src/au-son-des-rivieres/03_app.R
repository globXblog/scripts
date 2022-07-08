#---------------
# Load required packages and functions
library(ggplot2);library(shiny);library(leaflet);library(shinyWidgets)

# Load data
stations_data <- read.table("France207_MetaData.csv", sep=";", header=TRUE, encoding="UTF-8")

# Create help panel
helpPanel <- modalDialog(
  size="l",
  title = "Bienvenue sur l'app 'Au son des rivières' !",
  tags$p("Cette application permet d'explorer les régimes hydrologiques grâce à de la visualisation et de la sonification de données.
          En cliquant sur une des stations qui mesurent le débit des rivières en France,
          vous pouvez visualiser les données correspondantes et écouter la musique créée à partir de celles-ci.",),
  tags$p("Le schéma ci-dessous explique les séries visualisées ainsi que leur association avec un instrument de musique.
         Pour rouvrir ce panneau plus tard, cliquez sur le bouton d'information en haut à droite."),
  tags$p("Conception: Chloé Le Bescond et Benjamin Renard,",
         HTML("<a href='https://globxblog.inrae.fr'> https://globxblog.inrae.fr </a>")),
  tags$img(src = "helpPanel_fr.png",width="100%"),
  easyClose = TRUE,
  footer =modalButton("Fermer")
)

# Create map
FranceMap <- leaflet(options=leafletOptions(minZoom=6, maxZoom=17))
FranceMap <- addProviderTiles(map=FranceMap,"OpenTopoMap") #   Stamen.Terrain
FranceMap <- addCircles(map=FranceMap, data=stations_data,radius=5000,
                stroke=TRUE, fillOpacity=0.5,
                color="black",fillColor="black",
                lng=~Lon,  # Add points
                lat=~Lat,
                layerId=~ID,
                label=lapply(paste0("<b>Station : </b>", stations_data$River.station, "<br>",
                                    "<b>Code hydro : </b>", stations_data$ID, "<br>",
                                    "<b>Taille du bassin versant : </b>", stations_data[,3], " km²<br>"),
                             htmltools::HTML),
                highlightOptions=highlightOptions(color="red",bringToFront=TRUE)
)

#---------------
# Define UI

ui <- bootstrapPage(

      tags$head(tags$style(type="text/css","html, body {width:100%;height:100%;}")),

      leafletOutput("map", width="100%", height="100%"),
      
      absolutePanel(draggable=TRUE, bottom="2%", left="2%", right="2%",height="33%",
                    htmlOutput(outputId="siteID",width="100%"),
                    htmlOutput(outputId="siteVideo",width="100%"),
      ),
      
      absolutePanel(
        top = 10, right = 10, style = "z-index:500; text-align: right;",
        actionButton(inputId="showHelp", label="Info",
                     icon=icon("info-circle"),class="btn btn-info"),
      )
)

#---------------
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # Information panel
  showModal(helpPanel) # at startup
  observeEvent(input$showHelp,{showModal(helpPanel)}) # when asked

  # Create leaflet map
  output$map <- renderLeaflet ({FranceMap})
  
  # Change clicked site colour
  myLeafletProxy <- leafletProxy(mapId = "map", session)
  observeEvent(input$map_shape_click,{
    clickSite <- input$map_shape_click
    # removeShape(map = myLeafletProxy, layerId = clickSite$id)
    addCircles(map=myLeafletProxy,radius=5000,
               stroke=TRUE, fillOpacity=0.5,
               color="red",fillColor="black",
               lng=clickSite$lng,  # Add points
               lat=clickSite$lat,
               layerId=clickSite$id
    )
  })
  
  # Add site name
  output$siteID <- renderUI ({
    clickSite <- input$map_shape_click$id
    if (!is.null(clickSite)){
      tags$h4(stations_data$River.station[which(stations_data$ID==input$map_shape_click$id)],
              tags$style(type="text/css","h4 {margin-top: 0; margin-bottom: 0;}"))
    }
  })
  
  # Play video
  output$siteVideo <- renderUI ({
    clickSite <- input$map_shape_click$id
    if (is.null(clickSite)==FALSE){
    tags$video(src=paste0(input$map_shape_click$id,".mp4"),
               type="video/mp4", autoplay=NA, controls=NA,width="100%")
    }
  })
}

#---------------
# Call to shiny app
shinyApp(ui=ui, server=server)

