#---------------
# Load required packages and functions
library(ggplot2);library(shiny);library(leaflet);library(shinyWidgets);library(dplyr)

# Load data
load('data/time.RData')  # dates
load("data/space.RData")   # coordinates
load('data/values.RData')   # values to be plotted

# Set variable indices, names and units
var1_index <- 4   # index of variable in "values" to be plotted on the map (circle size)
var1_name <- "Return period"
var1_unit <- "Year"
var2_index <- 3   # index of variable in "values" to be plotted on the bottom left corner
var2_name <- "Discharge"
var2_unit <- "m\U00B3/s"

# Set titles
appTitle <- "Flood occurences in the World, 1903-2016"   # Title of the whole app
legendTitle <- "Flood occurence"   # Title of the map legend

# Set source info
sourceLink <- "https://doi.pangaea.de/10.1594/PANGAEA.887470"
sourceText <- "The Global Streamflow Indices and Metadata Archive"

# Set text for slider2
slider2_text <- c(seq(1,9,1),seq(10,90,10),seq(100,1000,100))

# Set x axis breaks for plots
yearMin <- floor(min(time$TIME))
yearMax <- ceiling(max(time$TIME))
plotXBreaks <- seq(yearMin,yearMax, length.out=7)

#---------------
# List all sites and their coordinates. This will be the dataframe to be plotted for the map
dataToMap <- data.frame(SPACEID=space$SPACEID,
                        lon=space$X,
                        lat=space$Y,
                        var1=NA,   # variable to be plotted on the map (circles)
                        var2=NA,   # variable to be plotted on the bottom left corner
                        circleColour="noData",
                        circleSize=0.8,
                        var1Text="No data",   # Info about var1 to be displayed in popup (2nd line)
                        var2Text="No data")   # Info about var2 to be displayed in popup (3rd line)

# Count nb of sites by time step
nbSitesbyTimestep <- group_by(values,TIMEID)
nbSitesbyTimestep <- summarise(nbSitesbyTimestep,nData=n())
nbSitesbyTimestep <- left_join(nbSitesbyTimestep,time) # Add "real" time column

# Text for slider 1 (time)
slider1_text <- time$LABEL

# Create a palette that maps factor levels to colors
pal <- colorFactor(c("black", "red", "lightgrey"), domain=c("lessThanThreshold", "moreThanThreshold", "noData"))

#---------------
# Define UI
ui <- fluidPage(
  fluidRow(
    
    titlePanel(appTitle),
    
    column(6,
           sliderTextInput(inputId="selectedSlider1",
                           label=tags$label(HTML(paste("Select time ", 
                                                       tags$span(style="color:#99a4ac;font-weight: 100;", 
                                                                 "or press \U25B6 below to animate map over the whole period"), 
                                                       sep=""))),
                           choices=slider1_text,
                           selected=slider1_text[265],
                           grid=TRUE,
                           animate=animationOptions(interval=2000)) # in milliseconds
    ),

    column(6,
           sliderTextInput(inputId="selectedSlider2",
                           label=paste0("Select threshold: ", tolower(var1_name), " [",tolower(var1_unit), "]"),
                           choices=slider2_text,
                           selected=slider2_text[10],
                           grid=TRUE),
    ),
  ),
  
  fluidRow(
    column(4,

           textOutput("nbVisibleSites"),
           
           plotOutput("nbSites", height=130),
           
           textOutput("percentageFrequencyOccurence"),
           
           plotOutput("frequencyOccurence", height=130),
           
           fluidRow(
              column(9,
                     h5(htmlOutput("clickedSite"))),
              column(3,checkboxInput(inputId="logy", label="Y-axis log?", value=FALSE)),
           ),
           
           plotOutput("var2", height=232),
           
           ),
    
    column(8,
           leafletOutput(outputId="map", width="100%", height="550px"),
           
           tags$p("Data source: ",
                  tags$a(href=sourceLink, sourceText),
                  style="font-size: 14px;margin-top: 10px;margin-bottom: 0px;text-align: right;"),
    ),
  ),
    
    tags$style(HTML("
    .shiny-input-container:not(.shiny-input-container-inline) { width: 100%;}
    h2 { margin-left: 10px; }
    h5 { margin-bottom: 5px; }
    .col-sm-6 { padding-right: 30px;padding-left: 30px;margin-top:5px;  margin-bottom: 5px; }
    label.control-label { margin-left: 10px; }
    .help-block {color: #99a4ac;margin-bottom: 5px;margin-top: 0px;}
    .checkbox { margin-top:5px;  margin-bottom: 5px;}
    .checkbox label { color: #99a4ac;}
    .shiny-input-container:not(.shiny-input-container-inline) {margin-top:5px;  margin-bottom: 5px;}
    ")),
    
)

#---------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Transform selected time into numeric
  numericSelectedTime <- reactive({
    
    selected_time_label <- input$selectedSlider1
    selected_NumTime <- as.numeric(time$TIME[which(time$LABEL==selected_time_label)])
    
    return(selected_NumTime)
    
  })
  
  # Subset data based on selected time
  mapData <- reactive({

    # Time index corresponding to selected time
    t_index <- time$TIMEID[which(time$TIME==numericSelectedTime())]
    selectedDF <- values[values$TIMEID==t_index,]
    n_selectedDF <- NROW(selectedDF)   # Number of data corresponding to selected time

    # Fill dataToMap
    if(n_selectedDF!=0){
      for (k in 1:n_selectedDF){
        kSite <- as.character(selectedDF$SPACEID[k])   # k-th site in selectedDF
        targetRow <- which(dataToMap$SPACEID==kSite)   # row containing kSite in dataToMap
        targetVar1 <- as.numeric(selectedDF[k, var1_index])   # k-th var1_index in selectedDF
        dataToMap$var1[targetRow] <- targetVar1   # Add targetVar1 to dataToMap
        dataToMap$circleSize[targetRow] <- targetVar1   # Add targetVar1 to dataToMap
        dataToMap$var2[targetRow] <- as.numeric(selectedDF[k, var2_index])  # Add targetValue2 to dataToMap
        # get circle colour based on selectedSlider2
        if (targetVar1<as.numeric(input$selectedSlider2)){
          dataToMap$circleColour[targetRow] <- "lessThanThreshold"
        }
        if(targetVar1>=as.numeric(input$selectedSlider2)){
          dataToMap$circleColour[targetRow] <- "moreThanThreshold"
        }
        # get popup text
        if (tolower(var1_unit)=="year") {
          if(round(selectedDF[k, var1_index]) <= 1){
            targetVal1Text <- paste0(" ", tolower(var1_unit))
          } else {
            targetVal1Text <- paste0(" ", tolower(var1_unit), "s")
          }
        } else {
          targetVal1Text <- paste0(" ", tolower(var1_unit))
        }
        dataToMap$var1Text[targetRow] <- paste(round(selectedDF[k,var1_index]), targetVal1Text)
        dataToMap$var2Text[targetRow] <- paste(round(selectedDF[k,var2_index]), var2_unit)
      }
    }

    # Sort dataToMap by selected slider 2 so bigger points are on top
    dataToMap <- dataToMap[order(-dataToMap$var1, decreasing=TRUE, na.last=FALSE), ]

    return(dataToMap)

  })

  # Plot leaflet base map
  output$map <- renderLeaflet ({
    m <- leaflet(dataToMap,
                 options=leafletOptions(minZoom=2))
    m <- fitBounds(m, lat1=80, lng2=190, lat2=-60, lng1=-170)
    m <- addTiles(map=m)
    m <- addLegend(map=m, "bottomleft",
                   title=legendTitle,
                   colors=c("black", "red", "lightgrey"),
                   labels=c("< threshold", "> threshold", "No data"),
                   opacity=0.8)

  })

  # Add subsetted data (mapData())to leaflet map without reloading the bacKground map
  observe({
    m <- leafletProxy(mapId="map",data=mapData())
    m <-  clearMarkers(map=m)
    popTxt_time <- paste0(input$selectedSlider1,": ")

    m <-  addCircles(map=m,data=mapData(),
                     lng=~lon, lat=~lat,
                     weight=1,
                     radius=~log(circleSize)*30000,
                     color=~pal(circleColour),
                     stroke=FALSE,
                     fillOpacity=0.4,
                     layerId=~SPACEID,
                     popup=paste0("<b>Site ID: </b>", mapData()$SPACEID, "<br>",
                                  "<b>",var1_name, " in ",popTxt_time,"</b>", mapData()$var1Text, "<br>",
                                  "<b>",var2_name, " in ",popTxt_time,"</b>", mapData()$var2Text, "<br>"))
    })

  # Find visible sites depending on map zoom
  visibleSites <- reactive({

    # Get map bounds
    Nbound <- input$map_bounds$north
    Ebound <- input$map_bounds$east
    Sbound <- input$map_bounds$south
    Wbound <- input$map_bounds$west

    # Subset "space" to get visible sites only
    visibleSites <- space[space$X>Wbound & space$X<Ebound & space$Y<Nbound & space$Y>Sbound, ]

    visibleSites <- values[values$SPACEID %in% (visibleSites$SPACEID),]
    visibleSites$moreThanThreshold <- ifelse(visibleSites[,var1_index]>=input$selectedSlider2,1,0)
    visibleSites <- group_by(visibleSites,TIMEID)

    return(visibleSites)
  })

  # Subset values to get visible sites
  nbVisibleSitesbyTimestep <- reactive({
    nbVisibleSitesbyTimestep <- summarise(visibleSites(),nData=n())
    # Add "real" time column to nbVisibleSitesbyTimestep
    nbVisibleSitesbyTimestep <- left_join(nbVisibleSitesbyTimestep,time)
    return(nbVisibleSitesbyTimestep)
  })

  # Number of sites for selected time
  output$nbVisibleSites <- renderText ({
    nb <- nbVisibleSitesbyTimestep()$nData[nbVisibleSitesbyTimestep()$TIME==numericSelectedTime()]
    paste0("Number of sites (visible & available at selected time) : ",nb)
  })

  # Plot number of visible sites by timestep
  output$nbSites <- renderPlot({

    # Plot nbVisibleSitesbyTimestep
    ggplot()+
      geom_vline(xintercept=numericSelectedTime(), linetype="dashed")+
      geom_area(data=nbSitesbyTimestep, aes(x=TIME,y=nData), fill="#428bca", alpha=0.2)+
      geom_line(data=nbVisibleSitesbyTimestep(), aes(x=TIME,y=nData))+
      scale_x_continuous(limits=c(yearMin,yearMax),
                         expand=c(0.01,0.01),
                         breaks=plotXBreaks)+
      labs(x="Time", y=NULL)+
      theme_bw()+
      theme(legend.position="none")
  })

  freqOccurenceByTimestep <- reactive({
    freqOccurenceByTimestep <- summarise(visibleSites(),
                                         freq=mean(moreThanThreshold))
    # Add "real" time column to freqOccurenceByTimestep
    freqOccurenceByTimestep <- left_join(freqOccurenceByTimestep,time)

    return(freqOccurenceByTimestep)
  })

  # Plot frequency of occurence by year for selected RP
  output$frequencyOccurence <- renderPlot({

    ggplot()+
      geom_vline(xintercept=numericSelectedTime(), linetype="dashed")+
      geom_line(data=freqOccurenceByTimestep(),aes(x=TIME, y=freq*100))+
      scale_x_continuous(limits=c(yearMin,yearMax),
                         expand=c(0.01,0.01),
                         breaks=plotXBreaks)+
      ylim(0,NA)+
      labs(x="Time", y=NULL)+
      theme_bw()+
      theme(legend.position="none")
  })

  # Find frequency of occurence for selected time
  output$percentageFrequencyOccurence <- renderText ({

    pct <- round(freqOccurenceByTimestep()$freq[freqOccurenceByTimestep()$TIME==numericSelectedTime()]*100)
    paste0("Frequency of occurence at selected time and threshold: ",pct, "%")

  })

  # Plot variable2 time-series for selected site
  output$var2 <- renderPlot({

    clickSite <- input$map_shape_click$id

    if (is.null(clickSite)==FALSE){
      # Subset "values" based on selected site
      var2_df <- visibleSites()[visibleSites()$SPACEID==clickSite,]
      var2_df <- var2_df[,c(1,2,var2_index, which(colnames(var2_df)=="moreThanThreshold"))]
      colnames(var2_df) <- c("TIMEID","SPACEID","VAR2","moreThanThreshold")

      # Add "real" time column to var2_df
      var2_df <- left_join(var2_df,time)

      # Plot subsetted dataframe
      p <- ggplot()+
        geom_vline(xintercept=numericSelectedTime(), linetype="dashed")+
        geom_point(data=var2_df,
                   aes(x=TIME, y=VAR2, colour=as.factor(moreThanThreshold)),
                   size=3, alpha=0.5)+
        scale_colour_manual(breaks=c("0", "1"),
                            values=c("black", "red"))+
        scale_x_continuous(limits=c(yearMin,yearMax),
                           expand=c(0.01,0.01),
                           breaks=plotXBreaks)+
        labs(x="Time", y=NULL)+
        theme_bw()+
        theme(legend.position="none")
      
      if(input$logy==TRUE){
        p+scale_y_continuous(trans='log10')
      } else {
        p
      }
    }
  })

  # Get title for variable2 plot
  output$clickedSite <- renderText({
    clickSite <- input$map_shape_click$id
    if (is.null(clickSite)){
      paste("<span style=\"color:#99a4ac\">Click a circle to display corresponding", tolower(var2_name) ,"time-series</span>")
    } else {
      paste0("<span>", var2_name ," at ", clickSite, " [",var2_unit, "]</span>")
    }
  })

}

#---------------
# Call to shiny app
shinyApp(ui=ui, server=server)
