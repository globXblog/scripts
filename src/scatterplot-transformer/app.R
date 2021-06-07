# Load required package and functions
library(shiny)
library(ggplot2)
source("funk.R")
# Uploaded data must be .txt, sep=";"
df <- data.frame(x=rep(seq(1,10),10),
                 y=rep(seq(1,10),each=10))

# Set colour palette
oGridCol <- "#F0F0F0"  # original grid
tGridCol <- "#BDBDBD"  # transformed grid
oPointsCol <- "#C6DBEF"  # original points
tPointsCol <- "blue"  # transformed points

# Shiny interface
ui <- fluidPage(
            
    titlePanel("Scatterplot transformer"),
    
    column(2,
           
           fileInput(inputId="file",
                     label=tags$label(HTML(paste("Select .txt file \n", 
                                                 tags$span(style="color:#99a4ac;font-weight: 300;font-size: 13px;", 
                                                           "Must contain 2 columns named 'x' and 'y' separated by a semicolon (';')"), 
                                                 sep=""))),
                     multiple=FALSE,
                     accept=".txt"),
    
           sliderInput(inputId="centerx", label="x coordinate of center:",
                       min=round(min(df),1),
                       max=round(max(df),1),
                       value=round((max(df$x)-min(df$x))/2+min(df$x),1),
                       step=0.5),
           
           sliderInput(inputId="centery", label="y coordinate of center (folding only):",
                       min=round(max(df),1),
                       max=round(((max(df$y)-min(df$y))*3)+max(df$y),1),
                       value=round(max(df),1)),
            
           sliderInput(inputId="angle",
                       label="Angle [degree]:",
                       min=0, max=360, value=45),
           
           checkboxInput(inputId="ratio", label="x/y aspect ratio = 1", value=FALSE)
           
    ),
    
    column(5,
           plotOutput(outputId="folder", height = "519px"),
           downloadButton("downloadFoldedData", "Download folded data"),
    ),
    
    column(5,
           plotOutput(outputId="bender", height = "519px"),
           downloadButton("downloadBentData", "Download bent data"),
    
    ),

    tags$style(HTML("
     .btn-default { float: right;}
    ")),
    
)

server <- function(input, output, session) {
    
    # Select df to be transformed
    data <- reactive({
        if (is.null(input$file$datapath)){
            df <- df
            
        } else {
            df <- read.table(file=input$file$datapath, header=TRUE, sep=";")
        }
        return(df)
    })
    
    # Update values of sliders based on data
    observeEvent(input$file, {
        
        # Update centerx slider
        dataminx <- min(data()$x)
        datamaxx <- max(data()$x)
        slidervaluex <- ((datamaxx-dataminx)/2)+dataminx
        sliderstepx <- (datamaxx-dataminx)/18
        updateSliderInput(session, "centerx", 
                          min=round(dataminx, 2),
                          max=round(datamaxx,2), 
                          value=round(slidervaluex,2),
                          step=round(sliderstepx,2))
        
        # Update centery slider
        dataminy <- min(data()$y)
        datamaxy <- max(data()$y)
        slidermaxy <- ((datamaxy-dataminy)*3)+datamaxy
        slidervaluey <- mean(c(datamaxy, slidermaxy))
        sliderstepy <- (slidermaxy-datamaxy)/18
        updateSliderInput(session, "centery", 
                          min=round(datamaxy,2),
                          max=round(slidermaxy,2), 
                          value=round(datamaxy,2),
                          step=round(sliderstepy,2))
    })
    
    # Create a background "grid" to better understand how data are folded and bent
    customGrid <- reactive({
        minx <- min(data()$x)
        miny <- min(data()$y)
        maxx <- max(data()$x)
        maxy <- max(data()$y)
        meanx <- mean(c(minx,maxx))
        meany <- mean(c(miny,maxy))
        grid <- data.frame(x=c(seq(minx,maxx,length.out=100),   #bottom
                               seq(minx,maxx,length.out=100),   #top
                               rep(minx,100),   #left
                               rep(maxx,100),   #right
                               seq(minx,maxx,length.out=100),   #horizontal
                               rep(meanx,100)),   #veryical
                           y=c(rep(miny,100),   #bottom
                               rep(maxy,100),   #top
                               seq(miny,maxy,length.out=100),    #left
                               seq(miny,maxy,length.out=100),   #right
                               rep(meany,100),   #horizontal
                               seq(miny,maxy,length.out=100)))   #vertical
        return(grid)
    })
    
    # Compute folded data
    foldingData <- reactive({
        center <- c(input$centerx,input$centery)
        angle <- pi*(input$angle)/180
        # Fold scatterplot and grid
        foldingScatterplot <- foldScatterplot(data()$x,data()$y,center,angle)
        foldingGrid <- foldScatterplot(customGrid()$x,customGrid()$y,center,angle)
        return(list(scatterplot=foldingScatterplot,
                    grid=foldingGrid,
                    center=center))
    })
    
    # Plot folded data
    output$folder <- renderPlot({
        # Plot folded scatterplot and grid
        grid <- foldingData()$grid
        scatterplot <- foldingData()$scatterplot
        center <- foldingData()$center
        g1 <- ggplot()+
                geom_point(data=grid, aes(x=x,y=y), color=oGridCol, size=0.2, alpha=0.5)+  # original grid
                geom_point(data=grid, aes(x=tx,y=ty), color=tGridCol, size=0.2)+  # folded grid
                geom_point(data=scatterplot, aes(x=x,y=y), color=oPointsCol, alpha=0.5)+  # original points
                geom_point(data=scatterplot, aes(x=tx,y=ty), color=tPointsCol)+  # folded points
                geom_point(aes(x=center[1],y=center[2]), color="red", size=2)+  # center
                theme_bw()+
                labs(title="Folding")+
                theme(panel.grid = element_blank(),
                      plot.title = element_text(size=16),
                      axis.title = element_text(size=14),
                      axis.text = element_text(size=10))
        # Add fixed ratio to plot if checkbox is ticked
        if(input$ratio==TRUE){
            g1+coord_fixed(ratio=1)
        } else {
            g1
        }
    })
    
    # Compute bent data
    bendingData <- reactive({
        xmin <- min(data()$x)
        xmax <- max(data()$x)
        pivot <- (input$centerx-xmin)/(xmax-xmin)
        xmin2 <- min(customGrid()$x)
        xmax2 <- max(customGrid()$x)
        pivot2 <- (input$centerx-xmin2)/(xmax2-xmin2)
        angle <- pi*(input$angle)/180
        center <- input$centerx
        # Bend scatterplot and grid
        bendingScatterplot <- bendScatterplot(data(), pivot=pivot, angle=angle)
        bendingGrid <- bendScatterplot(customGrid(),pivot=pivot2,angle=angle)
        return(list(scatterplot=bendingScatterplot,
                    grid=bendingGrid))
    })
    
    
    # Plot bent data
    output$bender <- renderPlot({
        # Plot bent scatterplot and grid
        grid <- bendingData()$grid
        scatterplot <- bendingData()$scatterplot
        g2 <- ggplot()+
                geom_point(data=grid, aes(x=x,y=y), color=oGridCol, size=0.2)+  # original grid
                geom_point(data=grid, aes(x=tx,y=ty), color=tGridCol, size=0.2)+  # bent grid
                geom_point(data=scatterplot, aes(x=x,y=y), color=oPointsCol)+  # original points
                geom_point(data=scatterplot, aes(x=tx,y=ty), color=tPointsCol)+  # bent points
                geom_point(aes(x=input$centerx,y=max(bendingData()$grid$y)), color="red", size=2)+  # pivot
                theme_bw()+
                labs(title="Bending")+
                theme(panel.grid = element_blank(),
                      plot.title = element_text(size=16),
                      axis.title = element_text(size=14),
                      axis.text = element_text(size=10))
        # Add fixed ratio to plot if checkbox is ticked
        if(input$ratio==TRUE){
            g2+coord_fixed(ratio=1)
        } else {
            g2
        }
    })
    
    # Dowloadable .txt of folded data
    output$downloadFoldedData <- downloadHandler(
        filename = function() {
            if (is.null(input$file)){
                paste("demo", "_folded.txt", sep="")
            } else {
                paste(substr(input$file, 1, nchar(input$file)-4), "_folded.txt", sep = "")
            }
        },
        content = function(file) {
            write.table(foldingData()$scatterplot, file, sep=";",row.names = FALSE)
        }
    )
    
    # Dowloadable .txt of bent data
    output$downloadBentData <- downloadHandler(
        filename = function() {
            if (is.null(input$file)){
                paste("demo", "_bent.txt", sep="")
            } else {
                paste(substr(input$file, 1, nchar(input$file)-4), "_bent.txt", sep = "")
            }
        },
        content = function(file) {
            write.table(bendingData()$scatterplot, file, sep=";",row.names = FALSE)
        }
    )

}

#---------------
# Call to shiny app
shinyApp(ui=ui, server=server)


