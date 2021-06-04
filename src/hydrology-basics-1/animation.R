#---------------
# Create movie from Sauze data
# for the blog

#---------------
# Load required data and packages
library(tidyr);library(ggplot2);library(patchwork);library(av)

#---------------
# Read data
rawData <- read.table('SauzeData.txt',header=T)

# Set number of days to be displayed in animation and which one is turned on
ndays <- 30
dayOn <- 10

# Set colour palette
PColour <- "#004369"   # Precipitation, light blue
QColour <- "#01949A"   # Streamflow, blue
PETColour <- "#DB1F48"   # Evaporanspiration, green
offColour <- "#EAEAE0"   # Unplayed data
insetColour <- "#F8F8F5"   # Inset background colour
areaColour <- "#2E2E2E"   # Inset graph area colour

# # Create folder to store image for animation
wd <- getwd()
unlink('animationImages',recursive=TRUE);dir.create('animationImages')

#---------------
# INSET PLOT

# Function to create inset
getInsetPlot <- function(data, yAxisLim,areaColour="#171111",insetColour="#F8F8F5"){
  # Duplicate data
  df <- data
  # Add column in df for x axis
  m <- NROW(df)   # Number of data
  df$anim_state <- seq(1,m)
  # Plot inset
  g <- ggplot2::ggplot()+geom_area(data=df, aes(x=anim_state, y=df[,4]), fill=areaColour)+
                scale_x_continuous(limits=c(-0.5,(m+0.5)),expand=c(0,0))+
                scale_y_continuous(limits=c(0,yAxisLim),expand=c(0,0))+
                theme(axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title=element_blank(),
                      panel.grid=element_blank(),
                      panel.background=element_rect(fill=insetColour))
  return(g)
}

# Get inset plots for each variable
Pdata <- rawData[,c(1,2,3,5)]
PInset <- getInsetPlot(data=Pdata, yAxisLim=max(Pdata$P)*1.06, areaColour=areaColour,insetColour=insetColour)

Qdata <- rawData[,c(1,2,3,4)]
QInset <- getInsetPlot(data=Qdata, yAxisLim=max(Qdata$Q)*1.06, areaColour=areaColour,insetColour=insetColour)

PETdata <- rawData[,c(1,2,3,6)]
PETInset <- getInsetPlot(data=PETdata, yAxisLim=max(PETdata$PET)*1.06, areaColour=areaColour,insetColour=insetColour)

#---------------
# CREATE PLOT WITH 3 VARIABLES AND INSETS

# Function to create individual plot with inset
getBarplotWithInset <- function(data,inset,i=1,yAxisLim, yAxisTitle="", ndays=30,dayOn=10,
                                onColour="red",offColour="#EAEAE0"){
  m <- NROW(data)
  if (i>(m+dayOn)){print(paste("i should be <",(m+dayOn),"(number of rows in data + dayOn value)"))}
  # Create empty rows at the beginning and at the end for animation
  nFakeEndData <- (ndays-dayOn+1)
  dfStart <- data[1:(dayOn-1),]
  dfStart[,seq(1,4)] <- 0
  dfEnd <- data[1:nFakeEndData,]
  dfEnd[,seq(1,4)] <- 0
  df <- rbind(dfStart,data,dfEnd)
  # Add column in df for x axis
  n <- NROW(df)
  df$anim_state <- seq(1,n)
  # Calculate coordinates for geom_polygon in insets (coloured stripe)
  if (i<(dayOn+1)){
    xStart <- 0.5} else {
    xStart <- i-dayOn+0.5}
  if (i>(m-nFakeEndData+1)){
    xEnd <- (m+0.5)} else {
    xEnd <- i+(ndays-dayOn)+0.5}
  # Add coloured stripe on inset
  insetGrob <- inset+geom_polygon(aes(x=c(xStart,xEnd,xEnd,xStart),
                                  y=c(-Inf,-Inf,Inf,Inf)),fill=onColour, alpha=0.3)+
                     geom_polygon(aes(x=c((i-0.5),(i+0.5),(i+0.5),(i-0.5)),
                                      y=c(-Inf,-Inf,Inf,Inf)),fill=onColour, alpha=0.5)
  
  # Create colour vector for geom_col
  barColours <- c(rep(offColour,(dayOn-1)),onColour,rep(offColour,(ndays-dayOn)))
  # Get data to be plotted from df
  plotStart <- i
  plotEnd <- i+(ndays-1)
  mask <- df[df$anim_state%in%seq(plotStart,plotEnd),] 
  # Create plot
  p <- ggplot2::ggplot()+geom_col(data=mask,aes(x=anim_state, y=mask[,4], fill=as.factor(anim_state)))+
            scale_x_continuous(limits=c((i-0.5),(i+ndays-0.5)),expand=c(0,0))+
            scale_y_continuous(limits=c(0,yAxisLim),expand=c(0,0))+
            annotation_custom(grob=ggplotGrob(insetGrob),
                              # xmin=(i+ndays-dayOn-1),xmax=Inf,ymin=(yAxisLim*2/3),ymax=Inf)+
                              xmin=(i+ndays-dayOn-0.5),xmax=Inf,ymin=(yAxisLim*2/3),ymax=Inf)+
            scale_fill_manual(values=barColours)+
            theme_bw()+
            theme(legend.position="none",
                  panel.grid=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),                    
                  axis.title.x=element_text(size=70, colour="transparent"),
                  axis.text.y=element_text(size=90,margin=margin(r=20)),
                  axis.title.y=element_text(size=102),
                  axis.ticks.length.y=unit(20, "pt"))+
            labs(x="Year", y=yAxisTitle)
  return(p)
}

m <- NROW(rawData)
for (i in 1:(m+dayOn)){
  # Get inset plots for each variable
  Pplot <- getBarplotWithInset(data=Pdata,inset=PInset,i=i,yAxisLim=max(Pdata[,4])*1.06,
                               yAxisTitle="Precipitation [mm]",ndays=ndays,dayOn=dayOn,
                               onColour=PColour,offColour=offColour)
  Qplot <- getBarplotWithInset(data=Qdata,inset=QInset,i=i,yAxisLim=max(Qdata[,4])*1.06,
                               yAxisTitle="Streamflow [mm]",ndays=ndays,dayOn=dayOn,
                               onColour=QColour,offColour=offColour)
  PETplot <- getBarplotWithInset(data=PETdata,inset=PETInset,i=i,yAxisLim=max(PETdata[,4])*1.06,
                                 yAxisTitle="Pot. evapotranspiration [mm]",ndays=ndays,dayOn=dayOn,
                                 onColour=PETColour,offColour=offColour)
  # Assemble individual plots
  allPlots <- Pplot/Qplot/PETplot
  if (i<=m){
    plotTitle <- paste0(sprintf("%02d", rawData$day[i]),"/",sprintf("%02d", rawData$month[i]),"/",rawData$year[i])
  } else {
    plotTitle <- ""
  }
  allPlots <- allPlots+plot_annotation(title=plotTitle) & 
    theme(plot.title=element_text(size=160, hjust=0.33))
  png(filename=paste0(wd, "/animationImages/",sprintf("im_%03d", i),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(allPlots)
  dev.off()
  print(paste0(i,"/",m))
}

#---------------
# Create first image (i=1) without onColours (onColour is offColour)
for (i in 1:1){
  # Get inset plots for each variable
  Pplot <- getBarplotWithInset(data=Pdata,inset=PInset,i=i,yAxisLim=max(Pdata[,4])*1.06,
                               yAxisTitle="Precipitation [mm]",ndays=ndays,dayOn=dayOn,
                               onColour=offColour,offColour=offColour)
  Qplot <- getBarplotWithInset(data=Qdata,inset=QInset,i=i,yAxisLim=max(Qdata[,4])*1.06,
                               yAxisTitle="Streamflow [mm]",ndays=ndays,dayOn=dayOn,
                               onColour=offColour,offColour=offColour)
  PETplot <- getBarplotWithInset(data=PETdata,inset=PETInset,i=i,yAxisLim=max(PETdata[,4])*1.06,
                                 yAxisTitle="Pot. evapotranspiration [mm]",ndays=ndays,dayOn=dayOn,
                                 onColour=offColour,offColour=offColour)
  # Assemble individual plots
  allPlots <- Pplot/Qplot/PETplot
  plotTitle <- paste0(sprintf("%02d", rawData$day[i]),"/",sprintf("%02d", rawData$month[i]),"/",rawData$year[i])
  allPlots <- allPlots+plot_annotation(title=plotTitle) & 
    theme(plot.title=element_text(size=160, hjust=0.33))
  png(filename=paste0(wd, "/animationImages/",sprintf("im_%03d", (i-1)),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(allPlots)
  dev.off()
  print(paste0(i,"/",m))
}

#---------------
# Create inset enlargement

# Create function to enlarge inset
getInsetEnlargement <- function(inset,yAxisLim, yAxisTitle="",zoomFactor=1){
  # Create plot
  p <- ggplot2::ggplot()+geom_col()+
    scale_x_continuous(limits=c((0.5),(30+0.5)),expand=c(0,0))+
    scale_y_continuous(limits=c(0,yAxisLim),expand=c(0,0))+
    theme_bw()+
    theme(legend.position="none",
          panel.grid=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),                    
          axis.title.x=element_text(size=70, colour="transparent"),
          axis.text.y=element_text(size=90,margin=margin(r=20)),
          axis.title.y=element_text(size=102),
          axis.ticks.length.y=unit(20, "pt"))+
    labs(x="Year", y=yAxisTitle)+
    annotation_custom(grob=ggplotGrob(inset),
                      xmin=((30*(1-zoomFactor))+0.5),xmax=Inf,ymin=(yAxisLim*(1-zoomFactor)),ymax=Inf)
  return(p)
}

# Set number of images to be created
nLast <- 50

# Zoom factors
zf <- seq((1/3), 1, length.out=nLast)
for (i in 1:nLast){
  zoomFactor <- zf[i]
  # Get inset plots for each variable
  Pplot <- getInsetEnlargement(inset=PInset,yAxisLim=max(Pdata[,4])*1.06,
                               yAxisTitle="Precipitation [mm]",zoomFactor=zoomFactor)
  Qplot <- getInsetEnlargement(inset=QInset,yAxisLim=max(Qdata[,4])*1.06,
                               yAxisTitle="Streamflow [mm]",zoomFactor=zoomFactor)
  PETplot <- getInsetEnlargement(inset=PETInset,yAxisLim=max(PETdata[,4])*1.06,
                                 yAxisTitle="Pot. evapotranspiration [mm]",zoomFactor=zoomFactor)
  # Assemble individual plots
  allPlots <- Pplot/Qplot/PETplot
  plotTitle <- ""
  allPlots <- allPlots+plot_annotation(title=plotTitle) & 
    theme(plot.title=element_text(size=160, hjust=0.33))
  png(filename=paste0(wd, "/animationImages/",sprintf("im_%03d", (m+dayOn+i)),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(allPlots)
  dev.off()
  print(paste0(i,"/",length(zf)))
}
i=7

#---------------
# Create animation
lf <- list.files("animationImages", full.names=TRUE)  # List all images
k <- length(lf)

# Duplicate first and last images to stick to the music
# length(lf2) must be 900
nStart <- (14*4)-1
lf2 <- c(rep(lf[1],nStart),lf,rep(lf[k],(900-(55+m+dayOn+nLast+1))))

av_encode_video(input=lf2,
                audio="Sauze.mp3",
                output="Sauze.mp4",
                framerate=(16/3))

