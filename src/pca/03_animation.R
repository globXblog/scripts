#---------------
# Load required data and packages
library(tidyr);library(ggplot2);library(patchwork);library(av)

#---------------
# Read data
PCs0 <- read.table('Components.txt',header=TRUE)
pcdates <- read.table('dates.txt',header=FALSE)
QM <- read.table('QM.txt',header=F)   # rows: dates, col: stations
France207 <- read.table("France207.txt",header=T)   # station coordinates

# Load function
source("getFunkPlotsWithZoom.R")
source("getEmptyMap.R")

# Create folder to store image for animation
wd <- getwd()
unlink('anim_im',recursive=TRUE);dir.create('anim_im')
unlink('anim_imStart',recursive=TRUE);dir.create('anim_imStart')
world <- map_data("world")   # Background map

#---------------
# MAP DATA

# Get normal scores
QMNS <- QM
for (i in 1:ncol(QM)){
  r <- rank(QM[,i],na.last='keep',ties.method='random')
  p <- (r-0.5)/sum(!is.na(r))
  QMNS[,i] <- qnorm(p)
}

# Rearrange QMNS
n <- NROW(QMNS)   # Number of frames (number of months)
QMNS_map <- cbind(pcdates, seq(1:n),QMNS)   # Add dates and anim_state columns to QM
colnames(QMNS_map) <- c("year","month","anim_state",as.character(France207$ID))
mapDF <- pivot_longer(data=QMNS_map, cols=seq(4,210),names_to="station")

# Add station coordinates to mapDF
mapDF$x <- NA
mapDF$y <- NA

for (i in 1:NROW(mapDF)){
  site <- mapDF$station[i]
  target_row <- which(France207$ID==site)
  mapDF$x[i] <- France207$X[target_row]
  mapDF$y[i] <- France207$Y[target_row]
}

#---------------
#  GRAPH DATA (PCs)

# PCA
QMnoMV <- QMNS
for (i in 1:ncol(QM)){
  QMnoMV[is.na(QMNS[,i]),i] <- 0
}
PCval <- prcomp(x=QMnoMV,center=T,scale=F)

# PC1 and PC2 are the first 2 rows in PCs
PCs <- PCs0[,c(1,2)]
# Add dates to PCs
PCs <- cbind(pcdates, PCs)
colnames(PCs) <- c("year","month", "PC1_original", "PC2_original")
# Add column for order in animation
n <- NROW(PCs)   # Number of frames (number of data)
PCs$anim_state <- seq(1:n)
# Reverse PCs values to stick to the sonification parts
PCs$PC1_reverse <- (PCs$PC1_original)*(-1)
PCs$PC2_reverse <- (PCs$PC2_original)*(-1)

#---------------
# PLOT

# Get plot limits
dayOn <- 4
lim <- c(min(mapDF$value,na.rm=T),max(mapDF$value,na.rm=T))

# Set map colour palette
mapBgColour <- "#E6E6E6"
mapPtColours <- c("#FF8210",alpha("#fcd57f",0.5),"#445B2A")
offColour <- "transparent"
areaOnColour <- "#A1C0BA"   # light aqua
onColour1 <- "#C9DBD8"   # light aqua
onColour2 <- "#A1C0BA"   # light aqua
onColour3 <- "#82ACA4"   # light aqua

# Create function to plot map
getMap <- function(data, xIndex, yIndex, valueIndex, lim,
                   plotTitle,plotSubtitle,legendTitle,mapText,
                   mapPtColours,mapBgColour="#E6E6E6") {
  
  # Plot
  world <- ggplot2::map_data("world")
  g <- ggplot2::ggplot()+
    geom_polygon(data=world[world$region=="France",],aes(long,lat,group=group),fill=mapBgColour,size=0)+
    geom_point(data=data,aes(x=data[,xIndex],y=data[,yIndex],
                             colour=data[,valueIndex], size=is.na(data[,valueIndex])))+
    annotate("text", x=-3.8, y=51.7, label=mapText, size=50)+
    scale_colour_gradientn(colours=mapPtColours,na.value="#B3B3B3",
                           limits=lim,breaks=(lim*0.8),labels=c("Low", "High"))+
    scale_size_manual(values=c(40,20))+   # Size for NA values is 1.5
    coord_map(xlim=c(-4.4,9.6),ylim=c(39.9,51.7),projection="mercator")+
    theme_bw()+
    theme(plot.title=element_text(size=170),
          plot.subtitle=element_text(size=140),
          panel.border=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          legend.position=c(0.5,0.065),
          legend.direction="horizontal",
          legend.title=element_text(size=110),
          legend.key.size=unit(70,'mm'),
          legend.text=element_text(size=86,colour="grey30"))+
    guides(colour=guide_colorbar(title=legendTitle, title.position="top",ticks=FALSE), 
           size="none")+
    labs(title=plotTitle,subtitle=plotSubtitle)
  return(g)
}

for (i in 1:(n+dayOn)){
  # Plot map
  DF <- data.frame(mapDF[mapDF$anim_state==i,])
  if (is.na(as.numeric(DF[1,2]))){
    mapText <- ""
  } else {
  mapText <- paste0(sprintf("%02d", as.numeric(DF[1,2])),"/",as.numeric(DF[1,1]))    
  }
  mapPlot <- getMap(data=DF, xIndex=6, yIndex=7, valueIndex=5, lim=lim,
                    plotTitle="Monthly streamflow in France",
                    plotSubtitle="and first two principal components",
                    legendTitle="Streamflow",
                    mapText=mapText,
                    mapPtColours=mapPtColours,mapBgColour=mapBgColour)
  # Plot graphs
  PC1Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=6,ndata=12,dayOn=dayOn,i=i,
                                  plotTitle="First principal component",
                                  reverseY=TRUE,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1=onColour1,onColour2=onColour2,onColour3=onColour3,
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourColourOn="transparent", contourColourOff=onColour1)
  PC2Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=4,ndata=12,dayOn=dayOn,i=i,
                                  plotTitle="Second principal component",
                                  reverseY=TRUE,
                                  # data=PCs,xIndex=5,yIndex=4,ndata=12,dayOn=dayOn,i=i,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1=onColour1,onColour2=onColour2,onColour3=onColour3,
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourColourOn="transparent", contourColourOff=onColour1)
  # Assemble map and graphs
  leftHand <- wrap_elements(mapPlot)
  rightHand <- wrap_elements((PC1Plot/plot_spacer()/PC2Plot)+plot_layout(heights=c(12,1,12)))
  png(filename=paste0(wd, "/anim_im/",sprintf("im_%03d", i),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(leftHand+rightHand+plot_layout(ncol=2,widths=c(43,57))&
      theme(plot.margin=margin(t=2,b=2,l=2,r=2, unit="cm")))
  dev.off()
  print(paste0(i,"/",n))
}


#---------------
# Create first image without onColours (onColour is offColour)
for (i in 1:1){
  # Plot map
  DF <- data.frame(mapDF[mapDF$anim_state==i,])
  if (is.na(as.numeric(DF[1,2]))){
    mapText <- ""
  } else {
    mapText <- paste0(sprintf("%02d", as.numeric(DF[1,2])),"/",as.numeric(DF[1,1]))    
  }
  mapPlot <- getMap(data=DF, xIndex=6, yIndex=7, valueIndex=5, lim=lim,
                    plotTitle="Monthly streamflow in France",
                    plotSubtitle="and first two principal components",
                    legendTitle="Streamflow",
                    mapText=mapText,
                    mapPtColours=mapPtColours,mapBgColour=mapBgColour)
  # Plot graphs
  PC1Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=6,ndata=12,dayOn=dayOn,i=i,
                                  plotTitle="First principal component",
                                  reverseY=TRUE,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1="transparent",onColour2="transparent",onColour3="transparent",
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourSizeOn=1.5, contourSizeOff=1.5,
                                  contourColourOn=onColour1, contourColourOff=onColour1)
  PC2Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=4,ndata=12,dayOn=dayOn,i=i,
                                  plotTitle="Second principal component",
                                  reverseY=TRUE,
                                  # data=PCs,xIndex=5,yIndex=4,ndata=12,dayOn=dayOn,i=i,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1="transparent",onColour2="transparent",onColour3="transparent",
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourSizeOn=1.5, contourSizeOff=1.5,
                                  contourColourOn=onColour1, contourColourOff=onColour1)
  # Assemble map and graphs
  leftHand <- wrap_elements(mapPlot)
  rightHand <- wrap_elements((PC1Plot/plot_spacer()/PC2Plot)+plot_layout(heights=c(12,1,12)))
  png(filename=paste0(wd, "/anim_im/",sprintf("im_%03d", (i-1)),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(leftHand+rightHand+plot_layout(ncol=2,widths=c(43,57))&
          theme(plot.margin=margin(t=2,b=2,l=2,r=2, unit="cm")))
  dev.off()
  print(paste0(i,"/",n))
}

#---------------
# PLOT FIRST AND LAST IMAGES

# Plot empty map
emptyMapPlot <- getEmptyMap(data=data.frame(mapDF[mapDF$anim_state==1,]), xIndex=6, yIndex=7, valueIndex=5, lim=lim,
                            plotTitle="Monthly streamflow in France",
                            plotSubtitle="and first two principal components",
                            legendTitle="Streamflow",
                            mapText=mapText,
                            mapPtColours=mapPtColours,mapBgColour=mapBgColour)
ndata <- 12
k=0
for (j in (ndata+1):dayOn){
  k <- k+1
  # Plot graphs
  PC1Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=6,ndata=ndata,dayOn=j,i=1,
                                  plotTitle="First principal component",
                                  reverseY=TRUE,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1="transparent",onColour2="transparent",onColour3="transparent",
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourSizeOn=1.5, contourSizeOff=1.5,
                                  contourColourOn=onColour1, contourColourOff=onColour1)
  PC2Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=4,ndata=ndata,dayOn=j,i=1,
                                  plotTitle="Second principal component",
                                  reverseY=TRUE,
                                  # data=PCs,xIndex=5,yIndex=4,ndata=ndata,dayOn=j,i=1,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1="transparent",onColour2="transparent",onColour3="transparent",
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourSizeOn=1.5, contourSizeOff=1.5,
                                  contourColourOn=onColour1, contourColourOff=onColour1)
  # Assemble map and graphs
  leftHand <- wrap_elements(emptyMapPlot)
  rightHand <- wrap_elements((PC1Plot/plot_spacer()/PC2Plot)+plot_layout(heights=c(12,1,12)))
  png(filename=paste0(wd, "/anim_imStart/",sprintf("im_%03d", k),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(leftHand+rightHand+plot_layout(ncol=2,widths=c(43,57))&
          theme(plot.margin=margin(t=2,b=2,l=2,r=2, unit="cm")))
  dev.off()
  print(paste0(k,"/",(ndata-dayOn)))
}

# Create last images
for (i in (n+1):(n+dayOn)){
  # Plot graphs
  PC1Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=6,ndata=12,dayOn=dayOn,i=i,
                                  plotTitle="First principal component",
                                  reverseY=TRUE,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1=onColour1,onColour2=onColour2,onColour3=onColour3,
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourColourOn="transparent", contourColourOff=onColour1)
  PC2Plot <- getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=4,ndata=12,dayOn=dayOn,i=i,
                                  plotTitle="Second principal component",
                                  reverseY=TRUE,
                                  # data=PCs,xIndex=5,yIndex=4,ndata=12,dayOn=dayOn,i=i,
                                  areaColour=mapBgColour,bgColour="white",
                                  onColour1=onColour1,onColour2=onColour2,onColour3=onColour3,
                                  areaOnColour=areaOnColour,borderColour="grey30",
                                  offColour=offColour,
                                  contourColourOn="transparent", contourColourOff=onColour1)
  # Assemble map and graphs
  leftHand <- wrap_elements(emptyMapPlot)
  rightHand <- wrap_elements((PC1Plot/plot_spacer()/PC2Plot)+plot_layout(heights=c(12,1,12)))
  png(filename=paste0(wd, "/anim_im/",sprintf("im_%03d", i),".png"),
      width=(1280*3), height=(720*3), res=36)
  print(leftHand+rightHand+plot_layout(ncol=2,widths=c(43,57))&
          theme(plot.margin=margin(t=2,b=2,l=2,r=2, unit="cm")))
  dev.off()
  print(paste0(i,"/",(n+dayOn)))
}


#---------------
# Create animation
lf <- list.files("anim_im", full.names=TRUE)  # List all images
lfStart <- list.files("anim_imStart", full.names=TRUE)

# Duplicate first and last images to stick to the music
k <- length(lf)   #should be 1+n+dayOn=557
l <- length(lfStart)
# nStart <- (12*6)
nEnd <- (12*10)-dayOn
startImages <- c(rep(lfStart[1],(12*2)),lfStart[2:l],rep(lfStart[l],15),rep(lf[1],23))
lf2 <- c(startImages,lf,rep(lf[k],nEnd))  # length(lf2) must be 744

av_encode_video(input=lf2,
                audio="PCfunk.mp3",
                output="PCA.mp4",
                framerate=7)
