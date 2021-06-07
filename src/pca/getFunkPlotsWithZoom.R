#---------------
# Create movie from france 207 data
# for the blog

#---------------
# # Load required data and packages
# library(tidyr);library(ggplot2);library(patchwork);library(av)

#---------------
# # Read data
# load("PCs.RData")
# data=PCs;xIndex=5;yIndex=3;ndata=12;dayOn=4;i=1;
# areaColour="#E6E6E6";bgColour="white"
# offColour="#3b3b3b"
# onColour1="#CDB93E";onColour2="blue";onColour3="green"
# areaOnColour=onColour1;borderColour="grey30"
# contourSizeOn=0; contourSizeOff=1.5
# contourColourOn="transparent"; contourColourOff="black";plotTitle=colnames(data)[yIndex]

#---------------
# Create function to plot graphs
getFunkPlotsWithZoom <- function(data,xIndex,yIndex,ndata=12,dayOn=4,i=1,
                                 plotTitle=colnames(data)[yIndex],
                                 reverseY=FALSE,
                                 areaColour="#E6E6E6",bgColour="white",
                                 onColour1="black",onColour2="yellow",onColour3="green",
                                 offColour="#EAEAE0",areaOnColour="red",borderColour="grey30",
                                 contourSizeOn=0, contourSizeOff=1.5,
                                 contourColourOn="transparent", contourColourOff="black"){
  # PLOT WHOLE GRAPH
  m <- NROW(data)
  g <- ggplot2::ggplot()+ geom_area(data=data, aes(x=data[,xIndex], y=data[,yIndex]), fill=areaColour)+
    scale_x_continuous(limits=c(0.5,(m+0.5)),expand=c(0,0))+
    labs(x="",y="")+
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill=bgColour))

  # Get g properties
  wholePlotParam <- ggplot_build(g)
  
  # PLOT ENLARGEMENT
  # Create empty rows at the beginning and at the end for animation
  nFakeEndData <- (ndata-dayOn+1)
  dfStart <- data[1:(dayOn-1),]
  # dfStart[,seq(1,4)] <- 0
  dfStart[-xIndex] <- 0
  dfEnd <- data[1:nFakeEndData,]
  # dfEnd[,seq(1,4)] <- 0
  dfEnd[-xIndex] <- 0
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
      xEnd <- i+(ndata-dayOn)+0.5}
  
  # Get data to be plotted from df
  plotStart <- i
  plotEnd <- i+(ndata-1)
  mask <- df[df$anim_state%in%seq(plotStart,plotEnd),]
  
  # Create colour vector for geom_col
  if(dayOn>ndata){
    barFill <- rep("transparent",ndata)
    colourSize <- rep(0,ndata)
    barColour <- rep("transparent",ndata)
  } else {
    barFill <- c(rep(offColour,(dayOn-1)),onColour1,rep(offColour,(ndata-dayOn)))
    colourSize <- c(rep(contourSizeOff,(dayOn-1)),contourSizeOn,rep(contourSizeOff,(ndata-dayOn)))
    colourSize <- ifelse (mask[,yIndex]==0,0,colourSize)
    barColour <- c(rep(contourColourOff,(dayOn-1)),contourColourOn,rep(contourColourOff,(ndata-dayOn))) 
  }

  # Add coloured stripes on g (whole plot)
  wholePlotWithStripes <- g+geom_polygon(aes(x=c(xStart,xEnd,xEnd,xStart),
                                             y=c(-Inf,-Inf,Inf,Inf)),
                                         colour=alpha(borderColour,0.6), size=1.5,fill="transparent")+
                            geom_area(data=mask,aes(x=mask[,xIndex]-(dayOn-1), y=mask[,yIndex]), 
                                      fill=areaOnColour)
  if (reverseY==TRUE){
    wholePlotWithStripes <- wholePlotWithStripes+scale_y_reverse()
  }
  
  # Create plot
  gBreaks <- wholePlotParam$layout$panel_params[[1]]$y$breaks   # Y breaks in whole plot g
  gYLims <- wholePlotParam$layout$panel_params[[1]]$y.range   # Y limits in whole plot g
  xPolygonStart <- mask[dayOn,xIndex]-0.3  # x coordinates of polygon for funky effect
  xPolygonEnd <- mask[dayOn,xIndex]+0.3   # x coordinates of polygon for funky effect
  yOn <- mask[dayOn,yIndex]
  yPolygonStart2 <- (0.15*yOn)  # y coordinates of polygon for funky effect
  yPolygonEnd2 <- yOn-(0.15*yOn)  # y coordinates of polygon for funky effect
  yPolygonStart3 <- (0.3*yOn)    # y coordinates of polygon for funky effect   
  yPolygonEnd3 <- yOn-(0.3*yOn)  # y coordinates of polygon for funky effect

  zoomPlot <- ggplot2::ggplot()+geom_col(data=mask,aes(x=mask[,xIndex], y=mask[,yIndex],
                                                       fill=as.factor(mask[,xIndex]),
                                                       colour=as.factor(mask[,xIndex])),
                                         size=colourSize)+
    scale_x_continuous(limits=c((i-0.5),(i+ndata-0.5)),expand=c(0,0))+
    # scale_y_continuous(limits=gYLims, expand=c(0,0),breaks=gBreaks)+
    scale_fill_manual(values=barFill)+
    scale_colour_manual(values=barColour)+
    geom_hline(aes(yintercept=0), colour="grey50")+
    geom_polygon(aes(x=c(xPolygonStart,xPolygonEnd,xPolygonEnd,xPolygonStart),
                     y=c(yPolygonStart2,yPolygonStart2,yPolygonEnd2,yPolygonEnd2)),fill=onColour2)+
    geom_polygon(aes(x=c((xPolygonStart+0.15),(xPolygonEnd-0.15),(xPolygonEnd-0.15),(xPolygonStart+0.15)),
                     y=c(yPolygonStart3,yPolygonStart3,yPolygonEnd3,yPolygonEnd3)),fill=onColour3)+
    labs(x="none",y=colnames(data)[yIndex],title=plotTitle)+
    theme_bw()+
    theme(legend.position="none",
          panel.grid=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.title.x=element_text(size=1),
          axis.text.y=element_text(size=90,margin=margin(r=20)),
          axis.title.y=element_text(size=10, colour="white"),
          axis.ticks.length.y=unit(20, "pt"),
          panel.border=element_rect(colour=alpha(borderColour,0.6), size=2),
          plot.title=element_text(size=100))
  
  if (reverseY==TRUE){
    zoomPlot <- zoomPlot+scale_y_continuous(limits=rev(gYLims), expand=c(0,0),trans="reverse")
    
  } else {
    zoomPlot <- zoomPlot+scale_y_continuous(limits=gYLims, expand=c(0,0))
  }
  
  # Assemble individual plots
  allPlots <- wrap_plots((zoomPlot/wholePlotWithStripes)+plot_layout(heights=c(2,1))&
                           # theme(plot.title=element_text(size=90, hjust=0),
                           #       axis.title.y=element_text(size=100, hjust=0.2)))
                          theme(axis.title.y=element_text(size=100, hjust=0.2)))
  return(allPlots)
}


# png(filename=paste0(wd, "/France207Images/00test.png"),
#     width=(1280*3), height=(720*3), res=36)
# print(getFunkPlotsWithZoom(data=PCs,xIndex=5,yIndex=3,ndata=12,dayOn=4,i=2))
# dev.off()

