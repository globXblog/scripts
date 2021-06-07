# dfForEmptyMap <- DF[DF$anim_state==1,]
# line1 <- dfForEmptyMap[1,]
# dfForEmptyMap$value <- NA
# 
# 
# data=DF; xIndex=6; yIndex=7; valueIndex=5; lim=lim;
# plotTitle="Monthly streamflow in France";
# plotSubtitle="and first two principal components";
# legendTitle="Streamflow";
# mapText=mapText;
# mapPtColours=mapPtColours;mapBgColour=mapBgColour

getEmptyMap <- function(data, xIndex, yIndex, valueIndex, lim,
                         plotTitle,plotSubtitle,legendTitle,mapText,
                         mapPtColours,mapBgColour="#E6E6E6") {
  
  # Create dataframe to be plotted
  dfForEmptyMap <- data
  line1 <- data[1,]
  dfForEmptyMap$value <- NA

  # Plot
  world <- ggplot2::map_data("world")
  g <- ggplot()+
    geom_point(data=line1,aes(x=x,y=y,colour=value, size=is.na(value)))+
    geom_polygon(data=world[world$region=="France",],aes(long,lat,group=group),fill=mapBgColour,size=0)+
    geom_point(data=dfForEmptyMap,aes(x=x,y=y),colour="#B3B3B3", size=20)+
    annotate("text", x=-3.8, y=51.7, label="", size=50)+
    scale_colour_gradientn(colours=mapPtColours,na.value="#B3B3B3",
                           limits=lim,breaks=(lim*0.8),labels=c("Low", "High"))+
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

# png(filename=paste0(wd, "/France207Images/00test.png"),
#     width=(1280*3), height=(720*3), res=36)
# print(tt)
# dev.off()
