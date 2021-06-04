#---------------
# Load data and required packages
load("02_dataRunoff.RData")
load("03_dfmap.RData")
require(ggplot2)
world <- map_data("world")
require(gridExtra)

#---------------

# Select 3 stations
station1 <- "G8150018"  # NT summer
station2 <- "405245"   # VIC
station3 <- "616002"   # WA winter

sites <- c(station1,station2, station3)

#---------------
# Bar plot
plotData <- dataRunoff[which(dataRunoff$siteid%in%sites),]

# Add station col for facet_wrap
plotData$station <- NA
for (i in 1:NROW(plotData)){
  id <- plotData$siteid[i]
  nb <- which(sites==id)
  plotData$station[i] <- nb
}

# Plot bars
ppt <- ggplot()+
  geom_col(data=plotData, aes(x=month, y=(QoverSum*100), fill=meanQyear))+
  facet_wrap(vars(station), ncol=1, 
             strip.position="right")+   # change grey stripe position
  scale_x_continuous(breaks=seq(1,12,1),
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_fill_gradient(high="#132B43", low="#56B1F7")+
  scale_y_continuous(limits=c(-0.01,42),
                     expand=c(0,0))+
  labs(x="Month", y="Part of annual flow [%]",
       fill="Annual flow \n [mm/year]",
       caption= " Data source: http://www.bom.gov.au/water/hrs/")+
  theme_bw()+
  theme(axis.text=element_text(size=18), # Set axis text size
        axis.title=element_text(size=26),
        strip.background= element_rect(fill="grey92"),
        strip.text.y=element_text(size=16, angle=0),  # Rotate strip text
        plot.caption=element_text(size=16, hjust=1.05), # Set caption text size
        legend.position=c(0.8,0.95),
        legend.direction="horizontal",
        legend.text=element_text(size=14),
        legend.title=element_text(size=18),
        legend.key.height=unit(0.8,"cm"), # Set legend height
        legend.key.width=unit(1.1,"cm")) # Set legend width

#---------------
# Map
mapData <- dfmap2[which(dfmap2$siteid%in%sites),]

# Add station col for facet_wrap
mapData$station <- NA
for (i in 1:NROW(mapData)){
  id <- mapData$siteid[i]
  nb <- which(sites==id)
  mapData$station[i] <- nb
}

# Plot map
ptscol <- "black"

pmap <- ggplot()+
  geom_polygon(data=world[world$region=="Australia",],
               aes(x=long,y=lat,group=group),
               fill="grey92", alpha=1)+
  scale_y_continuous(limits=c(-45,-8),
                     expand=c(0,0))+
  scale_x_continuous(limits=c(112,156),
                     expand=c(0,0))+
  coord_fixed(ratio=1)+
  geom_point(data=dfmap2, aes(x=as.numeric(long_bis),
                              y=as.numeric(lat_bis)),
              size=2, colour="grey60")+
  geom_point(data=mapData, aes(x=as.numeric(long_bis),
                                y=as.numeric(lat_bis)),
                            size=5, colour=ptscol)+
  geom_text(data=mapData, aes(x=(as.numeric(long_bis)-0.6),
                              y=(as.numeric(lat_bis)+1.6),
                              label=station),
            size=8, colour=ptscol)+
  theme_classic()+
  labs(x=NULL, y=NULL)+
  theme(axis.ticks=element_blank(), # Remove ticks on x and y axis
        axis.text=element_text(colour="white", size=12), # Set axis text size
        axis.line=element_line(size=0.7, linetype="solid", colour="white"),
        axis.text.y=element_blank())

#---------------
# Merge map and plot
long <- 1280
perc <- 0.6
layout <- rbind(c(rep(1,long*perc), rep(2,long*(1-perc))))

#---------------
# Save merged image
png(filename="OzMap.png",
    width=1280, height=720, units="px")
combo <- grid.arrange(ppt, pmap,layout_matrix=layout)
dev.off()
