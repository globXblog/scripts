#---------------
# Load required packages
library(ggplot2);library(gganimate);library(magick);library(av)

# Load data
wd <- getwd()
load(file.path(wd, "dataRunoff.RData"))

# Background map
world <- map_data("world")

#---------------
# Plot settings

# Color settings 
lowQ <- "#52bcd4"  # Color for min value gradient, from ochRe palette healthy_reef, blue sand
highQ <- "#4f585e"  # Color for max value gradient, from ochRe palette dead_reef[3]
textcol <- "#333B43"   # Element text color, from ochRe palette  emu_woman_paired[4]
mapcol <- "#e0c48c" # Map color, from ochRe palette olsen_qual, light tan

# Animation settings
orderingCol <- 13  # Set index of column to be used for ordering data
imageheight <- 720   # Set image heigth
imageratio <- 16/9   # Set ratio between heigth and width in image
mapratio <- 7/16    # Part of total image width allocated to map width
m <- 195   # Nb of rows to be kept from original data  
fps <- 12  # Set frame per sec
d <- 1.25 # Length per site

#---------------
# Create folders to store anmiations
unlink('map',recursive=TRUE);dir.create('map')
unlink('graph',recursive=TRUE);dir.create('graph')
unlink('combined',recursive=TRUE);dir.create('combined')

#---------------
# Create data frame for ggplot
mask <- dataRunoff[,orderingCol] <= m
DF <- cbind(dataRunoff[mask,], 'ordering'=dataRunoff[mask,orderingCol])
maxy <- round(max(DF$QoverSum)*100)  # max value for y axis
minQ <- min(DF$meanQyear, na.rm=TRUE)  # min value for color gradient
maxQ <- max(DF$meanQyear, na.rm=TRUE)  # max value for color gradient

#---------------
# Plot map m0
dfmap <- DF[DF$month==1,]  # Cut dataRunoff to get 1 row per site
dfmap2 <- dfmap
names(dfmap2) <- paste(names(dfmap), "bis", sep="_")

m0 <- ggplot()+
  geom_polygon(data=world[world$region=="Australia",],
               aes(x=long,y=lat,group=group),fill=mapcol, alpha=0.6)+
  scale_y_continuous(limits=c(-45,-8),expand=c(0,0))+
  scale_x_continuous(limits=c(112,156),expand=c(0,0))+
  coord_fixed(ratio=1)+
  geom_point(data=dfmap2, aes(x=as.numeric(lon_bis),y=as.numeric(lat_bis),
                              col=meanQyear_bis,group=ordering_bis),
             size=4, shape="\U1F322",alpha=0.25)+
  geom_point(data=dfmap, aes(x=as.numeric(lon),y=as.numeric(lat),col=meanQyear,group=ordering),
             size=12,shape="\U1F322")+
  scale_colour_gradient(low=lowQ, high=highQ,
                        limits=c(minQ, maxQ),
                        breaks=c(10, 1000, 2000),
                        name="Annual flow [mm/year]",
                        guide=guide_colorbar(title.position="top"))+
  theme_classic()+
  labs(x=NULL, y=NULL)+
  theme(axis.ticks=element_blank(), # Remove ticks on x and y axis
        axis.text=element_text(colour="white", size=12), # Set axis text size
        axis.line=element_line(size=0.7, linetype="solid", colour="white"),
        axis.text.y=element_blank(),
        legend.position=c(0.24,0.14),
        legend.direction="horizontal", # Get horizontal legend box
        legend.key.width=unit(1.42,"cm"), # Set legend width
        legend.key.height=unit(0.5,"cm"),  # Set legend height
        legend.title=element_text(colour=textcol, size=20),  # Set color and size of legend title
        legend.text=element_text(colour=textcol, size=16,   # Set color and size of legend text
                                   margin=margin(t=-0.11, r=0, b=0, l=0, unit="cm"))) # Decrease space between legend text and legend key

# Animate m0: sites appear in the increasing order of DF$ordering
m1 <- m0 + transition_states(states=ordering,transition_length=1,state_length=3) +
          enter_grow()+
          exit_shrink()

# Save animated map in "map" (nframes number of images)
nframes <- m*fps*d
imagewidth <- imageheight*imageratio  # image height based on image height and image ratio
mapwidth <- imagewidth*mapratio # map width based on map ratio
gifMap <- animate(m1, nframes=nframes, fps=fps,
                  width=mapwidth, height=imageheight,
                  renderer=file_renderer(dir="map"))

#---------------
# Plot graph b0
b0 <- ggplot(data=DF)+
  geom_col(aes(x=month,y=(QoverSum)*100,fill=meanQyear))+
  scale_y_continuous(limits=c(0,maxy*1.1),breaks=seq(0,50,10),expand=c(0,0))+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  coord_fixed(ratio=(8.5/55.4))+
  scale_fill_gradient(low=lowQ, high=highQ,
                      limits=c(minQ, maxQ),
                      breaks=c(10, 1000, 2000))+ 
  labs(x=NULL, y=NULL,
       title="Percentage of annual flow")+ 
  theme_classic()+
  theme(axis.ticks.x=element_blank(), # Remove ticks on x axis
        axis.text=element_text(colour=textcol, size=16), # Set axis text size
        plot.title=element_text(colour=textcol, size=26, hjust=0.5), # Set title size and position
        legend.position="none",
        plot.margin=margin(t=0, r=0, b=0.6, l=0.7, "cm"),
        axis.line=element_line(size=0.7, linetype="solid"))

# Animate b0: sites appear in the increasing order of DF$ordering
b1 <- b0 + transition_states(states=ordering, transition_length=1, state_length=3)

# Save animated graph in "graph" (nframes number of images)
gifGraph <- animate(b1, nframes=nframes, fps=fps,
                    width=imagewidth*(1-mapratio), height=imageheight, 
                    renderer=file_renderer(dir="graph"))

#---------------
# Merge map and plot
flist <- list.files("map")
for (i in 1:nframes){
  map_mgif <- image_read(file.path('map',flist[i]))  # Read map
  graph_mgif <- image_read(file.path('graph',flist[i]))   # Read graph
  combined <- image_append(c(graph_mgif, map_mgif))  # Combine map and graph
  image_write(combined, file.path('combined',flist[i]))   # Save combined image
}

# Animate combined map and plot
clist <- list.files("combined", full.names=TRUE)
av_encode_video(input=clist,
                output=paste(names(dataRunoff[orderingCol]), ".mp4", sep=""),
                framerate=fps,
                audio="OZdataset_arabic_3beats_orderByClustering.mp3")
