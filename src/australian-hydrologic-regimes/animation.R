#---------------
# Load required packages and functions
library(ggplot2);library(magick);library(dplyr);library(gganimate);library(tidyr)
load("dataRunoff.RData")  # Data
source("getNewGroupNumbersDataframe.R")
world <- map_data("world")   # Background map

#---------------
# SPLIT DATA INTO GROUPS

# Rearrange dataRunoff dataframe to get QoverSum for each month (columns) and each site (rows)
datawide <-  pivot_wider(data=dataRunoff[,c(1,2,5)], names_from=month, values_from=QoverSum)
datawide <-  as.data.frame(datawide)  # Transform datawide into dataframe
rownames(datawide) <- datawide$siteid   # Add row names datawide into dataframe
datawide <- datawide[,-1]   # Remove siteid col

# Calculate distances between sites using QoverSum
dataDist <- dist(datawide)

# Order data by distance based on QoverSum
h <- hclust(dataDist,method='ward.D')

# Create function to split h into groups
getGroups <- function(dataframe, datawide, hclustList, nb_groups) {
  # Cut dendrogram (hclust result) into groups
  g  <- cutree(tree=hclustList,k=nb_groups)
  # Add group column to dataframe
  m <- length(hclustList$labels)    # Nb of sites with data AND coordinates
  n <- NCOL(datawide)
  df_dendrogram <- cbind(dataframe,
                         "Group"=numeric(m*n),  # Duplicate dataframe and add group col
                         "nbgroups"=numeric(m*n))
  # Fill group col
  for (i in 1:m){
    df_dendrogram$Group[((n*i)-(n-1)):(n*i)] <- g[i]
    df_dendrogram$nbgroups <- nb_groups
  }
  return(df_dendrogram)
}

# Split dataRunoff into groups (from 1 to 4)
dataRunoffGroups <- data.frame()
nbgpmin <- 1  # min number of groups data should be split into
nbgpmax <- 4   # max number of groups data should be split into

for (i in nbgpmin:nbgpmax){
  nb_groups <- i
  df <- getGroups(dataframe=dataRunoff, datawide=datawide, hclustList=h, nb_groups=i)
  dataRunoffGroups <- rbind(dataRunoffGroups,df)
}

# Add new Group in to keep same point colours in final animation
dataRunoffGroups <- getNewGroupNumbersDataframe(dataGroups=dataRunoffGroups[,c(1,2,5,11,12,15,16)],
                                                   colGp=6,colNbGroups=7,colId=1)

#---------------
# CREATE PLOT (QoverSum by month for each group) AND MAP BY GROUPS

# Create function to plot facet manually
getManuelFacets <- function(data,x,y,
                            minplot=round(min(data[,y])-(max(data[,y])*0.1),2),
                            maxplot=round(max(data[,y])+(max(data[,y])*0.3),2),
                            gap=0.05,
                            ybreaks=c(min(data[,y]),mean(c(min(data[,y]),max(data[,y]))),max(data[,y])),
                            ylabels=c(min(data[,y]),mean(c(min(data[,y]),max(data[,y]))),max(data[,y]))) {
  
  # Calculate frame coordinates
  frameheight <- maxplot-minplot+gap
  nbgroupmax <- max(as.numeric(as.character(data$Group)))
  p <- nbgroupmax*4  # max number of group * 4 corners per rectangle
  deltax <- (max(data[,x])-min(data[,x]))*0.05
  
  rectshape <- data.frame("x"=numeric(p),
                          "y"=numeric(p),
                          "num"=numeric(p),
                          stringsAsFactors=FALSE)
  rectshape$x <- rep(c(min(data[,x])-deltax,max(data[,x])+deltax,max(data[,x])+deltax,min(data[,x])-deltax),nbgroupmax)
  for (i in 1:nbgroupmax){
    rectshape$y[((i*4)-3):((i*4)-2)] <- (minplot)+(i-1)*frameheight
    rectshape$y[((i*4)-1):((i*4))] <- (maxplot)+(i-1)*frameheight
    rectshape$num[((i*4)-3):(i*4)] <- i
  }
  return(rectshape)
}

# Facet coordinates
rectangleCoordinates <- getManuelFacets(data=dataRunoffGroups,x=2,y=3,gap=0.05)

# Calculate new y coordinates to be plotted in manual facets
dataRunoffGroups$newy <- NA
minplot <- round(min(dataRunoffGroups$QoverSum)-(max(dataRunoffGroups$QoverSum)*0.1),2)
maxplot <- round(max(dataRunoffGroups$QoverSum)+(max(dataRunoffGroups$QoverSum)*0.3),2)
frameheight <- maxplot-minplot+0.05
for(i in nbgpmin:nbgpmax){
  mask <- dataRunoffGroups$newGroup==i
  dataRunoffGroups$newy[mask] <- dataRunoffGroups$QoverSum[mask]+((nbgpmax-i)*frameheight)
}

#---------------
# Create a new dataframe (dataRunoffForAnimation) containing data to be plotted
# so point colours change then points move for each value of nbgroups

# # Add anim_states and plot_colour columns to dataRunoffGroups
# dataRunoffGroups$anim_states <- NA   # Index to be used as transition_state in animation
# dataRunoffGroups$plot_colour <- NA   # Point colour

# Group data and filter by nbgroups
groupedData <- group_by(dataRunoffGroups,nbgroups)

# Create dataRunoffForAnimation dataframe
# and fill its anim_states and plot_colour columns for nbgpmin
dataRunoffForAnimation <- filter(groupedData, nbgroups==nbgpmin)
dataRunoffForAnimation$anim_states <- 1
dataRunoffForAnimation$plot_colour <- 1

# Fill dataRunoffForAnimation for all other nbgroups
for (i in (nbgpmin+1):(nbgpmax-nbgpmin+1)){
  # Filter data to get i-1 and i nbgroups
  temp1 <- filter(groupedData, nbgroups==(i-1))
  temp2 <- filter(groupedData, nbgroups==i)
  # Copy newGroup in i in i-1
  temp1$newGroup <- temp2$newGroup
  temp1$nbgroups <- temp2$nbgroups
  # Fill anim_states col
  temp1$anim_states <- i+(i-1)-1
  temp2$anim_states <- i+(i-1)
  # Fill plot_colour col
  temp <- data.frame("original"=temp2$newGroup)
  temp$new <- NA
  temp$plot_colour <- NA
  gpsid <- unique(temp2$newGroup)  # Group names when nbgroups==i
  for (j in 1:NROW(temp)){
    z <- temp$original[j]
    temp$new[j] <- which(gpsid==z)
    temp$plot_colour[j] <- (((i-1)*i)/2)+temp$new[j]
  }
  temp1$plot_colour <- temp$plot_colour
  temp2$plot_colour <- temp$plot_colour
  dataRunoffForAnimation <- rbind(dataRunoffForAnimation, temp1, temp2)
}

#---------------
# Plot settings

# Create folders to store images from animation 
unlink('map',recursive=TRUE);dir.create('map')
unlink('graph',recursive=TRUE);dir.create('graph')
unlink('combined',recursive=TRUE);dir.create('combined')

# Set colours
textcol <- "#333B43"   # Element text colour, from ochRe palette  emu_woman_paired[4]
mapcol <- "#e0c48c" # Map colour, from ochRe palette olsen_qual, light tan
# Set point colours: one colour per value in dataRunoffForAnimation$plot_colour
palcol <- c("#3C4347",  #grey (dead reef [3])
            "#c47000","#005487",  # orange (olsen_seq[4]), dark blue (healthy_reef[4])
            "#c47000","#6090d8","#679c40",  # orange (olsen_seq[4]), light blue (tasmania[4]), light green (healty_reef[6])
            "#e0a81c","#8c3800","#6090d8","#679c40") # yellow (olsen_seq[6]), red (olsen_seq[2]), light blue (tasmania[4]), light green (healty_reef[6])

# Animation settings
nframes <- 120 # Number of frames
fps <- 10  # Frame per sec

#---------------
# Create graph by group and save images in "graph"
yaxisbreaks <- c(0,0.2,0.4,0.6)   # Calculate y axis breaks and labels
for (i in (nbgpmin+1):nbgpmax){
  yaxisbreaks <- c(yaxisbreaks,c(0,0.2,0.4,0.6)+(i-1)*frameheight)
}
yaxislabels <- rep((c(0,0.2,0.4,0.6)*100),nbgpmax)

plot_group <- ggplot()+
  geom_polygon(data=rectangleCoordinates,
               aes(x=x, y=y, group=num),col = "gray92", fill = "transparent")+
  geom_point(data=dataRunoffForAnimation, aes(x=month, y=newy, colour=plot_colour), size=3, alpha=0.3)+
  scale_x_continuous(breaks=seq(0,13,1),expand=c(0,0),
                     labels=c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", ""))+
  scale_y_continuous(breaks=yaxisbreaks, labels=yaxislabels,expand=c(0,0))+
  scale_colour_gradientn(colours=palcol)+
  theme_classic()+
  labs(x=NULL, y=NULL, title="Percentage of annual flow")+
  theme(legend.position="none",   # Remove legend
        axis.line=element_blank(),  # Remove axis lines
        axis.ticks.x=element_blank(),  # Remove ticks on x axis
        axis.ticks.y=element_line(colour="#B1B2B5"),  # Set y axis colour
        axis.text.x=element_text(colour=textcol, size=18), # Set axis text size
        axis.text.y=element_text(colour=textcol, size=16), # Set axis text size
        plot.title=element_text(colour=textcol, size=36), # Set title size and position
        plot.margin=margin(t=1, r=0.5, b=1, l=1.2, "cm"))

plot_group_anim <- plot_group + transition_states(states=anim_states,   # Animate plot
                                                  transition_length=2,
                                                  state_length=4)
gifGraph <- animate(plot_group_anim, nframes=nframes, fps=fps,    # Save animation images
                    width=560, height=720, 
                    renderer=file_renderer(dir="graph"))

#---------------
# Create map by group and save images in "map"
DFmap <- dataRunoffForAnimation[dataRunoffForAnimation$month==1,]  # 1 row per site only

map_group <- ggplot() +    # Create map
  geom_polygon(data=world[world$region=="Australia",],aes(x=long,y=lat,group=group),fill="gray92")+
  ylim(-44, -11)+
  xlim(112, 154)+
  coord_fixed(ratio=1)+
  geom_point(data=DFmap,aes(x=lon,y=lat,colour=plot_colour),size=4,alpha=0.7)+
  scale_colour_gradientn(colours=palcol)+
  labs(fill="Group")+
  theme_void()+
  theme(legend.position="none")

map_group_anim <- map_group + transition_states(states=anim_states,   # Animate map
                                                transition_length=2,
                                                state_length=4)

gifMap <- animate(map_group_anim, nframes=nframes, fps=fps,   # Save animation images
                  width=720, height=720, 
                  renderer=file_renderer(dir="map"))

#---------------
# Merge maps and plots
flist <- list.files("map")
for (i in 1:nframes){
  map_mgif <- image_read(file.path('map',flist[i]))
  graph_mgif <- image_read(file.path('graph',flist[i]))
  combined <- image_append(c(graph_mgif,map_mgif))
  image_write(combined, file.path('combined',flist[i]))
}

#---------------
# Animate combined map and plot
clist <- list.files("combined", full.names = TRUE)
im <- image_read(clist)
image_write_gif(image=im,path="australianHydrologicRegimes.gif",delay=1/fps)
