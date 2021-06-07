#---------------
# Bend a linear dendrogram given a pivot point (where the line should bend)
# and a bending option (how the line should bend)
# Change the bending angle (how far the line is bent) to create a gif
# 1. Create dataframe containing data to be used to calculate distances between sites
# 2. Calculate distances between sites using dist() function
# 3. Order sites by distances using hclust() function
# 4. Transform hclust results in a easier to read dataframe using getDataframeFromHclust() function)
# 5. Create a data frame containing point properties to plt dendrogram
# 6. Plot circular dendrogram with color by group
# 7. Save circular dendrogram plot with ggsave
#---------------
# Load required packages and functions
library(tidyr);library(ggplot2);library(gganimate)
source("getClusteringStepsAndGroups.R")
source("getDendrogramPoints.R")
source("getCircleProperties.R")
source("projectDatasetOnCircle.R")
source("getDendrogramSegments.R")
load("dataRunoff.RData")   # data

#---------------
# Set parameters
nbgpmax <- 4  # Nb of groups data should be divided into
bendingOption <- 2
palcol <-  c("#609048","#c47000","#d84860","#484878")   # colours

#---------------
#---------------
# Hierarchical Clustering

data <- dataRunoff[,1:12]  # Rename data

# Rearrange dataRunoff dataframe to get QoverSum for each month (columns) and each site (rows)
DF <-  pivot_wider(data=dataRunoff[,c(1,2,5)], names_from=month, values_from=QoverSum)
DF <-  as.data.frame(DF)  # Transform DF into dataframe
rownames(DF) <- DF$siteid   # Add row names DF into dataframe
DF <- DF[,-1]   # Remove siteid col

# Calculate distances between sites using QoverSum
D <- dist(DF)

# Order data by distance based on QoverSum
hclust_data <- hclust(D,method='ward.D')

# Change height so dendrogram is not "compressed" when plotted
hclust_data$height <- (hclust_data$height)^0.25

#---------------
# Create dataframe containing clustering steps and groups of all points and nodes
StepsAndGroupsDF <- getClusteringStepsAndGroups(hclust_data=hclust_data,
                                                nb_groups=nbgpmax)

#---------------
# Create a data frame containing point properties (coordinates, size and group)
dataDendro_pts <- getDendrogramPoints(StepsAndGroupsDF=StepsAndGroupsDF,
                                      data=data, pointSizeCol=6)

#---------------
# Find dendrogram point coordinates on a circle with given bendingAngle for a whole dendrogram
n <- 40  # Number of bendingAngles
bA <- seq(6.28e-06,6.28,length.out=n)  # changing bendingAngle

DFbent_pts <- data.frame()    # Ininialize dataframe to store point properties (coordinates, size, group)
DFbent_sgts <- data.frame()    # Ininialize dataframe to store segment coordinates to link points

for (j in 1:n) {
  bendingAngle <- bA[j]
  # Find circle properties with given bendingAngle and bendingOption
  prop <- getCircleProperties(data=dataDendro_pts,
                              bendingAngle=bendingAngle, 
                              bendingOption=bendingOption)
  # Find data points coordinates on a circle with given bendingAngle
  DFpts_temp <- projectDatasetOnCircle(data=dataDendro_pts,
                                       bendingAngle=bendingAngle,
                                       ptA=prop$ptA, ptB=prop$ptB, 
                                       aCoord=prop$a, bCoord=prop$b, 
                                       circleCenter=prop$circleCenter, 
                                       circleRad=prop$circleRad,
                                       bendingOption=bendingOption)
  DFbent_pts <- rbind(DFbent_pts,DFpts_temp)
  # Find segment coordinates to link points
  DFsgts_temp <- getDendrogramSegments(StepsAndGroupsDF=StepsAndGroupsDF,
                                       dataDendro_pts=DFpts_temp,
                                       xCol=7, yCol=8)
  DFsgts_temp$bendingAngle <- bendingAngle
  DFbent_sgts <- rbind(DFbent_sgts,DFsgts_temp)
  }

#---------------
# Animate with bendingAngle as transition state
statelength <- c(3, rep(0,(n-2)),6)
g <- ggplot()+
  geom_segment(data=DFbent_sgts, aes(x = from_x, xend = to_x, 
                                     y = from_y, yend = to_y, 
                                     color = gp_sgt))+
  geom_point(data=DFbent_pts[DFbent_pts$type=="leaf",], 
             aes(x=xbent, y=ybent, colour=gp, size=val, alpha=0.5))+
  coord_fixed(ratio=1)+
  scale_color_gradientn(colours = palcol, na.value="grey50")+
  theme_void()+
  theme(legend.position="none")+
  transition_states(states=bendingAngle, transition_length = 1, state_length=statelength, wrap=FALSE)

gif1 <- animate(g, nframes = (n+1), fps = 6, width = 1280, height = 720, res=36, type = "cairo")
anim_save(gif1,file="funWithBendingDendrogram.gif")
