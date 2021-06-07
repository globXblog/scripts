#---------------
# Fun with dendrograms
# Find out three ways to visualise hierarchical data
#---------------
# Load required packages
library(ggplot2);library(gganimate)

# Read files
load("circularDendrogramPoints.RData")
load("circularDendrogramSgts.RData")

# Set plot colours
colrs <-  c("#3C4347", # 1 group 
             "#c47000","#005487", # 2 groups
             "#c47000","#679c40","#6090d8", # 3 groups
             "#e0a81c","#679c40","#6090d8","#8c3800") # 4 groups

#---------------
# Create ciruclar dendrogram

# Plot leaves
leaves <- circularDendrogramPoints[circularDendrogramPoints$type=="leaf",]
g0 <- ggplot()+ geom_point(data=leaves,aes(x=xbent, y=ybent, color=colr, size=size), alpha=0.6)

# Add branches
g0 <- g0+geom_segment(data=circularDendrogramSgts, aes(x=from_x, xend=to_x,y=from_y, yend=to_y,color=colr))

# Makes the plot look nicer
g0 <- g0+coord_fixed(ratio=1)+
          scale_color_gradientn(colours=colrs, na.value="grey50")+
          theme_void()+
          theme(legend.position="none")

# Animate circular dendrogram g0
g1 <- g0+transition_states(states=nb_groups, transition_length=1, state_length=1)

# Save animated circular dendrogram g1
a1 <- animate(g1, nframes=48, fps=6,width=1280, height=720, res=36, type="cairo")
anim_save(a1,file="funWithCircularDendrogram.gif")
