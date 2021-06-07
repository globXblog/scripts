#---------------
# Fun with dendrograms
# Find out three ways to visualise hierarchical data
#---------------
# Load required packages
library(ggplot2);library(gganimate)

# Read data
load("linearDendrogramPoints.Rdata")
load("linearDendrogramSegments.RData")

# Set plot colours
colrs <- c("#609048","#c47000","#d84860","#484878")
#---------------
# Create dendrogram

# Plot leaves
leaves <- linearDendrogramPoints[linearDendrogramPoints$type=="leaf",]
g0 <- ggplot()+geom_point(data=leaves,aes(x=x, y=y, col=pt_group), size=6, pch=19)

# Add branches
g0 <- g0+geom_segment(data=linearDendrogramSegments,aes(x=from_x, xend=to_x,y=from_y, yend=to_y,col=sgt_group), size=1)

# Add dotted cut line
dottedLine <- linearDendrogramSegments[linearDendrogramSegments$to_id=="shoulder1",]
g0 <- g0+geom_segment(data=dottedLine,aes(x=0.8, xend=(195+0.2), y=line_height, yend=line_height),size=0.8, col="grey60", linetype="dashed")+
          geom_point(data=dottedLine,aes(x=0.8, y=line_height+max(linearDendrogramPoints$y)*0.023), col="grey60", size=40, shape="\U2701")

# Makes the dendrogram look nicer
g0 <- g0+scale_color_gradientn(colours=colrs, na.value="grey50")+
         theme_void()+
         theme(legend.position="none",
               plot.title=element_text(size=100, colour="grey50"))

# Animate plot g0
g1 <- g0+transition_states(states=nb_group, transition_length=1,state_length=1)+
          labs(title="{previous_state}-group cut")

# Save animated plot g1
a1 <- animate(g1, nframes=48, fps=6, width=1280, height=720, res=36)
anim_save(a1,file="funWithLinearDendrogram.gif")
