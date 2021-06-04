library(ggplot2);library(gganimate) # Plotting
library(ochRe) # Australian color palette
library(MASS);library(Rtsne) # Multidimensional scaling
library(ggvoronoi);library(concaveman) # Polygons (Voronoi and hull)

concavity=1.5 # parameter for computing "land" (concave hull)
maxiter=10000 # max. iterations for t-SNE
mySeed=999 # fix random seed to make t-SNE repeatable
showPolygons=TRUE # Show polygons ("land") or  only points?
pngSave=TRUE # save static images as png - otherwise pdf will be used

#*********************************************************----
# Define standardization of coordinates ----
stdz <- function(x,y,xyratio=1){
  # bring x and y between -1 and 1
  xout=2*(x-min(x))/(max(x)-min(x))-1
  yout=2*(y-min(y))/(max(y)-min(y))-1
  xout=xout*xyratio
  return(list(x=xout,y=yout))
}

#*********************************************************----
# Read data ----
D=read.table(file='OZ_FractionalMonthlyQ.txt',header=TRUE)
N=NROW(D)
Q=D[,4:15] # monthlyQ columns

#*********************************************************----
# Geographic map -----
# Get Australian land
world=map_data("world")
mask=(world$region=='Australia') &(
  is.na(world$subregion) # mainland
  | (world$subregion=='Tasmania')
) 
oz=world[mask,1:3]
# Center and scale coordinates + oz land
foo=stdz(c(D$x,oz$long),c(D$y,oz$lat),xyratio=(diff(range(D$x))/diff(range(D$y))))
x=foo$x;y=foo$y
# Get points
sites=data.frame(indx=1:N,site=D$site,x=x[1:N],y=y[1:N],
               type='GEOGRAPHIC',
               lon=D$x,lat=D$y) # used for coloring
allSites=sites
# Get land
land=data.frame(x=x[-(1:N)],y=y[-(1:N)],
                group=oz$group, type='GEOGRAPHIC')
allLands=land
# Get Voronoi
v=voronoi_polygon(sites,outline=land,data.frame=TRUE)
allV=v

#*********************************************************----
# t-SNE map -----
# Apply algorithm
set.seed(mySeed) # make it reproducible
w=Rtsne(Q,maxiter=maxiter)
w$Y=-1*w$Y # use mirror image (more consistent with geographic Australia)
# Center and scale coordinates
foo=stdz(w$Y[,1],w$Y[,2])
x=foo$x;y=foo$y
# Get points
sites=data.frame(indx=1:N,site=D$site,x=x,y=y,
                 type='t-SNE',
                 lon=D$x,lat=D$y) # used for coloring
allSites=rbind(allSites,sites)
# Get land
hulk=concaveman(cbind(x,y),concavity=concavity)
land = data.frame(x=hulk[,1],y=hulk[,2],group=0, type='TSNE')
allLands=rbind(allLands,land)
# Voronoi
v=voronoi_polygon(sites,outline=land,data.frame=TRUE)
allV=rbind(allV,v)

#*********************************************************----
# ISOMAP -----
# Apply algorithm
d=dist(Q)
w=isoMDS(d)
w$points=-1*w$points[,c(2,1)] # rotate and mirror (more consistent with geographic Australia)
# Center and scale coordinates
foo=stdz(w$points[,1],w$points[,2])
x=foo$x;y=foo$y
# Get points
sites=data.frame(indx=1:N,site=D$site,x=x,y=y,
                 type='ISOMAP',
                 lon=D$x,lat=D$y) # used for coloring
allSites=rbind(allSites,sites)
# Get land
hulk=concaveman(cbind(x,y),concavity=concavity)
land = data.frame(x=hulk[,1],y=hulk[,2],group=0, type='ISOMAP')
allLands=rbind(allLands,land)
# Voronoi
v=voronoi_polygon(sites,outline=land,data.frame=TRUE)
allV=rbind(allV,v)

#*********************************************************----
# Plot & Animate -----
g=ggplot()
# Add Voronoi polygons
if(showPolygons){
  g=g+geom_polygon(data=allV,aes(x=x,y=y,group=site,fill=lat),
                 color='white',alpha=0.3,show.legend=FALSE)
  g=g+scale_fill_ochre(palette="olsen_seq",discrete=FALSE)
}
# Add sites - note the reverse=TRUE in scale_color_ochre because 
# scale_color_ochre and scale_fill_ochre have opposite defaults
g=g+geom_point(data=allSites,aes(x=x,y=y,color=lat,group=site),
               size=3,alpha=0.7,show.legend=FALSE)
g=g+scale_color_ochre(palette="olsen_seq",discrete=FALSE,reverse=TRUE) 
# Finalize
g=g+theme_void()+coord_fixed()

# Save static plot
filename=paste0('OzAnimation_',ifelse(showPolygons,'with','without'),'Polygons')
if(pngSave){
  png(paste0(filename,'.png'),width=300,height=90,unit='mm',res=300)
} else {
  pdf(paste0(filename,'.pdf'),width=12,height=3.6)
}
print(g+facet_wrap(vars(type))+
  theme(strip.text=element_text(hjust=0.5,vjust=1,size=16)))
dev.off()

# Animate plot
g2=g+transition_states(states=type)+
 labs(title = "{closest_state}")+
 theme(plot.title = element_text(hjust=0.5,vjust=1,size=40))
anim=animate(g2,nframes=72,duration=6,height=600,width=600)
anim_save(paste0('OzAnimation_',ifelse(showPolygons,'with','without'),'Polygons.gif'), anim)
