# WARNING: this script takes a long time to run (several hours).
# Try decreasing resfactor to 0.25 for quicker-but-poor-quality videos.
library(ggplot2) # plotting library
library(av) # library for creating video from individual plots
# Data ----
DF=read.table('SOI_EastOzPrecip.csv',header=T,sep=';')
# Compute seasonal SOI values by 3-month moving averaging
foo=stats::filter(x=DF$SOI,filter=rep(1,3)/3)
foo[seq(2,length(foo),3)]=foo[seq(1,length(foo),3)] 
foo[seq(3,length(foo),3)]=foo[seq(1,length(foo),3)]
DF$SOIseasonal=foo

# Define colors ----
bcolor='#09232f' # background
gcolor='#5A493A' # panel grid
linecolor='#f7efdc' # lines for time series
vlinecolor="#eddaa6" # vertical line denoting the current value
pointcolor=c('#e2733d','#e55e5e','#8fb5e3') # Points/text for SOI, seasonal SOI and P
txtcolor=linecolor # plot title and labels

# Functions ----
# Function plotting one series y  at one given time step indx
onePanel <- function(y,indx,
                     gg=NULL, # if NULL, creates a new ggplot, otherwise add layers to existing gg
                     cursor=0.75, # vertical line denoting the current value, between 0 (full left) and 1 (full right)
                     right.nmax=96, # max. number of points right of the cursor
                     alpha.range=c(0.1,0.5), # alpha scaling for values left of the cursor
                     point.color=pointcolor,point.size=6, # points properties
                     line.color=linecolor,line.size=0.3, # lines properties
                     vline.show=F,vline.color=vlinecolor,vline.size=0.15*point.size # vertical line properties
                     ){
  # Length of plotted series
  n=min(c(indx+right.nmax-1,length(y)))
  # Values right of the cursor, including the cursor
  right.y=y[indx:n]
  right.step=(1-cursor)/(right.nmax-1)
  right.x=seq(cursor,1,right.step)[1:length(right.y)]
  # Values left of the cursor, including the cursor
  left.y=y[1:indx]
  left.step=min(c(cursor/(indx-1),right.step))
  left.x=seq(cursor,0,-1*left.step)[length(left.y):1]
  # Plotting using ggplot
  if(is.null(gg)) {g=ggplot()} else {g=gg}
  # line on the left
  g=g+geom_line(data=data.frame(x=left.x,y=left.y),aes(x=x,y=y,alpha=x),
                size=line.size,colour=line.color)
  g=g+scale_alpha(range=alpha.range)
  # line on the right
  g=g+geom_line(data=data.frame(x=right.x,y=right.y),aes(x=x,y=y),
                size=line.size,colour=line.color)
  # cursor
  if(vline.show){g=g+geom_vline(xintercept=cursor,size=vline.size,colour=vline.color)}
  g=g+geom_point(aes(x=right.x[1],y=right.y[1]),colour=point.color,size=point.size)
  # tidy up
  g=g+theme(legend.position='none')
  return(g)
}
# utility function to rescale a series y into [0;1]
rescale <- function(y){
  mini=min(y,na.rm=T)
  maxi=max(y,na.rm=T)
  return((y-mini)/(maxi-mini))
}
# Function to create all individual plots
makeplot <- function(){
  for(i in 1:(NROW(DF)+12*4)){ # Note the 12*4 additional frames at the end to allow for a 'fade out' of the audio
    indx=ifelse(i<=NROW(DF),i,NROW(DF))
    message(paste0(indx,'/',NROW(DF))) 
    # Create empty plot
    g=ggplot() 
    # Add panel grid
    for(h in seq(0,3,0.25)){
      g=g+geom_hline(yintercept=h,color=gcolor,alpha=0.2,size=0.5)
    }
    for(h in seq(0.5,2.5,1)){
      g=g+geom_hline(yintercept=h,color= gcolor,size=0.5)
    }
    # -1*SOI series (rescaled between 2 and 3)
    y=2+rescale(-1*DF$SOI)
    g=onePanel(y,indx,vline.show=T,gg=g,point.color=pointcolor[1])
    g=g+geom_text(aes(x=-0.1,y=2.5,label='-SOI'),vjust=-0.2,hjust=0,size=8,family='sans',colour=pointcolor[1])
    # -1*seasonal SOI series  (rescaled between 1 and 2)
    y=1+rescale(-1*DF$SOIseasonal)
    g=onePanel(y,indx,gg=g,point.color=pointcolor[2])
    g=g+geom_text(aes(x=-0.1,y=1.5,label='Seasonal -SOI'),vjust=-0.2,hjust=0,size=8,family='sans',colour=pointcolor[2])
    # P series (rescaled between 0and 1)
    y=rescale(DF$P)
    g=onePanel(y,indx,gg=g,point.color=pointcolor[3])
    g=g+geom_text(aes(x=-0.1,y=0.5,label='Rainfall'),vjust=-0.2,hjust=0,size=8,family='sans',colour=pointcolor[3])
    # Tidy up
    g=g+xlim(-0.1,1)+ylim(0,3)
    g=g+labs(title='The effect of El Nino on Eastern Australian Rainfall',
             subtitle=paste0(DF$Year[indx],' ',DF$month[indx]),
             caption='Data: Bureau of Meteorology')
    g=g+theme_bw()
    g=g+theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
              panel.grid=element_blank(),legend.position='none',
              plot.background=element_rect(fill=bcolor),panel.background=element_rect(fill=bcolor),
              plot.title=element_text(size=36,colour=txtcolor),
              plot.subtitle=element_text(family='sans',size=32,colour=txtcolor),
              plot.caption=element_text(size=12,colour=txtcolor))
    print(g)
  }
}

# Create video ----
bpm=120 # tempo in beats per minute
nyear=length(unique(DF$Year)) # number of years
nframes=nyear*12 # total number of frames 
nbeat=3*nyear # total number of beats  
d=(nbeat*60)/bpm # duration of the video
fps=nframes/d # frames per second
resfactor=1 # resolution factor. 1 is OKish, 2 recommended for good quality, but VERY long to run (dozen of hours)
noGuitar=TRUE
# Create video
if(noGuitar){
  audioFile='ElNinoz_noGuitar.wav'
  outputFile='ElNinoz_noGuitar.mp4'
} else {
  audioFile='ElNinoz.mp3'
  outputFile='ElNinoz.mp4'
}
av_capture_graphics(makeplot(),output=outputFile,
                    audio=audioFile, # NOTE: To be commented out if you don't have the audio file
                    res=72*resfactor,
                    width=1280*resfactor,height=720*resfactor,framerate=fps)
