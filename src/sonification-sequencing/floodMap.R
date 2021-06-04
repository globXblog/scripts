library(gganimate);library(sequenceR) # https://github.com/benRenard/sequenceR
duration=36.5 # Duration of the animation is second
fps=24 # Number of frames per second

# Part 1: read data ----
floods=read.table(file='floodObservatory_06June2020.csv',header=TRUE,sep=';',quote=NULL)
# rescale
floods$Area=floods$Area/10000
# restrict to target year
targetYear=2019
years=as.numeric(format(as.Date(floods$Began,format='%d/%m/%Y'),'%Y'))
floods=floods[years==targetYear,]
# get time vector
dates=as.Date(floods$Began,format='%d/%m/%Y')
time=as.numeric(dates-as.Date(paste0("01/01/",targetYear),format='%d/%m/%Y'))
floods=cbind(floods,time=time,group=1:NROW(floods))

# Part 2: Create Animation ----
# Function to format a time expressed in number of day since 01/01 as a date
# Will be used to label the plot
time2date <- function(time){
  d0=as.Date(paste0(targetYear,'-01-01'))
  return(format(d0+time,'%d/%m/%Y'))
}
# Create background map
world = map_data("world")
g=ggplot()
g=g+geom_polygon(data=world,aes(long,lat,group=group),
                 fill='#fff4caff',color=NA,size=0.1)
g=g+coord_fixed(ratio=1,ylim=c(-58,83.6),expand=FALSE)
g=g+theme_void()
g=g+theme(panel.border=element_rect(fill=NA))
# Add points
g=g+geom_point(data=floods,aes(x=long,y=lat,colour=Duration,size=Area,group=group),alpha=1)
lab=expression(paste('Area [',10^4,' ',km^2,']'))
g=g+scale_size(lab,breaks=c(1,10,50,100),range=c(0,10))
g=g+scale_colour_distiller('Duration [day]',palette='Blues',trans='log10',direction=1)
g=g+guides(size=guide_legend(order=1),colour=guide_colourbar(order=2)) # order of legends
# titles
g=g+labs(title=paste('Major flood events: {time2date(frame_time)}'),
         subtitle='Taken from the Dartmouth Flood Observatory',
         caption='G.R.Brakenridge, "Global Active Archive of Large Flood Events", Dartmouth Flood Observatory, University of Colorado.\n http://floodobservatory.colorado.edu/Archives/index.html, accessed 06 June 2020')
g=g+theme(plot.title=element_text(size=30),
          plot.subtitle=element_text(size=24),
          plot.caption=element_text(size=12),
          legend.title = element_text(size=24),
          legend.text = element_text(size=18))
g=g+transition_time(time,range=c(1,365))+shadow_mark()#+enter_grow()
# Part 3: Create Sound ----
# create sound sample (src: https://freesound.org/people/swordofkings128/sounds/398039/)
w=readMP3('splash.mp3')
sam=soundSample(wave=w@left)
# order by time
floods=floods[order(floods$time),]
# Create audio track
s=sequence(sam,
           time=(floods$time/365)*duration, 
           volume=rescale(floods$Area,0.01,1), # Volume proportional to flood area
           pan=rescale(floods$lat>0,-1,1)) # North/South hemisphere <-> right/left channel 
writeWave(s, filename='floodMap.wav')
# Part 4: Render full video ----
nframes=round(duration*fps)
animate(g,nframes=nframes,duration=duration,width=1280,height=720,
        renderer = av_renderer('floodMap.mp4',audio='floodMap.wav'))



