# Select audio file
audioFile='WaggaWaggaMelody.mp3' # 'WaggaWaggaMelody_v0.mp3'
bpm=60 # tempo (in beats per minute). 120 for WaggaWaggaMelody_v0.mp3.
# Read data
WaggaWagga=read.table('WaggaWagga.csv',header=TRUE,sep=';')
# Animation
library(tidyr);library(gganimate)
DF <- pivot_longer(WaggaWagga,-Year) # function from tidyr
DF <- cbind(DF,frame=DF$Year) # add 'frame' column controlling animation
# Add frames at the end to extend animation and match audio length
nXtraFrames <- 1+4*4*2
indx=c(NROW(DF)-1,NROW(DF))
DF=rbind(DF,data.frame(Year=rep(DF$Year[indx],nXtraFrames),
                       name=rep(DF$name[indx],nXtraFrames),
                       value=rep(DF$value[indx],nXtraFrames),
                       frame=DF$Year[indx]+rep(1:nXtraFrames,each=2)))
# Plot precipitation and temperature time series using ggplot
g <- ggplot(DF,aes(x=Year,y=value))
g <- g + geom_line(aes(color=name),size=1)
g <- g + scale_color_manual(values = c('blue','red'),guide=FALSE)
g <- g + facet_wrap(vars(name),ncol=1,scales='free_y')
# Make it look nicer
g <- g+theme_bw()+theme(axis.title=element_text(size=18), 
                        axis.text=element_text(size=14),
                        strip.text=element_text(size=18))
# Static plot
png(filename='WaggaWaggaData.png',width=(16/9)*150,height=150,unit='mm',res=300)
g0=g+geom_point(aes(color=name),size=3,alpha=0.5)
print(g0)
dev.off()
# Create an animated plot
g <- g  +geom_point(size=4) + transition_reveal(frame)
# 'Render' the animated plot into a .mp4 movie
bps=(bpm/60) # convert to beats per second
fps=bps*4 # convert to frames per second (there are 4 16th note per beat)
animate(g,nframes=NROW(DF)/2,fps=fps,width=1280,height=720,
        renderer = av_renderer('WaggaWaggaMelody.mp4',audio=audioFile))
