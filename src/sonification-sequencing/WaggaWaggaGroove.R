library(sequenceR) # https://github.com/benRenard/sequenceR
dur <- 9 # Duration of the animation is second
lowPrecip=450
highPrecip=800

# READ DATA ------
WaggaWagga=read.table('WaggaWagga.csv',header=TRUE,sep=';')

# SEQUENCING ------
# Timing
n <- NROW(WaggaWagga) # series size
tim <- dur*seq(0,1,length.out=n) # regular time vector between 0 and dur
# Temperature controls master volume
master <- rescale(WaggaWagga$Temperature,0.2,1) # temperature time series rescaled between 0.2 and 1
# Hi-hat rythmic pattern: groups of four 16th notes, the first one being accentuated.
every4=(((1:n)-1))%%4==0 # T F F F T F F F etc.
accents <- rescale(as.numeric(every4),0.2,1) # 1 0.2 0.2 0.2 1 0.2 0.2 0.2 etc.  
hh <- sequence(hiHat,time=tim,volume=master*accents) # create hi-hat sequence (hiHat sample is included in package sequenceR)
# Bass drum sequence: hit every time precipitation is lower than some threshold
mask=WaggaWagga$Precipitation<lowPrecip # time steps with low pp
k <- sequence(kick,time=tim[mask],volume=master[mask]) # play a kick at those time steps
# Snare sequence: hit every time precipitation is higher than some threshold
mask=WaggaWagga$Precipitation>highPrecip # time steps with high pp
s <- sequence(snare,time=tim[mask],volume=master[mask]) # play a snare at those time steps
# Mix hi-hat, kick and snare sequences
final <- mix(list(hh,k,s),volume=c(0.5,0.75,1))
writeWave(final,'WaggaWaggaGroove.wav') # write to disc

# ANIMATION ------
library(tidyr);library(ggplot2);library(gganimate)
# Modify the shape of the WaggaWagga dataset to facilitate plotting
DF <- pivot_longer(WaggaWagga,-Year) # function from tidyr
# Plot precipitation and temperature time series using ggplot
g <- ggplot(DF,aes(x=Year,y=value))
g <- g + geom_line(aes(color=name),size=1)+geom_point(size=4)
g <- g + scale_color_manual(values = c('blue','red'),guide=FALSE)
g <- g + facet_wrap(vars(name),ncol=1,scales='free_y')+theme_bw()
g <- g + geom_hline(data=data.frame(y=lowPrecip,name='Precipitation'),aes(yintercept=y))
g <- g + geom_hline(data=data.frame(y=highPrecip,name='Precipitation'),aes(yintercept=y))
# Make it look nicer
g <- g+theme_bw()+theme(axis.title=element_text(size=18), 
                        axis.text=element_text(size=14),
                        strip.text=element_text(size=18))
# Create an animated plot
g <- g + transition_reveal(Year)
# 'Render' the animated plot into a .mp4 movie
fps=n/dur # number of frames divided by duration
animate(g,nframes=NROW(WaggaWagga),fps=fps,width=1280,height=720,
        renderer = av_renderer('WaggaWaggaGroove.mp4',audio='WaggaWaggaGroove.wav'))
