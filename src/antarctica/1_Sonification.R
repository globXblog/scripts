library(sequenceR);library(ggplot2)
bpm=120
d0='2001-06-01' # d0='1994-06-01' for full period
dn='2003-05-31' # dn='2020-01-01' for full period
dataOnly=FALSE # if true, do not create sound, just output volume data
t_fadein=3 # fade-in (used for both sounds and animation) in seconds
t_fadeout=8 # same for fade-out

source('funk.R')
Antarctica=read.table('Antarctica.txt',header=TRUE,colClasses=c('Date',rep('numeric',5)))
yearDuration=(365.25*60)/(4*bpm)
DF=Antarctica[which(Antarctica$date==d0):which(Antarctica$date==dn),]
g=ggplot(tidyr::pivot_longer(DF,-date,names_to='variable'))
g=g+geom_line(aes(x=date,y=value))
g=g+facet_wrap(vars(variable),ncol=1,scales='free_y')
g+theme_bw()

days=as.numeric(substr(DF$date,9,10))
months=as.numeric(substr(DF$date,6,7))
n=NROW(DF)
tim=((0:(n-1))/365.25)*yearDuration

# Create each instrument
piano=writePiano(DF,dataOnly=dataOnly);gc()
bass=writeBass(DF,dataOnly=dataOnly);gc()
drums=writeDrums(DF,dataOnly=dataOnly);gc()
harmonics=writeHarmonics(DF,dataOnly=dataOnly);gc()
guitar=writeGuitar(DF,dataOnly=dataOnly);gc()
organ=writeOrgan(DF,dataOnly=dataOnly);gc()

# data frame for animation
animationDF=cbind(drums['date'],drums[,c('Radiation','Radiation_v')],
                  drums[,c('RelativeHumidity','RelativeHumidity_v')],
                  harmonics[,c('Streamflow','Streamflow_v')],
                  guitar[,c('Temperature','Temperature_v')],
                  bass[,c('Windspeed','Windspeed_v')])
write.table(animationDF,'AntarcticaAnimation.txt',row.names=FALSE)

# Mix them all
drums=readMP3('drums.mp3')
bass=readMP3('bass.mp3')
guitar=readMP3('guitar.mp3')
organ=readMP3('organ.mp3')
harmonics=readMP3('harmonics.mp3')
piano=readMP3('piano.mp3')
final=mix(list(drums,bass,bass,guitar,organ,harmonics,piano),
          volume=c(1,0.36,0.36,0.15,0.15,0.25,0.25),pan=c(0,-1,1,-0.7,0.7,0,0))
# Create fade-in and fade-out
n_fadein=round(t_fadein*final@samp.rate)
n_fadeout=round(t_fadeout*final@samp.rate)
n=length(final@left)
vol=rep(1,n)
vol[1:n_fadein]=seq(0,1,length.out=n_fadein)
vol[(n-n_fadeout+1):n]=seq(1,0,length.out=n_fadeout)
final@left=final@left*vol
final@right=final@right*vol
# Save file
writeMP3(final,'finalMix.mp3')
