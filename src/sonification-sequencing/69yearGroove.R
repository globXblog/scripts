library(sequenceR) # https://github.com/benRenard/sequenceR
library(BFunk) # https://github.com/benRenard/BFunk
library(gganimate);library(ochRe)


#***********************************************
# Define tempo ----
bpm=120
dataPerBeat=4

#***********************************************
# Define data ----
X=read.table('69yearData.csv',header=T,sep=';')
DF=data.frame()
seas=1:3 # first season (JFM)
nbs=12/length(seas) # number of seasons
for(i in 1:nbs){
  hyear=(i-1)*length(seas)+seas # JFM for i=1, AMJ for i=2, etc.
  for(j in 1:3){ # 3 variables: P, T, SLP
    Z=data.frame(year=X$year,month=X$month,value=X[,2+j])
    temp=getAnnualVariables(Z,hydroyear=hyear,variables='mean') # seasonal mean
    DF=rbind(DF,data.frame(year=temp$year,season=i,
                           time=temp$year+(i-0.5)/nbs,
                           variable=names(X)[2+j],
                           value=temp$value))
  }
}
DF=DF[order(DF$time),]

#***********************************************
# Create sequences ----
n=NROW(DF)/length(levels(DF$variable)) # number of time steps
duration=(n/dataPerBeat)*(60/bpm) # duration of audio track
x=seq(0,duration,length.out=n) # timing
# Hi-hat sequence
y=DF$value[DF$variable=='T_Marseille']
hh <- sequence(hiHat,time=x,volume=rescale(y,0.1,1)) 
# Bass Drum sequence
y=DF$value[DF$variable=='P_Marseille']
P0=quantile(y,1.5/8)
mask=y<P0
k <- sequence(kick,time=x[mask],volume=rescale(-1*y[mask],0.3,1))
# Snare sequence
y=DF$value[DF$variable=='SLP_Marseille']
SLP0=quantile(y,6.5/8)
mask=y>SLP0
s <- sequence(snare,time=x[mask],volume=rescale(y[mask],0.3,1))
# Mix
audio=mix(list(hh,k,s),volume=c(0.5,0.8,0.8))
writeWave(audio,'69yearGroove.wav')

#***********************************************
# Plotting ----
# change and reorder variable names
levels(DF$variable) <- c('3_P','1_T','2_SLP')
DF$variable <- ordered(DF$variable,c('1_T','2_SLP','3_P'))
# define labels
labeller <- as_labeller(c(`1_T`="Temperature [C]",
                          `2_SLP`="Pression atmospherique [hPa]",
                          `3_P`="Precipitation [mm]"))
# Static plot
g=ggplot(DF,aes(x=time,y=value,color=variable))+geom_line(size=1)
g=g+geom_point(size=4)
g=g+scale_color_ochre(palette='healthy_reef')
g=g+facet_wrap(vars(variable),scales='free_y',labeller=labeller,nrow=3)
# add thresholds
g=g+geom_hline(data=data.frame(y=SLP0,variable='2_SLP'),aes(yintercept=y))
g=g+geom_hline(data=data.frame(y=P0,variable='3_P'),aes(yintercept=y))
# permanent points for temperature
temp=DF[DF$variable=='1_T',]
temp=cbind(temp,size=rescale(temp$value))
g=g+geom_point(data=temp,aes(x=time,y=value,group=seq_along(time),size=size),alpha=0.3) 
# permanent points for SLP
temp=DF[DF$variable=='2_SLP' & DF$value>SLP0,]
temp=cbind(temp,size=rescale(temp$value))
g=g+geom_point(data=temp,aes(x=time,y=value,group=seq_along(time),size=size),alpha=0.3) 
# permanent points for SLP
temp=DF[DF$variable=='3_P' & DF$value<P0,]
temp=cbind(temp,size=rescale(-1*temp$value))
g=g+geom_point(data=temp,aes(x=time,y=value,group=seq_along(time),size=size),alpha=0.3) 
g=g+scale_size(range=c(1,7))
# themes
g=g+xlab("Temps")+ylab("Valeur")
g=g+theme_bw()
g=g+theme(axis.title=element_text(size=18), 
          axis.text=element_text(size=14),
          strip.text=element_text(size=18),
          legend.position='none')
# Create an animated plot
ganim <- g + transition_reveal(time)
# 'Render' the animated plot into a .mp4 movie
fps=n/duration # number of frames divided by duration
animate(ganim,nframes=n,fps=fps,width=1280,height=720,
        renderer = av_renderer('69yearGroove.mp4',audio='69yearGroove.wav'))
