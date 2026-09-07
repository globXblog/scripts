library(tidyr);library(dplyr)
library(ggplot2);library(patchwork)
library(sequenceR);library(musicXML);library(tuneR)
load('extent_monthly.RData')
DF=DF %>% filter(year>1978,year+month-2>1978) %>% arrange(region,year,month) %>%
  group_by(region) %>% mutate(dextent=c(0,diff(extent)),time=year+(month-0.5)/12) %>%
  group_by(region) %>% mutate(smooth=predict(loess(extent~time),newdata=time),
                              backbeat0=(month==5 & region=='S') | (month==11 & region=='N'),
                              backbeat=backbeat0*smooth+max(smooth,na.rm=TRUE)*!backbeat0) %>%
  group_by(region,month) %>% mutate(rank=rank(extent))

stats=DF %>% group_by(region,year) %>% 
  summarise(amean=mean(anomaly,na.rm=TRUE),
            min=min(extent,na.rm=TRUE),
            imin=which.min(extent))



load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/pianoSteinway.RData')
inst=pianoSteinway;rm(pianoSteinway)



ggplot(DF)+
  geom_line(aes(time,anomaly,group=region,color=region))+
  theme_bw()

lim=0.75
ggplot(stats)+
  geom_point(aes(year,amean,group=region,color=region))+
  geom_hline(yintercept=lim*c(-1,1))+
  theme_bw()



years=min(DF$year):max(DF$year)
North=list(
  minor=list(
    c('E1','E2','Bb2','E3','Bb3','Db4','E4','G4'),
    c('E1','E2','C3','Gb3','A3','C4','E4','A4'),
    c('E1','E2','B2','E3','Gb3','G3','B3','E4')
  ),
  neutral=list(
    c('Gb1','Gb2','A2','E3','A3','B3','E4','Gb4'),
    c('A1','A2','E3','A3','B3','E4','Gb4','A4'),
    c('E1','E2','B2','E3','A3','B3','E4','Gb4')
    ),
  major=list(
    c('Gb1','Gb2','A2','E3','A3','Db4','E4','Gb4'),
    c('A1','A2','E3','A3','B3','E4','Ab4','A4'),
    c('E1','E2','B2','Gb3','Ab3','B3','E4','Gb4')
  )
)
South=North
variant=rep(c(1,2,3,3),length.out=length(years))

target='extent'
w=list()
for(reg in c('N','S')){
  if(reg=='S'){notes=South} else {notes=North}
  nlist=vector('list',length(years))
  for(i in 1:length(years)){
    y=years[i]
    foo=DF %>% filter(year==y,region==reg)
    m=mean(foo$anomaly,na.rm=TRUE)
    if(m <= -1*lim){type='minor'} else if(m<=lim){type='neutral'} else(type='major')
    values=c(foo[[target]],range(DF[DF$region==reg,target],na.rm=TRUE))
    ns=sequenceR::pitchMapping(values,notes[[type]][[variant[i]]])
    vol=rescale(-1*c(foo$anomaly,range(DF$anomaly[DF$region==reg],na.rm=TRUE)),0,1)*
      rescale(-1*foo$extent,0.2,1)
    nlist[[i]]=data.frame(time=foo$time,
                          note=ns[1:(length(ns)-2)],
                          volume=vol[1:(length(vol)-2)])
  }
  melody=bind_rows(nlist);n=NROW(melody)
  melody$diff=c(TRUE,melody$note[2:n]!=melody$note[1:(n-1)])
  melody=melody %>% na.omit() %>% filter(diff==TRUE)
  
  w[[reg]]=play.instrument(inst=inst,notes=melody$note,
                           time=(melody$time-min(years))*2,
                           volume=melody$volume,
                           fadein=rep(0,NROW(melody)),
                           fadeout=rep(Inf,NROW(melody)))
}

final=mix(w,pan=c(-1,1))
writeWave(final,'atej.wav')


if(FALSE){
  notes=list(N=c('E4','Gb4','G4','A4','B4',
                 'C5','Eb5','E5','Gb5','G5','A5','B5',
                 'C6','Eb6','E6','Gb6','G6','A6','B6',
                 'C7','Eb7','E7'),
             S=c('E1','E1','G1','A1','B1','C2','E2','B2','E3'))
  
  # # With drumkit?
  # notes=list(N=c('B3','Eb3','B3','G4','Db6','Eb5','Ab4','G4','Bb1'),
  #            S=c('B1','C2','A2','B2','D2','E2'))
  
  final=list()
  for(what in c('N','S')){
    d=DF %>% filter(region==what) %>% arrange(year,month)
    x=d$extent^3;
    mask=is.na(x)
    x[mask]=mean(x,na.rm=TRUE)
    plot(x,type='l')
    ps=pitchMapping(x,notes[[what]])
    # x=-1*rescale(d$sanomaly)*rescale(d$extent)
    # x=-1*rescale(d$extent)
    x=-1*rescale(d$sanomaly)
    x[mask]=mean(x,na.rm=TRUE)
    vs=loudnessMapping(x,10,120)
    vs[mask]=0
    plot(vs,type='l')
    final[[what]]=getNotes(pitches=ps,loudnesses=vs)
    if(TRUE){final[[what]]=final[[what]] %>% tieNotes()}
    final[[what]]=final[[what]] %>% getMeasures(6,8)
  }
  s=score(final)
  writeMXL(s,'TwoPoles.xml')
}

# notes=list(N=list(c('E4','B4','Eb5','E5','E5','B5','Eb6','E6'),
#                   c('Gb4','B4','E5','Gb5','Gb5','B5','E6','Gb6'),
#                   c('Ab4','B4','E5','E5','Ab5','B5','E6','E6'),
#                   c('A4','Db5','E5','Gb5','A5','Db5','E6','Gb6'),
#                   c('B4','E5','Gb5','Ab5','B5','E6','Gb6','Ab6')),
#            S=list(c('E4','B4','Eb5','E5','E5','B5','Eb6','E6'),
#                   c('Gb4','B4','E5','Gb5','Gb5','B5','E6','Gb6'),
#                   c('Ab4','B4','E5','E5','Ab5','B5','E6','E6'),
#                   c('A4','Db5','E5','Gb5','A5','Db5','E6','Gb6'),
#                   c('B4','E5','Gb5','Ab5','B5','E6','Gb6','Ab6')))

if(FALSE){
  notes=list(N=list(c('E5','Gb5','A5','B5','E6','Gb6','A6','B6')),
             S=list(c('E3','Gb3','A3','B3','E4','Gb4','A4','B4')))
  
  final=list()
  for(what in c('N','S')){
    d=DF %>% filter(region==what) %>% arrange(year,month) %>% mutate(myear=year-(month<2))
    minis = d%>%group_by(myear)%>%summarise(extent=min(extent,na.rm=TRUE))%>%
      mutate(ix=ceiling(rescale(extent,0.01,1)*length(notes[[what]])))
    # x=d$extent
    x=-1*d$anomaly
    mask=is.na(x)
    x[mask]=mean(x,na.rm=TRUE)
    plot(x,type='b')
    years=unique(d$myear)
    ps=c()
    for(i in 1:length(years)){
      ns=notes[[what]][[minis$ix[i]]]
      ps=c(ps,pitchMapping(x[d$myear==years[i]],ns))
    }
    # x=-1*rescale(d$sanomaly)*rescale(d$extent)
    x=-1*rescale(d$extent)
    # x=-1*rescale(d$sanomaly)
    x[mask]=mean(x,na.rm=TRUE)
    vs=loudnessMapping(x,10,120)
    vs[mask]=0
    plot(vs,type='b')
    final[[what]]=getNotes(pitches=ps,loudnesses=vs)
    if(TRUE){
      final[[what]]=final[[what]] %>% tieNotes()
      }
    final[[what]]=final[[what]] %>% getMeasures(6,8)
  }
  s=score(final)
  writeMXL(s,'TwoPoles.xml')
}

if(FALSE){
  # DRUMS
  getDrumPart <- function(y,p,pitch,lMin=1,lMax=120,doPlot=TRUE,requireSameLoudness=TRUE){
    u=quantile(y,na.rm=TRUE,probs=p)
    x=(y<=u) * (u-y)
    x[is.na(x)]=0
    if(doPlot){plot(x,type='b')}
    out=getNotes(pitches=pitchMapping(x,pitch),
                 loudnesses=loudnessMapping(x,lMin,lMax)) %>% 
      tieNotes(requireSameLoudness=requireSameLoudness) %>% getMeasures(6,8)
    return(out)
  }
  final=list();k=0
  # South Pole percussions
  mask=DF$region=='S'
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=1.5/12,pitch='C2') # kick
  k=k+1;final[[k]]=getDrumPart(y=DF$backbeat[mask],p=1/12,pitch=c(rep('Db2',10),'Eb2')) # snare
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=6/12,pitch='Gb2') # hihat
  k=k+1;final[[k]]=getDrumPart(y=DF$dextent[mask],p=1/12,pitch='F3') # ride 1
  k=k+1;final[[k]]=getDrumPart(y=-1*DF$dextent[mask],p=1/12,pitch='Eb3',lMin=20) # ride 2
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=0.5/12,pitch='Db3') # crash
  k=k+1;final[[k]]=getDrumPart(y=DF$anomaly[mask],p=1/12,pitch=c('G2','F2')) # Toms
  # North Pole percussions
  mask=DF$region=='N'
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=1.5/12,pitch='A1') # kick
  k=k+1;final[[k]]=getDrumPart(y=DF$backbeat[mask],p=1/12,pitch=c(rep('Db2',10),'E2')) # snare
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=6/12,pitch='Eb4') # hihat 
  k=k+1;final[[k]]=getDrumPart(y=DF$dextent[mask],p=1/12,pitch='F3',lMin=20) # ride 1
  k=k+1;final[[k]]=getDrumPart(y=-1*DF$dextent[mask],p=1/12,pitch='Eb3',lMin=20) # ride 2
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=0.5/12,pitch='A3') # crash
  k=k+1;final[[k]]=getDrumPart(y=DF$anomaly[mask],p=1/12,pitch=c('C3','B2','A2')) # Toms
  
  s=score(final)
  writeMXL(s,'drums.xml')
  
  final=list();k=0
  # South Pole Bass
  mask=DF$region=='S'
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=3/12,pitch=c('D3','E3','C3','B2','A2','G2','E2'),requireSameLoudness=FALSE)
  # North Pole Bass
  mask=DF$region=='N'
  k=k+1;final[[k]]=getDrumPart(y=DF$extent[mask],p=3/12,pitch=c('D3','E3','C3','B2','A2','G2','E2'),requireSameLoudness=FALSE)
  
  s=score(final)
  writeMXL(s,'bass.xml')
}
