library(ggplot2);library(patchwork)
library(dplyr)
library(sequenceR);library(av)

load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/pianoSteinway.RData')
inst=pianoSteinway
load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/bassStandup.RData')
bass=bassStandup
load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/drumkitStahl.RData')
drum=drumkitStahl

# Initial counting ---------------
getCounting <- function(bpm,compt,t0=0,ff=0.6,f=0.4,m=0.2,p=0.08,random_tim=0.02,random_vol=0.02){
  tp4=1/(bpm/60)
  tp16=tp4/4
  # XXX
  nT=compt*7
  dur=c(rep(tp16,nT))
  vol=rep_len(c(ff,p,p,p,f,p,p),nT)
  foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
  TIM=tim;VOL=vol
  TIM=(TIM-0.05)+rnorm(length(TIM),sd=tp16*random_tim)
  TIM[TIM<0]=0
  VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
  w=play.instrument(drum,notes=rep('hihat',length(TIM)),time=TIM,volume=VOL,nmax=20*10^6,fadein=rep(0,length(TIM)))
  writeWave(w,'count.wav')
  return(w)
}

# Piano ---------------
getPiano <- function(dat,bpm,t0,intro,type,pitchPar=1,volPar=10){
  tp4=1/(bpm/60)
  tp16=tp4/4
  if(type=='major'){
    scale=c('E2','B2','E3','Ab3','B3','Eb4','Gb4','A4','Db5','E5','Ab5')
  } else {
    scale=c('E2','B2','E3','G3', 'B3','E4', 'G4', 'B4','E5', 'G5','A5')
  }
  nsc=length(scale)
  wleft=wright=0
  stations=unique(dat$station)
  for(s in stations){
    message(paste0(s))
    # DF=dat %>% filter(station==s) %>% mutate(smooth=runmed(obs,31))
    DF=dat %>% filter(station==s) %>% mutate(smooth=runmed(p2,31))
    nT=NROW(DF)
    # u=rescale(sqrt(DF$smooth),1,0)^pitchPar
    u=(1-DF$smooth)^pitchPar
    notes=1+round(u*(nsc-1))
    vol=(1-DF$p)^volPar
    vol[1]=0 # to avoid starting with one huge chord
    # plot(notes,type='l',ylim=c(0,max(notes)))
    # lines(vol,col='red')
    foo=c(1,diff(notes))
    mask=foo!=0
    time=t0+7*tp16*intro+tp16*((1:nT)-1)[mask]
    ix=notes[mask]
    v=vol[mask]
    # plot(time,ix,type='h',ylim=c(0,max(notes)))
    # points(time,v,col='red',pch=19)
    
    wpiano=play.instrument(inst,notes=scale[ix],time=time,
                           fadeout=rep(1,length(time)),
                           volume=v,nmax=50*10^60)
    
    ndiff=length(wleft)-length(wpiano@left)
    if(ndiff<0){
      wleft=c(wleft,rep(0,-1*ndiff))
      wright=c(wright,rep(0,-1*ndiff))
      pleft=wpiano@left
      pright=wpiano@right
    } else {
      pleft=c(wpiano@left,rep(0,ndiff))
      pright=c(wpiano@right,rep(0,ndiff))
    }
    wleft=wleft+pleft
    wright=wright+pright
  }
  L=as.Wave(soundSample(wleft))
  R=as.Wave(soundSample(wright))
  return(list(left=L,right=R))
}

# Bass ---------------
getBass <- function(bpm,t0,intro,type,isLeap=FALSE,ff=1,f=0.9,m=0.8,p=0.5,random_tim=0.02,random_vol=0.02){
    tp4=1/(bpm/60)
    tp16=tp4/4
    if(type=='major'){
      # Intro 
      not=c('E2');nT=length(not)
      vol=c(p)*rbeta(nT,1/random_vol,1)
      dur=c(7*tp16*intro)
      fade=c(0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=tim;VOL=vol;NOT=not;FADE=fade
      # E 
      not=c('E2','E1','E2','E1','E2','E1');nT=length(not)
      vol=c(m,     m,  m,   m,m,m)*rbeta(nT,1/random_vol,1)
      dur=c(14*tp16,14*tp16,14*tp16,14*tp16,14*tp16,14*tp16)
      fade=c(Inf,0,Inf,0,Inf,0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # A
      not= c('A1','A1','A1','A1');nT=length(not)
      vol= c(m,   m,   m,m)*rbeta(nT,1/random_vol,1) 
      dur= c(14*tp16,14*tp16,14*tp16,14*tp16)
      fade=rep(0,length.out=nT)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # B - C#
      not= c('B1','B1','Db2','Db2','B1','B1','Db2','Db2');nT=length(not)
      vol=c(m,     m,    m,   m,   m,     m,    m,   m)*rbeta(nT,1/random_vol,1)
      dur=c(14*tp16,14*tp16,14*tp16,14*tp16,14*tp16,14*tp16,14*tp16,14*tp16)
      fade=rep(0,length.out=nT)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # G# - A
      not= c('Ab1','A1','Ab1','A1','Ab1','A1');nT=length(not)
      vol=c(m,     m,    m,   m,    m,    m)*rbeta(nT,1/random_vol,1)
      dur=c(7*tp16,7*tp16,7*tp16,7*tp16,7*tp16,7*tp16)
      fade=rep(0,length.out=nT)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # F#
      not=c('Gb2','Gb1','Gb2','Gb1');nT=length(not)
      vol=c(m,     m,  m,   m)*rbeta(nT,1/random_vol,1)
      dur=c(7*tp16,7*tp16,7*tp16,7*tp16)
      fade=rep(0,length.out=nT)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # G# - A - B
      not= c('Ab1','A1','B1','B2','B1','B1');nT=length(not)
      vol=c(m,     m,    m,   m,    m,    m)*rbeta(nT,1/random_vol,1)
      dur=c(7*tp16,7*tp16,7*tp16,7*tp16,7*tp16,7*tp16)
      dur[length(dur)]=dur[length(dur)]+tp16+tp16*isLeap
      fade=rep(0,length.out=nT)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    } else {
      # Intro 
      not=c('E2');nT=length(not)
      vol=c(p)*rbeta(nT,1/random_vol,1)
      dur=c(7*tp16*intro)
      fade=c(0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=tim;VOL=vol;NOT=not;FADE=fade
      # January 
      not=c('E3','E2','E3','E2');nT=length(not)
      vol= c(m,   m,   m,   m)*rbeta(nT,1/random_vol,1) 
      dur=c(11*tp16,5*tp16,11*tp16,4*tp16)
      fade=c(Inf,0,Inf,0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # February
      not=c('G2','E2','G2','E3');nT=length(not)
      vol= c(m,   m,   m,   m)*rbeta(nT,1/random_vol,1) 
      dur=c(11*tp16,5*tp16,4*tp16,8*tp16+ifelse(isLeap,tp16,0))
      fade=c(0,0,Inf,0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # March
      not=c('Db3','G2','Db2','E1','Db3','G2','Db2','E1','G1','B1');nT=length(not)
      vol= c(m,    m,   m,    m,   m,    m,   m,   m,    m,   m)*rbeta(nT,1/random_vol,1) 
      dur= c(4,    3,   4,    3,   4,    3,   4,   2,    2,   2)*tp16
      fade=c(0,    0,   0,    0,   0,    0,   0,   0,    0,   0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # April
      not=c('C2','Gb2','C2','G2','C2','Gb2','C2','Gb2','C2','A1');nT=length(not)
      vol= c(m,    m,   m,    m,   m,    m,   m,   m,    m,   m)*rbeta(nT,1/random_vol,1) 
      dur= c(4,    3,   4,    3,   4,    3,   4,   3,    1,   1)*tp16
      fade=c(0,    0,   0,    0,   0,    0,   0,   0,    0,   0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)     
      # May
      not=c('B1','Gb2','B1','G2','B1','B2','B1','G2','G2','E2');nT=length(not)
      vol= c(m,    m,   m,    m,   m,    m,   m,   m,    m,   m)*rbeta(nT,1/random_vol,1) 
      dur= c(4,    3,   4,    3,   4,    3,   4,   4,    1,   1)*tp16
      fade=c(0,    0,   0,    0,   0,    0,   0,   0,    0,   0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
      # June
      not=c('Gb2','G3','Gb2','Gb3','Gb2','D3','Gb2','E3','D3');nT=length(not)
      vol= c(m,    m,   m,    m,   m,     m,   m,   m,    m)*rbeta(nT,1/random_vol,1) 
      dur= c(4,    3,   4,    3,   4,     3,   4,   3,    2)*tp16
      fade=c(Inf,  Inf, Inf,  Inf, Inf,   Inf, 0,   0,    0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)    
      # July
      not=c('D3','B1','Gb3','B1','D3','Gb3','B1');nT=length(not)
      vol= c(m,    m,   m,   m,   m,   m,    m)*rbeta(nT,1/random_vol,1) 
      dur= c(5,    2,   1,   6,   7,   1,    6)*tp16
      fade=c(0,   0,   Inf,  0,   0,   Inf,  0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)    
      # August
      not=c('D3','B1','Gb3','B1','D3','Gb3','B1','D3');nT=length(not)
      vol= c(m,    m,   m,   m,   m,   m,    m,   m)*rbeta(nT,1/random_vol,1) 
      dur= c(5,    2,   1,   6,   7,   1,    6,   6)*tp16
      fade=c(0,   0,   Inf,  0,   0,   Inf,  0,   0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)    
      # September
      not=c('G1','D2','G2','D2','G1','D2','G2','D2','D2');nT=length(not)
      vol= c(m,   m,   m,   m,   m,   m,   m,   m,   m)*rbeta(nT,1/random_vol,1) 
      dur= c(3,   5,   3,   3,   3,   5,   3,   3,   2)*tp16
      fade=c(0,   0,   0,   0,   0,   0,   0,   0,   0)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)   
      # October
      not=c('A1','G1','A1','E2','A1','G1','A1','A2');nT=length(not)
      vol= c(m,   m,   m,   m,   m,   m,   m,   m  )*rbeta(nT,1/random_vol,1) 
      dur= c(4,   3,   4,   3,   4,   3,   4,   6  )*tp16
      fade=c(0,   0,   0,   0,   0,   0,   0,   0  )
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)   
      # November
      not=c('C2','Gb3','C2','E3','C2','D3','C2','C2','B1');nT=length(not)
      vol= c(m,   m,   m,   m,   m,   m,   m,   m,    m  )*rbeta(nT,1/random_vol,1) 
      dur= c(4,   3,   4,   3,   4,   3,   5,   2,    2  )*tp16
      fade=c(Inf, Inf, Inf, Inf, Inf, Inf, 0,   0,    0  )
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)  
      # December
      not=c('F2','F3','F2','D3','F2','C3','F2','A2','D3','E3');nT=length(not)
      vol= c(m,   m,   m,   m,   m,   m,   m,   m,   m,  m   )*rbeta(nT,1/random_vol,1) 
      dur= c(4,   3,   4,   3,   4,   3,   4,   4,   1,  1  )*tp16
      fade=c(Inf, Inf, Inf, Inf, Inf, Inf, 0,   0,   0,  0  )
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    }
    # randomize 
    TIM=TIM+rnorm(length(TIM),sd=tp16*random_tim)
    # avoid slight shift in bass instrument
    TIM=TIM-0.05      
    # Play it 
    w=play.instrument(bass,notes=NOT,time=TIM,volume=VOL,nmax=50*10^6,fadeout=FADE+0.2)
    writeWave(w,'bass.wav')
  return(w)
}

# Cymbals ---------------
getCymbal <- function(bpm,t0,intro,type,isLeap=FALSE,ff=0.6,f=0.4,m=0.2,p=0.08,random_tim=0.02,random_vol=0.02){
    tp4=1/(bpm/60)
    tp16=tp4/4
    if(type=='major'){
      # XXX
      nT=intro*7+ifelse(isLeap,366,365)
      dur=c(rep(tp16,nT));nT=length(dur)
      vol=rep_len(c(ff,p,0,0, f,p,0, 0,0,0,p, f,m,p, ff,p,0,0, f,p,0, 0,0,0,0, f,p,0),nT)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=tim;VOL=vol
      # avoid slight shift and randomize
      TIM=(TIM-0.05)+rnorm(length(TIM),sd=tp16*random_tim)
      VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
      w=play.instrument(drum,notes=rep('hihat',length(TIM)),time=TIM,volume=VOL,nmax=50*10^6,fadein=rep(0,length(TIM)))
    } else {
      # Intro
      dur=c(rep(tp16,intro*7));nT=length(dur)
      vol=rep_len(c(ff,p,0,0, f,p,0, 0,0,0,p, f,m,p, ff,p,0,0, f,p,0, 0,0,0,0, f,p,0),nT)
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=tim;VOL=vol
      # January
      vol=c(f,f,f,f,f,f,f,f,f,f,f)
      dur=c(3,3,2,3,3,2,3,3,3,3,3)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # February
      vol=c(f,f,f,f,f,f,f,f,f,f)
      dur=c(3,3,2,3,3,2,3,3,3,3+ifelse(isLeap,1,0))*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # March
      vol=c(f,m,f,f,m,f,f,m,f,f,m,f,f)
      dur=c(2,2,3,2,2,3,2,2,3,2,2,3,3)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # April
      vol=c(f,m,f,f,m,f,f,m,f,f,m,f,f)
      dur=c(2,2,3,2,2,3,2,2,3,2,2,3,2)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # May
      vol=c(f,m,f,f,m,f,f,m,f,f,m,f,m,f)
      dur=c(2,2,3,2,2,3,2,2,3,2,2,2,2,2)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # June
      vol=c(f,m,f,f,m,f,f,m,f,f,m,f,f)
      dur=c(2,2,3,2,2,3,2,2,3,2,2,3,2)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # July-August
      vol=c(f,m,f, f,m,f, 0.25*m,0.5*m,0.75*m,m,1.5*m,2*m)
      dur=c(5,9,14,5,9,14,1,     1,    1,     1,1    ,1  )*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # September
      vol=c(f,f,f,f,f,f,f,f,f,f,f)
      dur=c(3,3,2,3,3,3,3,2,3,3,2)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # October
      vol=c(0,m, m,f)
      dur=c(2,14,9,6)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # November
      vol=c(f,m,f,f,m,f,f,m,f,f,m,f)
      dur=c(2,2,3,2,2,3,2,2,3,3,2,4)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # December
      vol=c(f,m,f,f,m,f,f,m,f,f,m,f,m,f)
      dur=c(2,2,3,2,2,3,2,2,3,2,2,2,2,2)*tp16
      foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
      TIM=c(TIM,tim);VOL=c(VOL,vol)
      # avoid slight shift and randomize
      TIM=(TIM-0.05)+rnorm(length(TIM),sd=tp16*random_tim)
      VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
      w=play.instrument(drum,notes=rep('ride',length(TIM)),time=TIM,volume=VOL,nmax=50*10^6,fadein=rep(0,length(TIM)))
  }
  writeWave(w,'cymbal.wav')
  return(w)
}

# Drum kick ---------------
getDrumKick <- function(bpm,t0,intro,type,isLeap=FALSE,ff=1,f=0.8,m=0.6,p=0.3,random_tim=0.02,random_vol=0.02){
  tp4=1/(bpm/60)
  tp16=tp4/4
  if(type=='major'){
    
  } else {
 
  }
  # # avoid slight shift and randomize
  # TIM=(TIM-0.05)+rnorm(length(TIM),sd=tp16*random_tim)
  # VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
  # w=play.instrument(drum,notes=rep('bass',length(TIM)),time=TIM,volume=VOL,nmax=50*10^6,fadein=rep(0,length(TIM)))
  # writeWave(w,'drumkick.wav')
  # return(w)
}
