library(ggplot2);library(dplyr);library(sequenceR);library(av)
library(BFunk)

# Guitar ---------------
getGuitar <- function(bpm,ff=1,f=0.8,m=0.5,p=0.3,random_tim=0.02,random_vol=0.02){
  if(file.exists('guitar.wav')){
    w=readWave('guitar.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/guitarPhilharmonia.RData')
    guitar=guitarPhilharmonia
    tp4=1/(bpm/60)
    tp16=tp4/4
    # Intro 1
    nT=16*2
    tim=tp16*((1:nT)-1)+c(0,rnorm(nT-1,sd=tp16*random_tim))
    vol=rep(c(f,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1) #*seq(0.1,1,length.out=nT)
    not=rep(c('B3','B4','B3','B3','B4','B3','B3','B4'),length.out=nT)
    TIM=tim;VOL=vol;NOT=not
    # Intro 2 
    nT=16*2
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(m,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1)
    not=rep(c('E3','E4','B3','B3','E4','B3','D4','E4'),length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    # Main theme 
    nT=16*2*2*4
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(ff,p,p,f,p,p,f,p,rep(c(m,p,p,f,p,p,f,p),7)),4)*rbeta(nT,1/random_vol,1)
    not=rep(
      c('E2','E4','B3','B3','E4','B3','D4','E4',
        rep(c('E3','E4','B3','B3','E4','B3','D4','E4'),3),
        rep(c('Gb3','E4','B3','A3','E4','B3','E4','E4'),4)),
      4)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    # Descending part 
    nT=16*2
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(f,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1)
    not=rep(c('A3','E4','B3','C4','E4','B3','G4','E4'),length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    not=rep(c('Ab3','E4','B3','D4','E4','B3','Gb4','E4'),length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    not=c(rep(c('G3','E4','B3','D4','E4','B3','Gb4','E4'),length.out=nT/2),
          rep(c('Gb3','E4','B3','D4','E4','B3','Gb4','E4'),length.out=nT/2))
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    not=rep(c('F3','E4','B3','D4','E4','B3','F4','E4'),length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    not=rep(c('F3','E4','B3','B3','E4','B3','F4','E4'),length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    # Flamenco part 
    nT=16*2*3
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(ff,p,p,f,p,p,f,p,f,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1)
    not=rep(c('E2','E4','B3','B3','E4','B3','E4','E4','E3','E4','B3','B3','E4','B3','E4','E4',
              'E2','E4','B3','B3','E4','B3','F4','E4','E3','E4','B3','B3','E4','B3','F4','E4'),
            length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    nT=16*2
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(ff,p,p,f,p,p,f,p,f,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1)
    not=rep(c('E2','E4','B3','B3','E4','B3','E4','E4','E3','E4','B3','B3','E4','B3','E4','E4'),length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    # Cuban part 
    nT=16*2*4
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(f,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1)
    not=c('C3','E4','B3','E3','E4','B3','C4','E4',
          'B2','E4','B3','Eb3','E4','B3','A3','E4',
          'E2','E4','B3','E3','E4','B3','G3','E4',
          'Db3','E4','B3','G3','E4','B3','A3','E4',
          'C3','E4','B3','E3','E4','B3','C4','E4',
          'B2','E4','B3','Eb3','E4','B3','A3','E4',
          'E2','E3','G3','B3','E4','D4','B3','G3',
          'G3','E4','B3','Gb3','E4','B3','E3','E4',
          'C3','E4','B3','E3','E4','B3','C4','E4',
          'B2','E4','B3','Eb3','E4','B3','A3','E4',
          'E2','E4','B3','E3','E4','B3','G3','E4',
          'Db3','E4','B3','G3','E4','B3','A3','E4',
          'C3','E4','B3','E3','E4','B3','C4','E4',
          'B2','E4','B3','Eb3','E4','B3','A3','E4',
          'E2','E4','B3','E3','E4','B3','G3','E4',
          'G2','E4','B3','E3','E4','B3','G3','E4')
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    # outro 
    nT=16*2*4
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(f,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1)
    not=c(rep(c('Db3','E4','B3','G3','E4','B3','A3','E4'),4),
          rep(c('C3','E4','B3','G3','E4','B3','A3','E4'),2),
          rep(c('B2','E4','B3','Gb3','E4','B3','A3','E4'),2),
          rep(c('Bb2','E4','B3','G3','E4','B3','A3','E4'),4),
          c('B2','E4','B3','Gb3','E4','B3','G3','E4',
            'B2','E4','B3','Gb3','E4','B3','A3','E4',
            'B2','E4','B3','Gb3','E4','B3','B3','E4',
            'B2','E4','B3','Gb3','E4','B3','B3','E4'))
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    # Repeat
    TIM=c(TIM,tp16+TIM+max(TIM));VOL=c(VOL,VOL);NOT=c(NOT,NOT)
    # END
    nT=16*8
    tim=tp16*(1:nT)+rnorm(nT,sd=tp16*random_tim)
    vol=rep(c(f,p,p,f,p,p,f,p),length.out=nT)*rbeta(nT,1/random_vol,1)*c(rep(1,nT/4),seq(1,0,length.out=3*nT/4))^2
    not=rep(c('B3','B4','B3','B3','B4','B3','B3','B4'),length.out=nT)
    TIM=c(TIM,tim+max(TIM));VOL=c(VOL,vol);NOT=c(NOT,not)
    w=play.instrument(guitar,notes=NOT,time=TIM,volume=VOL,fadeout=rep(Inf,length(NOT)),nmax=20*10^6)
    writeWave(w,'guitar.wav')
  }
  return(w)
}

# Bass ---------------
getBass <- function(bpm,ff=1,f=0.8,m=0.6,p=0.3,random_tim=0.02,random_vol=0.02){
  if(file.exists('bass.wav')){
    w=readWave('bass.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/bassStandup.RData')
    bass=bassStandup
    tp4=1/(bpm/60)
    tp16=tp4/4
    tp2=2*tp4
    tp1=4*tp4
    t0=tp1*4
    # Main theme 
    not=c('E2', 'Gb1','E1', 'Gb1',  rep('Gb2',8),    'E2', 'Gb1','E1', 'Gb1',  rep('Gb2',8));nT=length(not)
    vol=c(f,    m,    m,    m,      m,0,p,m,0,m,0,p, m,    m,    m,    m,      m,0,p,m,0,m,0,p)*rbeta(nT,1/random_vol,1) 
    dur=c(2*tp1,2*tp1,2*tp1,1.5*tp1,rep(tp16,8),     2*tp1,2*tp1,2*tp1,1.5*tp1,rep(tp16,8))
    fade=rep(Inf,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=tim;VOL=vol;NOT=not;FADE=fade
    # Descending part 
    not= c('A2',  'C3',  'G3',   'Ab2', 'D3',  'Gb3',  'G2',  'D3',  'Gb3',  'Gb2', 'D3',  'Gb3',  'F2',  'D3',  'F3',  'F2',  'D3',  'F3');nT=length(not)
    vol= c(f,     p,     f,      f,     p,     f,      f,     p,     f,      f,     p,     f,      f,     p,     f,      f,     m,     f)*rbeta(nT,1/random_vol,1) 
    dur= c(3*tp16,3*tp16,26*tp16,3*tp16,3*tp16,26*tp16,3*tp16,3*tp16,10*tp16,3*tp16,3*tp16,10*tp16,3*tp16,3*tp16,10*tp16,3*tp16,3*tp16,10*tp16)
    fade=c(Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    # Transition to Flamenco part
    not= c('F2',  'F2',  'B2','F3','B2','F2',  'F2', 'B2', 'F3','F3','F2',  'F2',  'B2','F3','B2','F3','F3','B2','F3','F2','F2','F3','F3');nT=length(not)
    vol= c(ff,    f,     p,   f,   f,   f,     f,     p,   f,   m,   ff,    f,     p,   f,   f,   f,   f,   p,   f,   p,   p,   f,  f)*rbeta(nT,1/random_vol,1) 
    dur= c(3*tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16)
    fade=rep(0,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    # Flamenco part 
    not= rep(c('E1',  'E3',  'D3','E3','B2','E2','E3','D3','E3',  'D2','E2','B1','E1',  'E3',  'E3','G3','B2','E2','F3','D3','F3',  'D2','E2','B1'),3);nT=length(not)
    dur= rep(c(3*tp16,2*tp16,tp16,tp16,tp16,tp16,tp16,tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,tp16,tp16,tp16,tp16,tp16,2*tp16,tp16,tp16,tp16),3)
    vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
    fade=rep(0,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    not= c('E1',  'E3',  'D3','E3','B2','E1',  'E3',  'D3','E3',  'E2',  'E2',  'E2',  'E2',  'A1', 'B1');nT=length(not)
    dur= c(3*tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,2*tp16,3*tp16,3*tp16,2*tp16,3*tp16,3*tp16,2*tp16)
    vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
    fade=rep(0,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    # Cuban part 
    # not= c('C2',  'B2',  'A2',  'Eb2', 'G2',  'Gb2', 'E2',  'G2',  'A2',  'B2',  'A2',  'Eb2', 'E2',  'D3',  'C2');nT=length(not)
    not= c('C2',  'E2',  'B1',  'Eb2', 'E2',  'G2',  'Db2', 'G2',  'A2',  'E2',  'B1',  'Eb2', 'E2',  'G2',  'D3',  'G2',  'Gb2', 'E2');nT=length(not)
    dur= c(3*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,2*tp16,3*tp16,3*tp16,2*tp16)
    vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
    fade=rep(0,length.out=nT);fade[15]=Inf
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    not= c('C2',  'B2',  'A2',  'Eb2', 'E2',  'G2',  'Db2', 'A2',  'C2',  'E2',  'B1',  'Eb2', 'E2',  'B1',  'G2',  'A2',  'Db2');nT=length(not)
    dur= c(3*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,2*tp16)
    vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
    fade=rep(0,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    # outro
    not= c('Db2', 'Db2', 'Db2',  'Db2', 'Db2',  'C2',  'D3',  'G3',   'B1',  'Gb3');nT=length(not)
    dur= c(3*tp16,3*tp16,10*tp16,6*tp16,10*tp16,3*tp16,3*tp16,10*tp16,6*tp16,10*tp16)
    fade=c(0,     0,     0,      0,     0,      Inf,   0,     Inf,    Inf,   Inf)
    vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    not= c('Bb1', 'A3',  'Bb2', 'Bb1',  'B2',   'B1');nT=length(not)
    dur= c(6*tp16,10*tp16,6*tp16,10*tp16,16*tp16,16*tp16)
    vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
    fade=rep(0,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
    # repeat 
    TIM=c(TIM,t0+TIM);VOL=c(VOL,VOL);NOT=c(NOT,NOT);FADE=c(FADE,FADE)
    # randomize 
    TIM=TIM+rnorm(length(TIM),sd=tp16*random_tim)
    # avoid slight shift in bass instrument
    TIM=TIM-0.05
    # Play it 
    w=play.instrument(bass,notes=NOT,time=TIM,volume=VOL,nmax=20*10^6,fadeout=FADE+0.2)
    writeWave(w,'bass.wav')
  }
  return(w)
}

# Drum kick ---------------
getDrumKick <- function(bpm,ff=1,f=0.8,m=0.6,p=0.3,random_tim=0.02,random_vol=0.02){
  if(file.exists('drumkick.wav')){
    w=readWave('drumkick.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/drumkitStahl.RData')
    drum=drumkitStahl
    ff=1;f=0.8;m=0.6;p=0.3;random_tim=0.02;random_vol=0.02
    tp4=1/(bpm/60)
    tp16=tp4/4
    tp2=2*tp4
    tp1=4*tp4
    t0=tp1*4
    # Main theme 
    nT=8
    vol=rep(p,nT)
    dur=rep(2*tp1,nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=tim;VOL=vol
    # Descending part 
    dur=c(rep(c(6*tp16,26*tp16),2),rep(c(6*tp16,10*tp16),4));nT=length(dur)
    vol=rep(m,nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # Flamenco part
    dur=c(rep(c(3*tp16,13*tp16),9),rep(c(3*tp16,5*tp16),2));nT=length(dur)
    vol=rep(f,nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # Cuban part 
    dur=rep(c(3*tp16,3*tp16,2*tp16),16);nT=length(dur)
    vol=rep(c(0,m,m),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # outro
    dur= c(3*tp16,3*tp16,10*tp16,6*tp16,10*tp16,3*tp16,3*tp16,10*tp16,6*tp16,10*tp16,6*tp16,10*tp16,6*tp16,10*tp16,16*tp16,16*tp16);nT=length(dur)
    vol= rep(c(m,p),each=nT/2)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # repeat 
    TIM=c(TIM,t0+TIM);VOL=c(VOL,VOL)
    # avoid slight shift and randomize
    TIM=(TIM-0.05)+rnorm(length(TIM),sd=tp16*random_tim)
    VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
    w=play.instrument(drum,notes=rep('bass',length(TIM)),time=TIM,volume=VOL,nmax=20*10^6,fadein=rep(0,length(TIM)))
    writeWave(w,'drumkick.wav')
  }
  return(w)
}

# Hi-hat ---------------
getHiHat <- function(bpm,ff=0.6,f=0.6,m=0.3,p=0.08,random_tim=0.02,random_vol=0.02){
  if(file.exists('hihat.wav')){
    w=readWave('hihat.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/drumkitStahl.RData')
    drum=drumkitStahl
    tp4=1/(bpm/60)
    tp16=tp4/4
    tp2=2*tp4
    tp1=4*tp4
    t0=tp1*20
    # descending part 
    dur=c(rep(tp2,2*6),rep(tp4,4*2));nT=length(dur)
    vol=rep(2*p,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=tim;VOL=vol
    # Flamenco part
    dur=rep(tp16,16*10);nT=length(dur)
    vol=rep(c(ff,p,p,p),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # Cuban part 
    dur=rep(tp16,16*8);nT=length(dur)
    vol=rep(c(f,p,p,m),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # outro
    dur=rep(tp16,16*8);nT=length(dur)
    vol=rep(c(f,p,p,m),length.out=nT)*seq(1,0.05,length.out=nT)^0.3
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # repeat 
    TIM=c(TIM,t0+TIM);VOL=c(VOL,VOL)
    # avoid slight shift and randomize
    TIM=(TIM-0.05)+rnorm(length(TIM),sd=tp16*random_tim)
    VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
    w=play.instrument(drum,notes=rep('hihat',length(TIM)),time=TIM,volume=VOL,nmax=20*10^6,fadein=rep(0,length(TIM)))
    writeWave(w,'hihat.wav')
  }
  return(w)
}

# Ride ---------------
getRide <- function(bpm,ff=0.6,f=0.6,m=0.3,p=0.08,random_tim=0.02,random_vol=0.02){
  if(file.exists('ride.wav')){
    w=readWave('ride.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/drumkitStahl.RData')
    drum=drumkitStahl
    tp4=1/(bpm/60)
    tp16=tp4/4
    tp2=2*tp4
    tp1=4*tp4
    t0=tp1*2
    # Enter 
    dur=c(2*tp1);nT=length(dur)
    vol=rep(ff,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=tim;VOL=vol
    # Main theme 
    dur=rep(2*tp1,10);nT=length(dur)
    vol=rep(f,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # descending part
    dur=rep(tp1,4);nT=length(dur)
    vol=rep(f,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # Flamenco part
    dur=rep(c(6*tp16,10*tp16),10);nT=length(dur)
    vol=rep(f,length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # Cuban part
    dur=rep(c(2*tp16,2*tp16,tp16,2*tp16,tp16,2*tp16,tp16,2*tp16,2*tp16,tp16),8);nT=length(dur)
    vol=rep(c(  p,     ff,    f,   m,     p,   ff,      p,   f,      f,     p),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # Outro
    dur=rep(c(3*tp16,3*tp16,tp16,tp16,2*tp16,tp16,2*tp16,2*tp16,tp16),8);nT=length(dur)
    vol=rep(c(ff,    f,     f,   p,   ff,      p,   f,      f,     p),length.out=nT)*seq(1,0.05,length.out=nT)^0.3
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol)
    # repeat 
    TIM=c(TIM,t0+TIM);VOL=c(VOL,VOL)
    # avoid slight shift and randomize
    TIM=(TIM-0.05)+rnorm(length(TIM),sd=tp16*random_tim)
    VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
    w=play.instrument(drum,notes=rep('ride',length(TIM)),time=TIM,volume=VOL,nmax=20*10^6,
                      fadein=c(2*tp1,rep(0,length(TIM)-1)))
    writeWave(w,'ride.wav')
  }
  return(w)
}

# Clave ---------------
getClave <- function(bpm,f=0.8,m=0.8,p=0.8,random_tim=0.02,random_vol=0.02){
  if(file.exists('clave.wav')){
    w=readWave('clave.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/percussionFWS.RData')
    percu=percussionFWS
    tp4=1/(bpm/60)
    tp16=tp4/4
    t0=8*tp4
    # 3-2 clave
    nT=5*36
    not=rep('claves',nT)
    vol=rep(c(f,m,m,p,f),length.out=nT)*rbeta(nT,1/random_vol,1)
    dur=rep(c(3*tp16,4*tp16,3*tp16,2*tp16,4*tp16),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=tim;VOL=vol;NOT=not
    # 2-3 clave
    nT=5*16
    t0=t0+2*tp16
    not=rep('claves',nT)
    vol=rep(c(m,f,m,f,m),length.out=nT)*rbeta(nT,1/random_vol,1) 
    dur=rep(c(2*tp16,4*tp16,3*tp16,3*tp16,4*tp16),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not)
    # Repeat
    TIM=c(TIM,t0-2*tp16+TIM);VOL=c(VOL,VOL);NOT=c(NOT,NOT)
    TIM=TIM+rnorm(length(TIM),sd=tp16*random_tim)
    w=play.instrument(percu,notes=NOT,time=TIM,volume=VOL,nmax=20*10^6)
    writeWave(w,'clave.wav')
  }
  return(w)
}

# Bell ---------------
getBell <- function(bpm,ff=1,f=0.8,m=0.5,p=0.3,random_tim=0.02,random_vol=0.02){
  if(file.exists('bell.wav')){
    w=readWave('bell.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/percussionFWS.RData')
    percu=percussionFWS
    tp4=1/(bpm/60)
    tp16=tp4/4
    t0=38*4*tp4
    nT=12*8
    not=rep('cowbell3',nT)
    vol=rep(c(f,     f,     p,   m,   f,   ff,    p,   m,   f,   ff,    m,   m),length.out=nT)*rbeta(nT,1/random_vol,1)
    dur=rep(c(2*tp16,2*tp16,tp16,tp16,tp16,2*tp16,tp16,tp16,tp16,2*tp16,tp16,tp16),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=tim;VOL=vol;NOT=not
    # Repeat
    t0=t0+8*4*tp4
    TIM=c(TIM,t0+TIM);VOL=c(VOL,VOL);NOT=c(NOT,NOT)
    TIM=TIM+rnorm(length(TIM),sd=tp16*random_tim)
    w=play.instrument(percu,notes=NOT,time=TIM,volume=VOL,nmax=20*10^6)
    writeWave(w,'bell.wav')
  }
  return(w)
}

# Conga ---------------
getConga <- function(bpm,ff=0.8,f=0.6,m=0.2,p=0.05,random_tim=0.02,random_vol=0.05){
  if(file.exists('conga.wav')){
    w=readWave('conga.wav')
  } else {
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/percussionFWS.RData')
    percu=percussionFWS
    tp4=1/(bpm/60)
    tp16=tp4/4
    tp2=2*tp4
    tp1=4*tp4
    t0=tp1*20
    # descending part
    dur=rep(tp16,16*10);nT=length(dur)
    vol=rep(c(ff,p,m,f,p,p,f,p),length.out=nT)
    not=rep(c(4,3,4,5,3,4,4,5),length.out=nT)
    # not=rep(c(8,10,8,9,10,8,9,9),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=tim;VOL=vol;NOT=not
    # Flamenco part
    dur=rep(tp16,16*8);nT=length(dur)
    vol=rep(c(ff,p,m,f,p,p,f,p),length.out=nT)
    not=rep(c(4,3,4,5,3,4,5,5),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not)
    # Cuban part
    dur=rep(tp16,16*8);nT=length(dur)
    vol=rep(c(m,p,ff,p,p,m,ff,f, p,p,ff,m,p,p,ff,f),length.out=nT)
    not=rep(c(3,4,5,3,5,3,4,4,   3,4,5,4,4,3,4,4),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not)
    # Outro
    dur=rep(tp16,16*8);nT=length(dur)
    vol=rep(c(ff,m,p,f,m,p,ff,m, p,p,ff,m,p,p,ff,m),length.out=nT)*seq(1,0.05,length.out=nT)^0.3
    not=rep(c(4,5,3,5,3,4,4,5,   3,4,5,4,4,3,4,4),length.out=nT)
    foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
    TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not)
    # # repeat 
    TIM=c(TIM,t0+TIM);VOL=c(VOL,VOL);NOT=c(NOT,NOT)
    # randomize
    TIM=TIM+rnorm(length(TIM),sd=tp16*random_tim)
    VOL=VOL*rbeta(length(TIM),1/random_vol,1) 
    w=play.instrument(percu,notes=as.integer(NOT),time=TIM,volume=VOL,nmax=20*10^6,fadein=rep(0,length(TIM)))
    writeWave(w,'conga.wav')
  }
  return(w)
}

# Q and P ---------------
getX <- function(variable,bpm,intro,minRP=100,maxRP=1000){
  if(file.exists(paste0(variable,'left.wav'))){
    L=readWave(paste0(variable,'left.wav'))
    R=readWave(paste0(variable,'right.wav'))
  } else {
    # load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/pianoSteinway.RData')
    # inst=pianoSteinway

    # scale1=rev(c('E2','E3','B3','D4','E4','B4','D5'))
    # scale2=rev(c('Gb2','Gb3','A3','E4','Gb4','A4','E5'))
    # scale3=rev(c('A2','A3','D4','G4','A4','D5','G5'))
    # scale4=rev(c('Ab2','Ab3','D4','Gb4','Ab4','D5','Gb5'))
    # scale5=rev(c('G2','G3','D4','Gb4','G4','D5','Gb5'))
    # scale6=rev(c('Gb2','Gb3','D4','Gb4','Gb4','D5','Gb5'))
    # scale7=rev(c('F2','F3','D4','F4','F4','D5','F5'))
    # scale8=rev(c('F2','F3','B4','F4','F4','B5','F5'))
    # scale9=rev(c('E2','E3','B4','E4','E4','B5','E5'))
    # scale10=rev(c('E2','E3','B4','F4','E4','B5','F5'))
    # scale11=rev(c('C3','C4','E4','G4','C5','D5','E5'))
    # scale12=rev(c('B2','B3','Eb4','Gb4','A4','B4','Eb5'))
    # scale13=rev(c('Db3','G3','A3','E4','G4','A4','E5'))
    # scale14=rev(c('C3','G3','A3','E4','G4','A4','E5'))
    # scale15=rev(c('B2','G3','A3','E4','G4','A4','E5'))
    # scale16=rev(c('Bb2','G3','A3','E4','G4','A4','E5'))

    # scale1=rev(c('E3','E4','B4','D5','E5','B5','D6'))
    # scale2=rev(c('Gb3','Gb4','A4','E5','Gb5','A5','E6'))
    # scale3=rev(c('A3','A4','D5','G5','A5','D6','G6'))
    # scale4=rev(c('Ab3','Ab4','D5','Gb5','Ab5','D6','Gb6'))
    # scale5=rev(c('G3','G4','D5','Gb5','G5','D6','Gb6'))
    # scale6=rev(c('Gb3','Gb4','D5','Gb5','Gb5','D6','Gb6'))
    # scale7=rev(c('F3','F4','D5','F5','F5','D6','F6'))
    # scale8=rev(c('F3','F4','B5','F5','F5','B6','F6'))
    # scale9=rev(c('E3','E4','B5','E5','E5','B6','E6'))
    # scale10=rev(c('E3','E4','B5','F5','E5','B6','F6'))
    # scale11=rev(c('C4','C5','E5','G5','C6','D6','E6'))
    # scale12=rev(c('B3','B4','Eb5','Gb5','A5','B5','Eb6'))
    # scale13=rev(c('Db4','G4','A4','E5','G5','A5','E6'))
    # scale14=rev(c('C4','G4','A4','E5','G5','A5','E6'))
    # scale15=rev(c('B3','G4','A4','E5','G5','A5','E6'))
    # scale16=rev(c('Bb3','G4','A4','E5','G5','A5','E6'))
 
    load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/hangDrum.RData')
    inst=hangDrum
    scale1=rev(c('E3','B3','D4','B4'))
    scale2=rev(c('Gb3','E4','Gb4','A4'))
    scale3=rev(c('A3','C4','G4','A4'))
    scale4=rev(c('Ab3','D4','Gb4','Ab4'))
    scale5=rev(c('G3','D4','Gb4','G4'))
    scale6=rev(c('Gb3','Gb3','D4','Gb4'))
    scale7=rev(c('F3','F3','D4','F4'))
    scale8=rev(c('F3','F3','B3','F4'))
    scale9=rev(c('E3','E3','B3','E4'))
    scale10=rev(c('E3','E3','B3','F4'))
    scale11=rev(c('C3','D4','E4','G4'))
    scale12=rev(c('B3','Eb4','Gb4','A4'))
    scale13=rev(c('Db3','E4','G4','A4'))
    scale14=rev(c('C3','E4','G4','A4'))
    scale15=rev(c('B3','E4','G4','A4'))
    scale16=rev(c('Bb3','E4','G4','A4'))
    
    scales=cbind(scale1,scale2,scale3,scale4,scale5,scale6,scale7,scale8,
                 scale9,scale10,scale11,scale12,scale13,scale14,scale15,scale16)
    tp4=1/(bpm/60)
    tp16=tp4/4
    tp24=tp4/6
    nsc=NROW(scales)
    iscale=c(rep(c(rep(1,24*2),rep(2,24*2)),4),
             rep(3,24*2),rep(4,24*2),rep(5,24*1),rep(6,24*1),rep(7,24*2),rep(8,24*2),
             rep(c(rep(9,24*1),rep(10,24*1)),3),rep(9,24*2),
             rep(c(rep(11,24*0.5),rep(12,24*0.5),rep(1,24*1)),4),
             rep(13,24*2),rep(14,24*1),rep(15,24*1),rep(16,24*2),rep(15,24*2))
    
    load(paste0(variable,'.RData'))
    D$time=D$year-1916+(D$month-0.5)/12
    D$itime=12*(D$year-1916)+D$month
    wleft=wright=0
    for(i in seq(1,NROW(sites),1)){
      message(i)
      ts=D[D$ID==sites$ID[i],c('itime','rp')]
      ts=ts[ts$rp>minRP,]
      nT=NROW(ts) 
      if(nT>0){
        u=ts$rp;u[u>maxRP]=maxRP
        u=(u-minRP)/(maxRP-minRP) #u=(log10(u)-log10(minRP))/(log10(maxRP)-log10(minRP))
        notes=round(0.5+u*nsc);notes[notes==0]=1;notes[notes>nsc]=nsc;notes=as.integer(notes)
        # v=ts$rp;v[v>1000]=1000;v=(v-10)/990
        ix=cbind(notes,iscale[ts$itime])
        wpiano=play.instrument(inst,notes=scales[ix],time=intro*4*tp4+tp24*(ts$itime-1),
                               fadeout=rep(Inf,nT),volume=u,nmax=20*10^60,pan=rep(min(1,sites$lon[i]/180),nT))
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
    }
    L=as.Wave(soundSample(wleft))
    R=as.Wave(soundSample(wright))
    writeWave(L,paste0(variable,'left.wav'))
    writeWave(R,paste0(variable,'right.wav'))
  }
  return(list(left=L,right=R))
}
