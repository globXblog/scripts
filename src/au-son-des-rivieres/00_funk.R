library(sequenceR);library(dplyr);library(BFunk)

nmax=50*10^6 # Maximum number of points in a waveform

getDaily <- function(site,period,datadir){
  flist=list.files(datadir)
  k=which(grepl(site,flist,fixed=T))
  X=read.table(file.path(datadir,flist[k]),header=FALSE,skip=1,sep=',')
  daily=data.frame(year=X[,5],month=X[,6],day=X[,7],value=X[,9])
  if(!is.null(period)){
    days=seq.Date(as.Date(paste0(min(period),'-01-01')),as.Date(paste0(max(period),'-12-31')), by='day')
    daily0=data.frame(year=as.integer(format(days,'%Y')),month=as.integer(format(days,'%m')),
                     day=as.integer(format(days,'%d')))
    daily=daily %>% filter(year %in% period)
    daily=full_join(daily0,daily,by=c("year", "month", "day"))
  }
  daily$value[daily$value < 0]=NA
  return(daily)
}

getData <- function(dd,minCV=0.7,maxCV=2,period=1987:2016){
  days=seq.Date(as.Date(paste0(min(period),'-01-01')),as.Date(paste0(max(period),'-12-31')), by='day')
  daily0=data.frame(year=as.integer(format(days,'%Y')),month=as.integer(format(days,'%m')),
                    day=as.integer(format(days,'%d')))
  daily=dd %>% filter(year %in% period)
  daily=full_join(daily0,daily,by=c("year", "month", "day"))
  #monthly
  monthlyMean=dailyToMonthly(daily,stat=mean)
  # regime
  rmean=getMonthlyStat(monthlyMean)
  rcv=getMonthlyStat(monthlyMean,stat=sd)/rmean
  regime=data.frame(mean=rmean,paf=rmean/sum(rmean),cv=rcv)
  # monthly data
  med=monthlyMean
  minima=dailyToMonthly(daily,stat=min)
  maxima=dailyToMonthly(daily,stat=max) 
  sdev=dailyToMonthly(daily,stat=sd)
  Mcv=sdev$value/med$value;Mcv[sdev$value==0]=0
  paf=med$value/sum(rmean)
  monthly=data.frame(year=med$year,month=med$month,mean=med$value,paf=paf,
                     min=minima$value,max=maxima$value,sd=sdev$value,cv=Mcv)
  # annual
  annual=getAnnualVariables(daily,variables='mean')
  for(year in period){ # add missing years as NA values
    if(!(year %in% annual$year)){annual=rbind(annual,data.frame(year=year,variable='mean',value=NA))}
  }
  annual=annual[order(annual$year),]
  CV=sd(daily$value,na.rm=T)/mean(daily$value,na.rm=T)
  u=min(max((CV-minCV)/(maxCV-minCV),0),1)
  bpm=as.integer(75+(120-75)*u) # beats per minute
  if(bpm<85) {style='arabic'} else {
    if(bpm<91) {style='blues'} else {
      if(bpm<97) {style='penta'} else {
        if(bpm<108) {style='flamenco'} else {
          style='funk'
        }
      }
    }
  }
  spb=60/bpm # seconds per beat
  spt=spb/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
  return(list(daily=daily,monthly=monthly,annual=annual,regime=regime,
              CV=CV,style=style,bpm=bpm,spt=spt))
}

uglyPlot <- function(dat){
  par(mfrow=c(3,2))
  plot(dat$regime$paf,type='b',ylim=c(0,1/3))
  plot(dat$regime$cv,type='b')
  plot(dat$annual$year,dat$annual$value,type='b')
  plot(dat$monthly$max,type='l',col='blue',ylim=c(0,max(dat$monthly$max,na.rm=T)))
  lines(dat$monthly$min,type='l',col='red')
  plot(dat$daily$value,type='l')
}

loadInstruments <- function(instdir){
  load(file.path(instdir,'bassStandup.RData'))
  bass=bassStandup
  load(file.path(instdir,'guitarPhilharmonia.RData')) 
  guitar=guitarPhilharmonia
  load(file.path(instdir,'pianoSteinway.RData'))
  piano=pianoSteinway
  load(file.path(instdir,'drumkitStahl.RData'))
  drum=drumkitStahl
  load(file.path(instdir,'hangDrum.RData'))
  return(list(bass=bass,drum=drum,piano=piano,guitar=guitar,hangDrum=hangDrum))
}

getBand <- function(style,inst,strum=0){
  # Create piano chords
  cNotes=switch(style,
                blues=list(c('E1','E2','B2','D3','Ab3','B3'),
                           c('G1','G2','B2','F3','G3','D4'),
                           c('A1','A2','E3','G3','Db4','E4'),
                           c('C2','C3','E3','G3','C4','G4'),
                           c('D2','D3','Gb3','C4','D4','A4')),
                penta=list(c('E1','E2','B2','E3','G3','A3'),
                           c('G1','G2','C3','E3','G3','D4'),
                           c('A1','A2','E3','A3','D3','E4'),
                           c('C2','C3','G3','B3','C4','G4'),
                           c('D2','D3','A3','C4','D4','A4')),
                funk=list(c('A4','C5','Gb5'),
                          c('G4','B4','G5'),
                          c('G4','C5','G5'),
                          c('G4','C5','G5'),
                          c('A4','C5','A5')),
                flamenco=list(c('D1','D2','A2','D3','A3','D4'),
                              c('G1','G2','D3','F3','Bb3','D4'),
                              c('A1','A2','E3','Bb3','Db4','E4'),
                              c('Bb1','Bb2','D3','Bb3','D4','F4'),
                              c('C2','C3','E3','G3','D4','G4')),
                arabic=list(c('A1','A2','B3','F3','Ab3'),
                            c('A1','A2','C3','E3','A3'),
                            c('A1','A2','E3','G3','C4'),
                            c('A1','A2','F3','G3','D4'),
                            c('A1','A2','Ab3','B3','F4'))
  )
  cInst=inst$piano 
  chords=cNotes
  for(i in 1:length(cNotes)){
    w=play.instrument(cInst,cNotes[[i]],
                      time=seq(0,by=strum,length.out=length(cNotes[[i]])),
                      fadein=rep(0,length(cNotes[[i]])),fadeout=rep(Inf,length(cNotes[[i]])),nmax=nmax)
    chords[[i]]=soundSample(0.5*(w@left+w@right))
  }
  # Create bass notes
  bNotes=switch(style,
                blues=c('E1','G1','A1','C2','D2'),
                penta=c('E1','G1','A1','C2','D2'),
                funk=c('Gb1','G1','A1','C2','D2'),
                flamenco=c('D2','G2','A2','Bb2','C3'),
                arabic=c('A1','A1','A2','A3','A3'))
  bass=inst$bass[bNotes]
  # Create main melody
  mNotes=switch(style,
                blues=c('A2','C3','D3','Eb3','E3','G3','A3','C4','D4','E4','G4','A4'),
                penta=c('A3','C4','D4','E4','G4','A4','B4'),
                funk=c('G4','A4','C5','D5','E5','G5'),
                flamenco=c('A2','Bb2','Db3','D3','E3','F3','A3','Bb3','Db4','D4','E4','F4','A4','Bb4'),
                arabic=c('A2','B2','C3','E3','F3','Ab3','A3','B3','C4','E4','F4','Ab4','A4')
  )
  if(style=='penta'){
    mInst=inst$hangDrum
  } else {
    mInst=inst$piano
  }
  melody=mInst[mNotes]
  # Create notes for 'variation' melody (controled by monthly anomalies)
  vNotes=mNotes;vInst=inst$piano
  variation=vInst[vNotes]
  return(list(drum=inst$drum,bass=bass,chords=chords,melody=melody,variation=variation))
}

getCymbals <- function(spt,style,drum,nrep=1,hiSep=1/6,loSep=1/48,startFade=nrep+1){
  f=1;m=0.6;p=0.3 # forte - mezzo - piano
  n=12 
  tim=seq(from=0,by=spt,length.out=n*nrep)
  env=rep_len(1,n*nrep)
  if(startFade<=nrep){
    jx=which((1:length(env)) >= 1+n*(startFade-1))
    env[jx]=seq(1,0,length.out=length(jx))
  }
  
  # hihat
  pattern=switch(style,
                 penta=c(0,0,0,0,0,0,0,0,0,0,0,0),
                 blues=c(f,p,p,f,p,p,f,p,p,f,p,p),
                 flamenco=c(0,0,0,0,0,0,0,0,0,0,0,0),
                 funk=c(0,p,0,0,m,0,p,0,0,m,0,p),
                 arabic=c(0,0,f,p,f,m,0,0,f,0,m,p),
                 arabicSlow=c(0,0,p,0,0,p,0,0,0,p,0,p),
                 rep(0,12))
  hh=sequence(drum$hihat,time=tim,volume=env*rep_len(pattern,n*nrep),nmax=nmax)
  # foot hihat
  pattern=switch(style,
                 penta=c(m,0,0,m,0,0,m,0,0,m,0,0),
                 blues=c(m,0,0,m,0,0,m,0,0,m,0,0),
                 flamenco=c(0,0,p,0,0,p,0,p,0,p,0,p),
                 funk=c(f,0,m,0,0,0,0,m,0,0,0,0),
                 arabic=c(p,0,0,0,p,0,0,0,p,0,0,0),
                 arabicSlow=c(m,0,0,0,0,0,0,0,0,0,0,0),
                 rep(0,12))
  hf=sequence(drum$hihat_f,time=tim,volume=env*rep_len(pattern,n*nrep),nmax=nmax)
  # ride
  pattern=switch(style,
                 penta=c(f,0,m,0,m,p,0,m,0,m,0,p),
                 blues=c(0,0,0,p,0,0,0,0,0,p,0,0),
                 flamenco=c(f,0,0,0,m,0,0,0,m,0,0,0),
                 funk=c(f,0,m,p,0,f,0,m,f,0,p,0),
                 arabic=c(f,p,0,m,0,0,f,p,0,m,0,0),
                 arabicSlow=c(f,0,0,p,m,0,p,0,m,0,0,0),
                 rep(0,12))
  ri=sequence(drum$ridebell,time=tim,volume=env*rep_len(pattern,n*nrep),nmax=nmax)
  
  return(list(hh=hh,hf=hf,ri=ri))
}

sonifyMonthly <- function(x,spt,inst,nrep=1,vol=1+0*x,pan=NULL,fadeout=Inf,
                         knots=seq(from=1/3,to=1/48,length.out=length(inst)),
                         startFade=nrep+1){
  # time
  tim=seq(from=0,by=spt,length.out=length(x)*nrep)
  # Panoramic
  if(is.null(pan)){
    pan0=x-mean(x);pan0[is.na(pan0)]=0;panv=pan0
    foo=pan0[pan0>=0]
    panv[pan0>=0]=rescale(foo,min(foo)/max(foo),1)
    foo=pan0[pan0<0]
    panv[pan0<0]=rescale(-foo,-max(foo)/min(foo),-1)
    panv=rep_len(panv,length(x)*nrep)
  } else {
    panv=rep(pan,length(x)*nrep)
  }
  # notes
  ix=rep(0,length(x))
  for(i in 1:length(x)){
    if(is.na(x[i])){
      ix[i]=1
    } else {
      ix[i]=which.min(abs(x[i]-knots))
    }
  }
  ix=rep_len(ix,length(x)*nrep)
  volum=vol;volum[is.na(vol)]=0
  volume=rep_len(volum,length(x)*nrep)
  if(startFade<=nrep){
    jx=which((1:length(volume)) >= 1+length(x)*(startFade-1))
    volume[jx]=volume[jx]*seq(1,0,length.out=length(jx))
  }
  panv[is.na(panv)]=0
  w=play.instrument(inst,notes=as.integer(ix),time=tim,volume=volume,
                    pan=panv,fadein=rep(0,length(ix)),fadeout=rep(fadeout,length(ix)),nmax=nmax)
  return(w)
}

sonifyAnnual <- function(x,spt,inst,vol=1+0*x,
                         knots=quantile(x,na.rm=TRUE,
                                        probs=seq(1-1/(length(inst)+1),1/(length(inst)+1),length.out=length(inst)))){
  # time
  tim=seq(from=0,by=spt,length.out=length(x))
  # notes
  ix=rep(0,length(x))
  for(i in 1:length(x)){
    if(is.na(x[i])){
      ix[i]=1
    } else{
      ix[i]=which.min(abs(x[i]-knots))
    }
  }
  volum=vol;volum[is.na(x)]=0
  w=play.instrument(inst,notes=as.integer(ix),time=tim,volume=volum,
                    fadein=rep(0,length(ix)),fadeout=rep(0.1,length(ix)),nmax=nmax)
  return(w)
}

sonifyMonthly2 <- function(mini,maxi,spt,drum,vmin=0.2,nTop=5,
                          loT=quantile(mini,1/6,na.rm=T),hiT=quantile(maxi,5/6,na.rm=T)){
  n=length(mini)
  tim=seq(from=0,by=spt,length.out=n)
  # kick
  xs=maxi-hiT;xs[xs<0]=0;xs[is.na(xs)]=0
  if(all(xs==0)){pattern=xs} else {pattern=rescale(xs,low=vmin);pattern[xs==0]=0}
  ki=sequence(drum$bass,time=tim,volume=pattern,nmax=nmax)
  mask=rank(xs)>(length(xs)-nTop)
  cr=sequence(drum$crash,time=tim[mask],volume=pattern[mask],nmax=nmax)
  # snare
  xs=loT-mini;xs[xs<0]=0;xs[is.na(xs)]=0
  if(all(xs==0)){pattern=xs} else {pattern=rescale(xs)}
  sn=sequence(drum$snare,time=tim,volume=pattern,nmax=nmax)
  return(list(ki=ki,sn=sn,cr=cr))
}

sonify <- function(dat,band,intro=2,outro=4,Tflood=5,
                   minVol_melody=0.05,minVol_variation=0.05,minVol_chords=0.5){
  period=min(dat$daily$year):max(dat$daily$year)
  # Regime: cymbals and melody
  nrep=NROW(dat$annual)+intro+outro
  cy=getCymbals(spt=dat$spt,style=dat$style,drum=band$drum,nrep=nrep,startFade=nrep-outro+1)
  melody=sonifyMonthly(x=dat$regime$paf,spt=dat$spt,inst=band$melody,nrep=nrep,vol=rescale(dat$regime$cv,1,minVol_melody),startFade=nrep-outro+1)
  # Monthly values: variations
  paf=dat$monthly$paf
  x=c(rep(mean(paf),intro*12),paf,rep(mean(paf),outro*12))
  vol=c(rep(0,intro*12),rescale(abs(paf-dat$regime$paf),minVol_variation,1),rep(0,outro*12))
  variation=sonifyMonthly(x=x,spt=dat$spt,inst=band$variation,vol=vol)
  # Annual values: chords and bass
  x=c(rep(0,intro),dat$annual$value,rep(0,outro))
  vol=c(rep(0,intro),rescale(sqrt(dat$annual$value),minVol_chords,1),rep(0,outro))
  chords=sonifyAnnual(x=x,spt=dat$spt*12,vol=vol,inst=band$chords)
  bass=sonifyAnnual(x=x,spt=dat$spt*12,vol=vol,inst=band$bass)
  # Monthly mean and max: drums
  mini=c(rep(Inf,intro*12),dat$monthly$min,rep(Inf,outro*12))
  maxi=c(rep(-Inf,intro*12),dat$monthly$max,rep(-Inf,outro*12))
  dr=sonifyMonthly2(mini=mini,maxi=maxi,spt=dat$spt,drum=band$drum,nTop=length(period)/Tflood)
  # Mix
  final=mix(list(melody,variation,chords,bass,  cy$hh,cy$hf,cy$ri,  dr$ki,dr$sn,dr$cr),
            volume=c(0.15,0.35,0.4,1,  0.07,0.2,0.25,  1,0.6,0.15),
            pan=c(0,0,0,0,  -0.5,0,0.5,  0,0,-0.5))
  return(final)
}
