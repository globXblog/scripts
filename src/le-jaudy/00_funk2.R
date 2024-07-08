library(sequenceR);library(dplyr);library(BFunk)
library(ggplot2);library(patchwork);library(av);library(lubridate)

# ----------
# Animation settings
# period <- 1981:2000
wetColour="#27807f"   #blue
dryColour="#b98c5a"   #brown
regimeColour= "#102128"
annualColour <- "#27807f"
dailyColour <- "#102128"
offColour="#ede3cc"
labelColour <- "grey18"
resfactor <- 2
regimeLabels <- data.frame("month"=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                           "x"=seq(1,12),
                           stringsAsFactors=FALSE)
regimeLabels$angle <- (360-(((((regimeLabels$x)*2)-1)*360)/24))+1.5

nmax=50*10^6 # Maximum number of points in a waveform

getQJ <- function(QJfile){
  X=read.table(QJfile,header=TRUE,sep=',')
  QJ=data.frame(year=as.numeric(substr(X[,1],1,4)),
                month=as.numeric(substr(X[,1],6,7)),
                day=as.numeric(substr(X[,1],9,10)),
                value=X[,2])
  QJ$value[QJ$value<0]=NA
  return(QJ)
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
  return(list(daily=daily,monthly=monthly,annual=annual,regime=regime))
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

getPafPolygons <- function(pafData) {
  # Duplicate data
  foo <- pafData
  n <- nrow(foo)
  # Get polygon coordinates to draw monthly paf
  paf_polygons <- data.frame(matrix(ncol=4,nrow=5*n))  # Each polygon has 5 points
  colnames(paf_polygons) <- c("x","y","id","fill")
  for (i in 1:n){
    irows <- c((((i-1)*5)+1):(i*5))
    if(i==1){
      a2 <- (foo$paf_regime[i+1]-foo$paf_regime[i])/(foo$idx_monthly[i+1]-foo$idx_monthly[i])
      b2 <- foo$paf_regime[i+1]-(a2*foo$idx_monthly[i+1])
      ymid2 <- (a2*(foo$idx_monthly[i]+0.45))+b2
      paf_polygons$x[irows] <- c(foo$idx_monthly[i]-0.45,foo$idx_monthly[i],foo$idx_monthly[i]+0.45,foo$idx_monthly[i]+0.45,foo$idx_monthly[i]-0.45)
      paf_polygons$y[irows] <- c(foo$paf_regime[i],foo$paf_regime[i],ymid2,foo$paf_monthly[i],foo$paf_monthly[i])
      paf_polygons$id[irows] <- i
      paf_polygons$fill[irows] <- ifelse(foo$paf_monthly[i]>foo$paf_regime[i],"wet","dry")
    }
    else if(i==n){
      a1 <- (foo$paf_regime[i]-foo$paf_regime[i-1])/(foo$idx_monthly[i]-foo$idx_monthly[i-1])
      b1 <- foo$paf_regime[i]-(a1*foo$idx_monthly[i])
      ymid1 <- (a1*(foo$idx_monthly[i]-0.45))+b1
      paf_polygons$x[irows] <- c(foo$idx_monthly[i]-0.45,foo$idx_monthly[i],foo$idx_monthly[i]+0.45,foo$idx_monthly[i]+0.45,foo$idx_monthly[i]-0.45)
      paf_polygons$y[irows] <- c(ymid1,foo$paf_regime[i],foo$paf_regime[i],foo$paf_monthly[i],foo$paf_monthly[i])
      paf_polygons$id[irows] <- i
      paf_polygons$fill[irows] <- ifelse(foo$paf_monthly[i]>foo$paf_regime[i],"wet","dry")
    }
    else {
      # Slope before i-th point
      a1 <- (foo$paf_regime[i]-foo$paf_regime[i-1])/(foo$idx_monthly[i]-foo$idx_monthly[i-1])
      b1 <- foo$paf_regime[i]-(a1*foo$idx_monthly[i])
      ymid1 <- (a1*(foo$idx_monthly[i]-0.45))+b1
      # Slope after i-th point
      a2 <- (foo$paf_regime[i+1]-foo$paf_regime[i])/(foo$idx_monthly[i+1]-foo$idx_monthly[i])
      b2 <- foo$paf_regime[i+1]-(a2*foo$idx_monthly[i+1])
      ymid2 <- (a2*(foo$idx_monthly[i]+0.45))+b2
      # Create dataframe
      paf_polygons$x[irows] <- c(foo$idx_monthly[i]-0.45,foo$idx_monthly[i],foo$idx_monthly[i]+0.45,foo$idx_monthly[i]+0.45,foo$idx_monthly[i]-0.45)
      paf_polygons$y[irows] <- c(ymid1,foo$paf_regime[i],ymid2,foo$paf_monthly[i],foo$paf_monthly[i])
      paf_polygons$id[irows] <- i
      paf_polygons$fill[irows] <- ifelse(foo$paf_monthly[i]>foo$paf_regime[i],"wet","dry")
    }
  }
  return(paf_polygons)
}

getAnnualPolygons <- function(annualData) {
  # Duplicate data
  foo <- annualData
  # Get polygon coordinares to draw annual values
  annual_polygons <- data.frame(matrix(ncol=4,nrow=4*n))  # Each polygon has 4 points
  colnames(annual_polygons) <- c("x","y","id","fill")
  for (i in 1:n){
    irows <- c((((i-1)*4)+1):(i*4))
    # x axis starts at 0.5 (not 0.55 like getPafPolygons, so there is no gap between 2 bars in a row)
    annual_polygons$x[irows] <- c(foo$idx_monthly[i]-0.5,foo$idx_monthly[i]+0.5,foo$idx_monthly[i]+0.5,foo$idx_monthly[i]-0.5)
    annual_polygons$y[irows] <- c(0,0,foo$annual[i],foo$annual[i])
    annual_polygons$id[irows] <- i
  }
  return(annual_polygons)
}

getPafPlot0 <- function(annualData, pafPolygons,regimeColour="black") {
  # Duplicate data
  foo_a <- annualData
  foo_p <- pafPolygons
  n <- NROW(foo_a)
  # Plot regime paf (black line)
  pafPlot_t0 <- ggplot()+theme_void()+
    geom_line(data=foo_a,aes(x=idx_monthly,y=paf_regime),color=regimeColour)
  # Add y lims for a given site (get max ylims with ggplot_build by drawing the whole series)
  pafPlot_ylims <- pafPlot_t0+geom_polygon(data=foo_p, aes(x=x,y=y,group=id))
  paf_ylims <- ggplot_build(pafPlot_ylims)$layout$panel_scales_y[[1]]$range$range
  pafPlot_t0 <- pafPlot_t0+
    scale_x_continuous(limits=c(0.5,(n+1)), expand=c(0,0))+
    scale_y_continuous(limits=paf_ylims)
  return(pafPlot_t0)
}

getAnnualDailyPlot0 <- function(dailyData, annualPolygons, annualColour="red", dailyFullColour="grey96") {
  # Duplicate data
  foo_d <- dailyData
  foo_ap <- annualPolygons
  n <- max(foo_ap$id)
  # Get y lims for a given site (get max ylims with ggplot_build by drawing the whole series)
  # annualPlot_ylims <- ggplot2::ggplot()+ggplot2::geom_polygon(data=foo_ap, aes(x=x,y=y,group=id),fill=annualColour)
  # annual_ylims <- ggplot2::ggplot_build(annualPlot_ylims)$layout$panel_scales_y[[1]]$range$range
  # dailyPlot_ylims <- ggplot2::ggplot()+ggplot2::geom_line(data=foo_d, aes(x=new_x,y=value_daily))
  # daily_ylims <- ggplot2::ggplot_build(dailyPlot_ylims)$layout$panel_scales_y[[1]]$range$range
  ylimmax <- max(foo_d$value_daily,na.rm=TRUE)
  # Plot annual values, month by month (grey bars)
  annualDailyPlot_t0 <- ggplot2::ggplot()+
    ggplot2::geom_line(data=foo_d, aes(x=new_x,y=value_daily), colour=dailyFullColour)+
    ggplot2::theme_void()+
    ggplot2::scale_x_continuous(limits=c(0.5,(n+1)), expand=c(0,0))+
    ggplot2::scale_y_continuous(limits=c(0,ylimmax))+
    ggplot2::geom_text(aes(x=0.5,y=ylimmax*0.97, 
                           label=min(foo_d$year, na.rm=TRUE)), hjust=0, colour=dailyFullColour, size=14)+
    ggplot2::geom_text(aes(x=foo_d$new_x[foo_d$year==max(foo_d$year, na.rm=TRUE)][1],y=ylimmax*0.97, 
                           label=max(foo_d$year, na.rm=TRUE)), hjust=0, colour=dailyFullColour, size=14)
  return(annualDailyPlot_t0)
}
