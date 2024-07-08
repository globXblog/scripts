library(dplyr);library(tidyr);
library(ggplot2);library(patchwork);library(gganimate)
library(av)
library(sequenceR) # https://github.com/benRenard/sequenceR
library(BFunk) # https://github.com/benRenard/BFunk

# DATA ----

getQJ <- function(QJfile){
  X=read.table(QJfile,header=TRUE,sep=',')
  QJ=data.frame(year=as.numeric(substr(X[,1],1,4)),
                month=as.numeric(substr(X[,1],6,7)),
                day=as.numeric(substr(X[,1],9,10)),
                value=X[,2])
  QJ$value[QJ$value<0]=NA
  return(QJ)
}

getSeasonalData <- function(dataset,normalizeAnom=TRUE,
                    seasons=rbind(c(3,4,5),c(6,7,8),c(9,10,11),c(12,1,2))){
    seasonnal=c()
    for(i in 1:NROW(seasons)){
      means=getAnnualVariables(dataset,hydroyear=seasons[i,],variables='mean')
      if(normalizeAnom){
        anom=qnorm((rank(means$value)-0.5)/NROW(means))
      } else {
        anom=scale(means$value)
      }
      seasonnal=rbind(seasonnal,cbind(means,anom=anom,
                                      iseason=i,season=paste0(monthName(seasons[i,],1),collapse='')))
    }
    seasonnal=seasonnal %>% complete(year=min(year):max(year),variable,nesting(iseason,season)) %>%
      mutate(time=year+(iseason-0.5)/NROW(seasons)) %>% arrange(time) 
  return(seasonnal)
}

# SOUND ----

loadInstruments <- function(instdir){
  load(file.path(instdir,'bassStandup.RData'))
  bass=bassStandup
  load(file.path(instdir,'guitarHarmonics.RData')) 
  guitar=guitarHarmonics
  load(file.path(instdir,'pianoSteinway.RData'))
  piano=pianoSteinway
  load(file.path(instdir,'drumkitStahl.RData'))
  drum=drumkitStahl
  load(file.path(instdir,'hangDrum.RData'))
  return(list(bass=bass,drum=drum,piano=piano,guitar=guitar,hangDrum=hangDrum))
}

createSound <- function(seasonnal,ID,fname,normalizeAnom=TRUE,style='major',bpm=90,
                        seasons=levels(factor(seasonnal$season)),pow=2,t0=0){
  spb=60/bpm # seconds per beat
  spt=spb/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
  if(style=='oriental'){
    n1=c('E3','G3','A3','B3','C4','Eb4','E4','Gb4','G4','A4','B4','C5')
    n2=c('E3','G3','A3','B3','C4','Eb4','E4','Gb4','G4','A4','B4','C5')
    n3=c('E1','G1','A1','B1','C2','Eb2','E2','Gb2','G2','A2','B2','C3')
  } else if(style=='major'){
    n1=c('E3','E3','B3','Eb4','E4','Ab4','B4','B4')
    n2=c('E3','E3','B3','Eb4','E4','Ab4','B4','B4')
    n3=c('E1','Ab1','A1','B1','Db2','E2','Ab2','A2','B2','Db3')
  } else if(style=='minor'){
    n1=c('E3','B3','E4','B4','D4','E4','G4','B4')
    n2=c('E3','B3','E4','B4','D4','E4','G4','B4')
    n3=c('E1','G1','A1','B1','D2','E2','G2','A2','B2','D3')
  }
  
  # Data
  naMask=!is.na(seasonnal$value)
  seasonMask=seasonnal$season %in% seasons
  svalue=seasonnal$value;svalue[svalue==0]=0.1*min(svalue[naMask & svalue>0])
  # Time
  tim0=t0+((1:NROW(seasonnal))-1)*spt
  # Instrument 1 highlights positive anomalies
  pitch=log10(svalue[naMask & seasonMask])
  ix=as.integer(rescale(pitch)*(length(n1)-1))+1
  volum=seasonnal$anom[naMask & seasonMask]
  vol=rescale(volum * (volum > 0))^pow
  i1=play.instrument(inst$piano,notes=n1[ix],time=tim0[naMask & seasonMask],
                     volume=vol,fadeout=max(ix)-ix+1)
  # Instrument 2 highlights negative anomalies
  pitch=log10(svalue[naMask & seasonMask])
  ix=as.integer(rescale(pitch)*(length(n2)-1))+1
  volum=seasonnal$anom[naMask & seasonMask]
  vol=rescale(-1*volum * (volum < 0))^pow
  i2=play.instrument(inst$hangDrum,notes=n2[ix],time=tim0[naMask & seasonMask],
                     volume=vol,fadeout=0*ix+Inf)
  # Instrument 3 (Bass) highlights negative anomalies
  thresh=quantile(seasonnal$anom[naMask & seasonMask],1/8)
  mask=seasonnal$anom[naMask & seasonMask]<thresh
  timBass=tim0[naMask & seasonMask][mask]
  vol=0*timBass+1 # rescale(thresh-seasonnal$anom[mask]) # 
  pitch=log10(svalue[naMask & seasonMask][mask])
  ix=as.integer(rescale(pitch)*(length(n3)-1))+1
  bass=play.instrument(inst$bass,notes=n3[ix],time=timBass,volume=vol,
                       fadein=rep(0.1,length(ix)),fadeout=rep(0.5,length(ix)))
  # Drumkit:  bass drum with the bass
  thresh=quantile(seasonnal$anom[naMask],1/8)
  mask=seasonnal$anom[naMask & seasonMask]<thresh
  timBassDrum=tim0[naMask & seasonMask][mask]
  vol=rescale(thresh-seasonnal$anom[naMask & seasonMask][mask])
  drum_bass=play.instrument(inst$drum,notes=rep('bass',length(timBassDrum)),time=timBassDrum,volume=vol)
  # Drumkit:  ride with values
  timRide=tim0[naMask]
  vol=rescale(seasonnal$value[naMask])
  drum_ride=play.instrument(inst$drum,notes=rep('ride',length(timRide)),time=timRide,
                            volume=vol,fadeout=0*timRide+Inf)
  # Drumkit:  regular hihat
  timHH=tim0[rep_len(c(T,F,F,F),length(tim0))]
  vol=0*timHH+1
  drum_hh=play.instrument(inst$drum,notes=rep('hihat_f',length(timHH)),time=timHH,volume=vol)
  # Drumkit:  snare with high values
  thresh=quantile(seasonnal$anom[naMask & seasonMask],1-1/4)
  mask=seasonnal$anom[naMask & seasonMask]>thresh
  timSnare=tim0[naMask & seasonMask][mask]
  vol=rescale(seasonnal$anom[naMask & seasonMask][mask])
  drum_snare=play.instrument(inst$drum,notes=rep('snare',length(timSnare)),time=timSnare,volume=vol)
  # mix
  final=mix(waves=c(list(i1,i2,bass,drum_bass,drum_ride,drum_hh,drum_snare)),
            vol=c(0.7,1,1,1,0.7,0.3,1),pan=c(-0.7,0.7,0,0,0,0,0))
  writeWave(final,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',file.path(paste0(fname,'.mp3'))))
  file.remove('temp.wav')
}

# IMAGE/VIDEO ----

plotOneStep <- function(seasonnal,tstep,tmax,what,coord,nom,memory=6,seasons=levels(seasonnal$season),
                        colorLow='#8c510a',colorMid='#f5f5f5',colorHigh='#01665e',colorBkg='#011a27',
                        colorLine='#c4dfe6',colorPoint=colorLine,vspace=0.5,ff='Arial',revertCol=FALSE){
  if(revertCol) {cLow=colorHigh;cHigh=colorLow} else {cLow=colorLow;cHigh=colorHigh}
  frMap = map_data("world") %>% filter(region=='France')
  n=NROW(seasonnal)
  # trend
  annual=seasonnal %>% group_by(year) %>% summarise(amean=mean(value))
  foo=glm(data=annual,amean~year)
  changePct=100*(foo$fitted.values[NROW(foo$fitted.values)]-foo$fitted.values[1])/foo$fitted.values[1]
  isSignif=coef(summary(foo))[2,4]<0.1
  changeTxt=ifelse(isSignif,
                   paste0(ifelse(changePct>0,'En hausse de ','En baisse de '),round(abs(changePct)),'%'),
                   'Pas de changement significatif')
  changeTxt=paste0(changeTxt,' sur la période ',min(annual$year),'-',max(annual$year))
  changeCol=ifelse(isSignif,ifelse(changePct>0,cHigh,cLow),colorPoint)
  changeAlpha=ifelse(tstep<n,0,( (tstep-n)/(tmax-n) )^0.75)
  changesize=ifelse(isSignif,16,14)
  yearAlpha=ifelse(tstep<n,0.15,0.15*( (tmax-tstep)/(tmax-n) )^2)
  # texts and titles
  stxt=switch(what,Q='Débit dans la rivière',P='Précipitations',E='Évapotranspiration potentielle','')
  if(tstep>NROW(seasonnal)){
    ytxt=seasonnal$year[NROW(seasonnal)]
  } else if (tstep<=0){
    ytxt=''
  } else {
    ytxt=seasonnal$year[tstep]
  }
  # stxt=switch(as.character(seasonnal$season[tstep]),
  #             MAM='Printemps',DJF='Hiver',SON='Automne',JJA='Été','')
  # get base data frame
  DF=seasonnal
  DF$sanom=rescale(DF$anom,-1,1)
  DF$svalue=vspace+rescale(DF$value,1,2.5)
  arrowData=data.frame(x=min(DF$time)-0.25,xend=min(DF$time)-0.25,y=c(-0.02,0.02),yend=range(DF$sanom,na.rm=TRUE))
  # Compute x/y lims
  xlim=range(seasonnal$time);xlim[1]=xlim[1]-0.05*diff(xlim)
  ylim=c(-1,max(DF$svalue,na.rm=TRUE)+1.5*vspace)
  # Rescale France Map
  rescaler <-function(lon,lat,xf=0.18,yf=xf*1.7,xl=xlim,yl=ylim){
    resc <- function(z,src,factor,zlim,onTheLeft=FALSE){
      if(onTheLeft){
        target=c(zlim[1],zlim[1]+factor*diff(range(zlim)))
      } else {
        target=c(zlim[2]-factor*diff(range(zlim)),zlim[2])
      }
      out=target[1]+ ((z-src[1])/(src[2]-src[1])) * (target[2]-target[1])
      return(out)
    }
    LON=resc(lon,range(frMap$long),xf,xl)
    LAT=resc(lat,range(frMap$lat),yf,yl)
    return(data.frame(LON=LON,LAT=LAT))
  }
  frMap2=frMap
  frMap2=cbind(frMap2,rescaler(frMap2$long,frMap2$lat))
  # ratio=4
  # xl=0.5*sum(xlim)+(0.5*diff(range(xlim))*(1/ratio))*c(-1,1)
  # yl=0.5*sum(ylim)+(0.5*diff(range(ylim))*(1/ratio))*c(-1,1)
  # frMap2=cbind(frMap2,rescaler(frMap2$long,frMap2$lat,xf=0.18*ratio*2,xl=xl,yl=yl))
  # QR code
  # qr=NULL png::readPNG('QR.png')
  # qr2=qr2[qr2$value,]
  # Base plot
  g0=ggplot()+
    geom_polygon(data=frMap2,aes(LON,LAT,group=group),fill=colorPoint,alpha=0.15)+
    geom_point(data=rescaler(coord$Lon,coord$Lat),aes(LON,LAT),color=colorPoint,size=4,shape='circle plus')+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=ylim)+
    theme_void()+
    theme(plot.background=element_rect(fill=colorBkg),
          legend.position='none')+
    annotate('text',label=nom,x=quantile(DF$time,0.5),y=max(ylim)-0.1*diff(range(ylim)),
             color=colorLine,size=10,family=ff,vjust=1,alpha=0.8)
    # annotation_raster(qr,ymin=ylim[1]+0.85*diff(ylim),ymax=ylim[1]+1.05*diff(ylim),
    #                   xmin=xlim[1]-0.05*diff(xlim)*(9/16),xmax=xlim[1]+0.15*diff(xlim)*(9/16)) 
  
  g0=g0+
    annotate('text',label="Moyennes\nsaisonnières",x=xlim[1],y=mean(range(DF$svalue,na.rm=TRUE)),
             color=colorLine,size=9,angle=90,family=ff)+
    annotate('text',label=stxt,x=quantile(DF$time,0.5),y=max(ylim),color=colorLine,
             size=20,family=ff,vjust=1,alpha=0.8)+
    annotate('text',label=ytxt,x=quantile(DF$time,0.5),y=1+vspace/2,color=colorPoint,
             alpha=yearAlpha,size=54,family='Times')+
    annotate('text',label=changeTxt,x=quantile(DF$time,0.5),y=1+vspace/2,color=changeCol,
             alpha=changeAlpha,size=changesize,family=ff,fontface = 'italic')+
    annotate('text',label="Anomalies saisonnières",x=xlim[1],y=0,
             color=colorLine,size=9,angle=90,family=ff,vjust=-1)+
    geom_segment(data=arrowData[1,],aes(x=x,y=y,xend=xend,yend=yend),
                 color=colorspace::lighten(cLow,0.4),arrow=arrow(type='closed'),size=1)+
    geom_segment(data=arrowData[2,],aes(x=x,y=y,xend=xend,yend=yend),
                 color=colorspace::lighten(cHigh,0.4),arrow=arrow(type='closed'),size=1)+
    annotate('text',label="Moins que\nd'habitude",x=xlim[1],y=-0.5,family=ff,
             color=colorspace::lighten(cLow,0.4),size=7,angle=90,vjust=1)+
    annotate('text',label="Plus que\nd'habitude",x=xlim[1],y=0.5,family=ff,
             color=colorspace::lighten(cHigh,0.4),size=7,angle=90,vjust=1)
  if(tstep<=0) return(g0)
  
  # Restrict DF to requested period (1 to tstep)
  d1toT=DF[1:min(tstep,NROW(seasonnal)),]
  # dlast=d1toT[max(1,NROW(d1toT)-memory+1):NROW(d1toT),]
  dlast=d1toT[max(1,tstep-memory+1):tstep,]
  
  # Top part: values
  g=g0+geom_line(data=d1toT,aes(time,svalue),color=colorLine,alpha=0.3,size=0.3)+
    geom_line(data=dlast,aes(time,svalue,alpha=time),color=colorLine)+
    geom_point(data=dlast,aes(time,svalue,alpha=time,size=time),color=colorPoint)
  # Bottom part: anomalies
  g=g+geom_col(data=d1toT[d1toT$season %in% seasons,],aes(time,sanom,fill=sanom))+
    scale_fill_gradient2(low=cLow,mid=colorMid,high=cHigh,limits=c(-1,1))+
    geom_point(data=dlast,aes(time,sanom,alpha=time,size=time,color=anom))+
    scale_color_gradient2(low=cLow,mid=colorMid,high=cHigh,limits=c(-1,1))
  
  return(g)  
}

plotMap <- function(coord,nom,colorBkg='#011a27',colorMap='#c4dfe6',colorText='#c4dfe6',ff='Arial'){
  frMap = map_data("world") %>% filter(region=='France')
  rlon=range(frMap$long);rlat=range(frMap$lat)
  frMap2=frMap
  frMap2$LON=rescale(frMap$long,0,1)
  frMap2$LAT=rescale(frMap$lat,0,1)
  coo=coord
  coo$LON=(coord$Lon-rlon[1])/(diff(rlon))
  coo$LAT=(coord$Lat-rlat[1])/(diff(rlat))
  g0=ggplot()+
    geom_polygon(data=frMap2,aes(LON,LAT,group=group),fill=colorMap,alpha=0.15)+
    geom_point(data=coo,aes(LON,LAT),color=colorMap,size=4,shape='circle plus')+
    theme_void()+theme(plot.background=element_rect(fill=colorBkg))+
    annotate('text',label=nom,x=0.5,y=1.1,color=colorText,size=14,family=ff,alpha=0.8)+
    scale_x_continuous(limits=c(-0.5,1.5))+scale_y_continuous(limits=c(0,2/1.7))
  return(g0)  
}

concatVids <- function(ID){
  lines=paste0('file ',ID,c('_0','_1_P','_2_E','_3_Q'),'.mp4')
  writeLines(lines,con=file.path('www','temp.txt'))
  cmd=paste0('cd www;ffmpeg -y -f concat -i temp.txt -c copy ',ID,'.mp4')
  system(cmd)
}

concatStations <- function(IDs,vname){
  lines=paste0('file ',file.path('www',paste0(IDs,'.mp4')))
  writeLines(lines,con='temp.txt')
  cmd=paste0('ffmpeg -y -f concat -i temp.txt -c copy ',vname,'.mp4')
  system(cmd)
}

getQRcode <-function(url,colorBkg='#011a27',colorTile='#c4dfe6'){
  M=qrcode::qr_code(url)
  n=NCOL(M)
  DF=M%>%as.data.frame()%>%pivot_longer(cols=all_of(1:n))
  DF=cbind(DF,row=rep(1:n,n),col=rep(n:1,each=n))
  g=ggplot(DF)+geom_raster(aes(row,col,fill=value,alpha=value))+
    scale_fill_manual(values=c(colorBkg,colorTile),guide=NULL)+
    scale_alpha_manual(values=c(1,0.5),guide=NULL)+coord_equal()+
    theme_void()+
    theme(plot.background=element_rect(fill=colorBkg,color=colorBkg),legend.position='none')
  png(filename='QR.png',960,960)
  print(g)
  dev.off()
}
