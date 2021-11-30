oneTrack <- function(sam,v,pattern=1:31,lo=0,hi=1,time=((0:(length(v)-1))/365.25)*yearDuration){
  y=v;y[is.na(y)]=0
  if(sd(y)==0){
    s=sequence(sam,time=time,volume=lo*(days %in% pattern),nmax=10^8)
  } else {
    s=sequence(sam,time=time,volume=rescale(y,low=lo,high=hi)*(days %in% pattern),nmax=10^8)
  }
  return(s)
}

dampen <-function(sam,d=2){
  n=d*sam$rate
  fade=exp(-3*(1:sam$n)/(d*sam$rate))
  out=sam
  out$wave=(sam$wave*fade)[1:n]
  out$n=n
  out$duration=n/sam$rate
  return(out)
}

writeMP3 <- function(w,fname){
  writeWave(w,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
  file.remove('temp.wav' )
}

writeDrums <- function(DF,fname='drums.mp3',lo=0.1,wind1=7,wind2=15,tDiff=-3,dataOnly=F){
  # samples 
  setwd('samples')
  sam_ride=soundSample(readMP3('ride.mp3')@left)
  sam_hhc=soundSample(readMP3('hhc.mp3')@left)
  sam_hho=soundSample(readMP3('hho.mp3')@left)
  sam_bell=soundSample(readMP3('bell.mp3')@left)
  sam_crash=soundSample(readMP3('crash.mp3')@left)
  setwd('..')
  
  # kick and crash
  vkick=DF$Windspeed-wind1;vkick[is.na(vkick)]=0;vkick[vkick<0]=0
  #snare
  y=DF$Temperature;y[is.na(y)]=0
  mask=c(F,diff(y) < tDiff);events <- rle(mask)
  cs=cumsum(events$lengths)
  vsnare=0*y
  vsnare[cs[which(events$values)]]=events$lengths[events$values]

  # output data frame
  out=data.frame(date=DF$date,Radiation=DF$Radiation,Radiation_v=rescale(DF$Radiation,lo=lo),
                 RelativeHumidity=DF$RelativeHumidity,RelativeHumidity_v=rescale(DF$RelativeHumidity,lo=lo),
                 Windspeed=DF$Windspeed,Windspeed_v=rescale(vkick),
                 Temperature=DF$Temperature,Temperature_v=rescale(vsnare))
  if(dataOnly){return(out)}
  # cymbals
  bell=oneTrack(sam_bell,DF$Radiation,pattern=c(6,8,14,22,24,30),lo=lo)
  ride=oneTrack(sam_ride,DF$Radiation,pattern=c(1,4,7,11,13,17,20,23,27,29),lo=lo)
  hhc=oneTrack(sam_hhc,DF$RelativeHumidity,pattern=c(2,3,5,9,10,12,15,16,18,19,21,25,26,28,31),lo=lo)
  hho=oneTrack(sam_hho,DF$RelativeHumidity,pattern=c(1),lo=lo)
  # kick and crash
  ki=oneTrack(kick,vkick)
  crash=oneTrack(sam_crash,as.numeric(vkick>wind2-wind1))
  #snare
  sn=oneTrack(snare,vsnare)
  # mix
  drums=mix(list(ki,ki,ki,sn,sn,hho,hhc,ride,bell,crash),
            volume=c(1,1,1,1,1,0.5,0.4,0.6,0.6,0.4),
            pan=c(-1,1,0,-0.7,0.7,0.8,0.8,-0.8,-0.8,0))
  writeWave(drums,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
  file.remove('temp.wav' )
  return(out)
}

writeGuitar <- function(DF,fname='guitar.mp3',lo=0.02,tThresh=-30,dataOnly=F){
  # samples 
  setwd('samples')
  sam_gE0=soundSample(readMP3('gE0.mp3')@left)
  sam_gF0=soundSample(readMP3('gF0.mp3')@left)
  sam_gG0=soundSample(readMP3('gG0.mp3')@left)
  sam_gA0=soundSample(readMP3('gA0.mp3')@left)
  sam_gB0=soundSample(readMP3('gB0.mp3')@left)
  sam_gC1=soundSample(readMP3('gC1.mp3')@left)
  sam_gD1=soundSample(readMP3('gD1.mp3')@left)
  sam_gE1=soundSample(readMP3('gE1.mp3')@left)
  sam_gG1=soundSample(readMP3('gG1.mp3')@left)
  setwd('..')
  # compute volume
  vguitar=-1*DF$Temperature;vguitar[is.na(vguitar)]=0
  vguitar[vguitar<(-1*tThresh)]=(-1*tThresh)
  
  out=data.frame(date=DF$date,Temperature=DF$Temperature,Temperature_v=rescale(vguitar,lo=lo))
  if(dataOnly){return(out)}
  
  # create sequences
  E0=oneTrack(sam_gE0,vguitar,pattern=c(1,4,6,9,11,21,24),lo=lo)
  F0=oneTrack(sam_gF0,vguitar,pattern=c(2),lo=lo)
  G0=oneTrack(sam_gG0,vguitar,pattern=c(3,7,30),lo=lo)
  A0=oneTrack(sam_gA0,vguitar,pattern=c(8,29,31),lo=lo)
  B0=oneTrack(sam_gB0,vguitar,pattern=c(12,18,28),lo=lo)
  D1=oneTrack(sam_gD1,vguitar,pattern=c(5,13,17,19,22,25,27),lo=lo)
  E1=oneTrack(sam_gE1,vguitar,pattern=c(10,14,16,20,23,26),lo=lo)
  G1=oneTrack(sam_gG1,vguitar,pattern=c(15),lo=lo)
  guitar=mix(list(E0,F0,G0,A0,B0,D1,E1,G1))
  writeWave(guitar,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
  file.remove('temp.wav' )
  return(out)
}

writeOrgan <- function(DF,fname='organ.mp3',lo=0.02,tThresh=-30,dataOnly=F){
  # samples 
  setwd('samples')
  sam_E0=soundSample(readMP3('E0.mp3')@left)
  sam_F0=soundSample(readMP3('F0.mp3')@left)
  sam_G0=soundSample(readMP3('G0.mp3')@left)
  sam_A0=soundSample(readMP3('A0.mp3')@left)
  sam_B0=soundSample(readMP3('B0.mp3')@left)
  sam_D1=soundSample(readMP3('D1.mp3')@left)
  sam_E1=soundSample(readMP3('E1.mp3')@left)
  sam_G1=soundSample(readMP3('G1.mp3')@left)
  setwd('..')
  
  # compute volume
  vguitar=-1*DF$Temperature;vguitar[is.na(vguitar)]=0
  vguitar[vguitar<(-1*tThresh)]=(-1*tThresh)
  out=data.frame(date=DF$date,Temperature=DF$Temperature,Temperature_v=rescale(vguitar,lo=lo))
  if(dataOnly){return(out)}

  # create sequences
  E0=oneTrack(sam_E0,vguitar,pattern=c(1,4,6,9,11,21,24),lo=lo)
  F0=oneTrack(sam_F0,vguitar,pattern=c(2),lo=lo)
  G0=oneTrack(sam_G0,vguitar,pattern=c(3,7,30),lo=lo)
  A0=oneTrack(sam_A0,vguitar,pattern=c(8,29,31),lo=lo)
  B0=oneTrack(sam_B0,vguitar,pattern=c(12,18,28),lo=lo)
  D1=oneTrack(sam_D1,vguitar,pattern=c(5,13,17,19,22,25,27),lo=lo)
  E1=oneTrack(sam_E1,vguitar,pattern=c(10,14,16,20,23,26),lo=lo)
  G1=oneTrack(sam_G1,vguitar,pattern=c(15),lo=lo)
  organ=mix(list(E0,F0,G0,A0,B0,D1,E1,G1))
  writeWave(organ,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
  file.remove('temp.wav' )
  return(out)
}

writeHarmonics <- function(DF,fname='harmonics.mp3',qExp=0.25,dataOnly=F){
  # samples 
  setwd('samples')
  sam_hE1=soundSample(readMP3('hE1.mp3')@left)
  sam_hG1=soundSample(readMP3('hG1.mp3')@left)
  sam_hA1=soundSample(readMP3('hA1.mp3')@left)
  sam_hB1=soundSample(readMP3('hB1.mp3')@left)
  sam_hD2=soundSample(readMP3('hD2.mp3')@left)
  sam_hE2=soundSample(readMP3('hE2.mp3')@left)
  sam_hF2=soundSample(readMP3('hF2.mp3')@left)
  sam_hG2=soundSample(readMP3('hG2.mp3')@left)
  sam_hB2=soundSample(readMP3('hB2.mp3')@left)
  setwd('..')
  
  # compute volume
  hlist=list(sam_hE1,sam_hG1,sam_hA1,sam_hB1,sam_hD2,
             sam_hE2,sam_hF2,sam_hG2,sam_hB2)
  slist=hlist
  q=DF$Streamflow;q[is.na(q)]=0
  q=ceiling(length(hlist)*(rescale(q^qExp)))
  qForAnim=rescale(q)
  q[ c(FALSE,q[2:n]==q[1:(n-1)]) ]=0
  out=data.frame(date=DF$date,Streamflow=DF$Streamflow,Streamflow_v=qForAnim)
  if(dataOnly){return(out)}
  
  # create sequences
  for(i in 1:length(hlist)){
    mask=as.numeric(q==i)
    if(sum(mask)>0){
      slist[[i]]=oneTrack(hlist[[i]],mask)
    } else {
      slist[[i]]=oneTrack(hlist[[i]],q,hi=0)
    }
  }
  harmonics=mix(slist,volume=(1:length(slist))/length(slist))
  writeWave(harmonics,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
  file.remove('temp.wav' )
  return(out)
}

writePiano <- function(DF,fname='piano.mp3',dataOnly=F){
  if(dataOnly){return(NULL)}
  # samples 
  setwd('samples')
  sam_E=soundSample(readMP3('cE.mp3')@left)
  sam_F=soundSample(readMP3('cF.mp3')@left)
  sam_G=soundSample(readMP3('cG.mp3')@left)
  sam_A=soundSample(readMP3('cA.mp3')@left)
  sam_C=soundSample(readMP3('cC.mp3')@left)
  sam_D=soundSample(readMP3('cD.mp3')@left)
  sam_B=soundSample(readMP3('cB.mp3')@left)
  sam_Bb=soundSample(readMP3('cBb.mp3')@left)
  setwd('..')
  # create sequences
  cE=oneTrack(sam_E,as.numeric(months %in% c(1)),pattern=c(1))
  cF=oneTrack(sam_F,as.numeric(months %in% c(2,12)),pattern=c(1))
  cG=oneTrack(sam_G,as.numeric(months %in% c(3,11)),pattern=c(1))
  cA=oneTrack(sam_A,as.numeric(months %in% c(4,10)),pattern=c(1))
  cC=oneTrack(sam_C,as.numeric(months %in% c(5,7)),pattern=c(1))
  cD=oneTrack(sam_D,as.numeric(months %in% c(6)),pattern=c(1))
  cB=oneTrack(sam_B,as.numeric(months %in% c(8)),pattern=c(1))
  cBb=oneTrack(sam_Bb,as.numeric(months %in% c(9)),pattern=c(1))
  piano=mix(list(cE,cF,cG,cA,cC,cD,cB,cBb))
  writeWave(piano,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
  file.remove('temp.wav' )
}

writeBass <- function(DF,fname='bass.mp3',wind1=7,dataOnly=F){
  # samples 
  setwd('samples')
  sam_bE=soundSample(readMP3('bE1.mp3')@left)
  sam_bF=soundSample(readMP3('bF1.mp3')@left)
  sam_bG=soundSample(readMP3('bG1.mp3')@left)
  sam_bA=soundSample(readMP3('bA1.mp3')@left)
  sam_bBb=soundSample(readMP3('bBb1.mp3')@left)
  sam_bB=soundSample(readMP3('bB1.mp3')@left)
  sam_bC=soundSample(readMP3('bC2.mp3')@left)
  sam_bD=soundSample(readMP3('bD2.mp3')@left)
  setwd('..')
  # compute volume
  vkick=DF$Windspeed-wind1;vkick[is.na(vkick)]=0;vkick[vkick<0]=0
  out=data.frame(date=DF$date,Windspeed=DF$Windspeed,Windspeed_v=rescale(vkick))
  if(dataOnly){return(out)}

    # create sequences
  E0=oneTrack(dampen(sam_bE),vkick*as.numeric(months %in% c(1)))
  F0=oneTrack(dampen(sam_bF),vkick*as.numeric(months %in% c(2,12)))
  G0=oneTrack(dampen(sam_bG),vkick*as.numeric(months %in% c(3,11)))
  A0=oneTrack(dampen(sam_bA),vkick*as.numeric(months %in% c(4,10)))
  C1=oneTrack(dampen(sam_bC),vkick*as.numeric(months %in% c(5,7)))
  D1=oneTrack(dampen(sam_bD),vkick*as.numeric(months %in% c(6)))
  B0=oneTrack(dampen(sam_bB),vkick*as.numeric(months %in% c(8)))
  Bb0=oneTrack(dampen(sam_bBb),vkick*as.numeric(months %in% c(9)))
  bass=mix(list(E0,F0,G0,A0,C1,D1,B0,Bb0))
  writeWave(bass,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
  file.remove('temp.wav' )
  return(out)
}
