source('funk.R')
bpm=95
intro=8
compt=4
minVol=0.5

tp4=1/(bpm/60)
tp16=tp4/4
t0=compt*tp16*7

load('fourYears.RData')
load('stations.RData')
years=unique(dat$year)
types=c('major','minor', 'minor','major')
leap=c(FALSE, FALSE, FALSE, TRUE)
stations=stations %>% filter(CODE10 %in% unique(dat$station))

# panning according to station longitude
dat=dat %>% left_join(data.frame(station=stations$CODE10,
                                 pan=rescale(stations$lon,-1,1)),
                      by='station')
# Minor and major chords index
dat$minor=dat$month
dat$major=1 #E
dat$major[dat$date >= as.Date(paste0(dat$year,'-03-25'))]=2 #A
dat$major[dat$date >= as.Date(paste0(dat$year,'-05-20'))]=3 #B
dat$major[dat$date >= as.Date(paste0(dat$year,'-06-17'))]=4 #Db
dat$major[dat$date >= as.Date(paste0(dat$year,'-07-15'))]=3 #B
dat$major[dat$date >= as.Date(paste0(dat$year,'-08-12'))]=4 #Db
dat$major[dat$date >= as.Date(paste0(dat$year,'-09-09'))]=5 #Ab
dat$major[dat$date >= as.Date(paste0(dat$year,'-10-21'))]=6 #Gb
dat$major[dat$date >= as.Date(paste0(dat$year,'-11-18'))]=7 #Ab
dat$major[dat$date >= as.Date(paste0(dat$year,'-12-02'))]=8 #B

# master volume according to mean anomaly
master=dat %>% group_by(date,day,month,year) %>% 
  summarise(vol=mean(anomaly_smooth,na.rm=TRUE))
master$vol=rescale(master$vol,1,minVol)
plot(master$date,master$vol)

wcount=getCounting(bpm=bpm,compt=compt)
wcymbal=wbass=wkick=wpiano=wguitar=vector('list',length(years))
for(i in 1:length(years)){
  type=types[i]
  currentYear=years[i]
  isLeap=leap[i]
  wguitar[[i]]=getGuitar(bpm=bpm,tstart=t0,intro=intro,type=type,isLeap=isLeap)
  wkick[[i]]=getDrumKick(bpm=bpm,tstart=t0,intro=intro,type=type,isLeap=isLeap)
  wcymbal[[i]]=getCymbal(bpm=bpm,tstart=t0,intro=intro,type=type,isLeap=isLeap)
  wbass[[i]]=getBass(bpm=bpm,tstart=t0,intro=intro,type=type,isLeap=isLeap)
  wpiano[[i]]=getPiano(dat=dat %>% filter(year==currentYear,
                                          station %in% unique(dat$station)),
                bpm=bpm,tstart=t0,intro=intro,type=type)
  t0=t0 + intro*7*tp16 + (365+isLeap)*tp16
}
# final chord
wkick[[i+1]]=play.instrument(drum,'bass',time=t0,fadein=0,nmax=50*10^6)
wcymbal[[i+1]]=play.instrument(drum,c('ride','splash'),time=c(t0,t0),fadein=c(0,0),nmax=50*10^6)
wbass[[i+1]]=play.instrument(bass,'E2',time=t0,nmax=50*10^6)
foo=play.instrument(inst,c('E2','B2','Gb3','B3','E4','B4','E5'),
                    time=t0+seq(0,0.5,length.out=7),
                    volume=rep(master$vol[NROW(master)],7),
                    fadeout=rep(Inf,7),nmax=50*10^6)
wpiano[[i+1]]=list(left=foo,right=foo)

allGuitar=mix(wguitar)
allKick=mix(wkick)
allCymbals=mix(wcymbal)
allBass=mix(wbass)
rm(wkick);rm(wcymbal);rm(wbass);gc()
# Apply master volume
tvect0=cumsum(c(0,compt*tp16*7,
                intro*7*tp16,rep(tp16,(365+leap[1])),
                intro*7*tp16,rep(tp16,(365+leap[2])),
                intro*7*tp16,rep(tp16,(365+leap[3])),
                intro*7*tp16,rep(tp16,(365+leap[4]))))
v0=c(1,master$vol[master$year==years[1]][1],
     master$vol[master$year==years[1]][1],master$vol[master$year==years[1]],
     master$vol[master$year==years[2]][1],master$vol[master$year==years[2]],
     master$vol[master$year==years[3]][1],master$vol[master$year==years[3]],
     master$vol[master$year==years[4]][1],master$vol[master$year==years[4]],
     master$vol[master$year==years[4]][length(master$vol[master$year==years[4]])])
applyMaster <- function(w){
  w_l=as.soundSample(w,-1)
  env=envelope(t=c(tvect0,w_l$duration)/w_l$duration,v=v0)
  w_l=applyEnvelope(w_l,env) %>% as.Wave()
  w_r=as.soundSample(w,1)
  env=envelope(t=c(tvect0,w_r$duration)/w_r$duration,v=v0)
  w_r=applyEnvelope(w_r,env) %>% as.Wave()
  out=mix(list(w_l,w_r),pan=c(-1,1))
  return(out)
}
allGuitar=allGuitar %>% applyMaster()
allKick=allKick %>% applyMaster()
allCymbals=allCymbals %>% applyMaster()
allBass=allBass %>% applyMaster()

allLeft=mix(list(wpiano[[1]]$left,wpiano[[2]]$left,wpiano[[3]]$left,wpiano[[4]]$left,wpiano[[5]]$left))
allRight=mix(list(wpiano[[1]]$right,wpiano[[2]]$right,wpiano[[3]]$right,wpiano[[4]]$right,wpiano[[5]]$right))
final=mix(list(wcount,allKick,allCymbals,allBass,allBass,allLeft,allRight,allGuitar,allGuitar),
          volume=c(0.5,0.7,0.98,0.8,0.8,0.5,0.5,0.3,0.3),
          pan=c(0,0,0,-0.3,0.3,-1,1,-0.7,0.7))

writeWave(final,'temp.wav')
system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',paste0('backing','.mp3')))
file.remove('temp.wav')


