source('funk.R')
bpm=95
intro=8
compt=4

tp4=1/(bpm/60)
tp16=tp4/4
t0=compt*tp16*7

load('four.RData')
years=unique(four$year)
types=c('major','minor', 'minor','major')
leap=c(FALSE, FALSE, TRUE, FALSE)
stations=unique(four$station)
# panning according to station code
four=four %>% left_join(data.frame(station=stations,
                                   pan=rescale(1:length(stations),-1,1)),
                        by='station')

wcount=getCounting(bpm=bpm,compt=compt)
wcymbal=wbass=wpiano=vector('list',length(years))
for(i in 1:length(years)){
  type=types[i]
  currentYear=years[i]
  
  wcymbal[[i]]=getCymbal(bpm=bpm,tstart=t0,intro=intro,type=type)
  wbass[[i]]=getBass(bpm=bpm,tstart=t0,intro=intro,type=type)
  wpiano[[i]]=getPiano(dat=four %>% filter(year==currentYear,
                                      station %in% unique(four$station)[seq(1,135,5)]),
                bpm=bpm,tstart=t0,intro=intro,type=type)
  t0=t0 + intro*7*tp16 + (365+leap[i])*tp16
}

allCymbals=mix(wcymbal)
allBass=mix(wbass)
# final=mix(list(wcount,allCymbals,allBass),
#           volume=c(0.5,0.75,1),
#           pan=c(0,0,0))

allLeft=mix(list(wpiano[[1]]$left,wpiano[[2]]$left,wpiano[[3]]$left,wpiano[[4]]$left))
allRight=mix(list(wpiano[[1]]$right,wpiano[[2]]$right,wpiano[[3]]$right,wpiano[[4]]$right))
final=mix(list(wcount,allCymbals,allBass,allLeft,allRight),
          volume=c(0.4,0.8,1,0.7,0.7),
          pan=c(0,0,0,-1,1))

writeWave(final,'temp.wav')
system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',paste0('backing','.mp3')))
file.remove('temp.wav')
