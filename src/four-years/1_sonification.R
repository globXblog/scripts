source('funk.R')
bpm=100
intro=8
compt=4

tp4=1/(bpm/60)
tp16=tp4/4
t0=compt*tp16*7

load('four.RData')
years=unique(format(four$date,'%Y'))
types=c('major', 'minor', 'minor', 'major')
leap=c(FALSE, FALSE, FALSE, TRUE)
  
wcount=getCounting(bpm=bpm,compt=compt)
wcymbal=wbass=wpiano=vector('list',length(years))
for(i in 1:length(years)){
  type=types[i]
  year=years[i]
  
  wcymbal[[i]]=getCymbal(bpm=bpm,t0=t0,intro=intro,type=type)
  wbass[[i]]=getBass(bpm=bpm,t0=t0,intro=intro,type=type)
  wpiano[[i]]=getPiano(dat=four %>% filter(format(date,'%Y')==year,
                                      station %in% unique(four$station)[seq(1,131,10)]),
                bpm=bpm,t0=t0,intro=intro,type=type)
  t0=t0+tp16*(365+leap[i])+intro*tp16*7
}

allCymbals=mix(wcymbal)
allBass=mix(wbass)
allLeft=mix(list(wpiano[[1]]$left,wpiano[[2]]$left,wpiano[[3]]$left,wpiano[[4]]$left))
allRight=mix(list(wpiano[[1]]$right,wpiano[[2]]$right,wpiano[[3]]$right,wpiano[[4]]$right))
final=mix(list(wcount,allCymbals,allBass,allLeft,allRight),
          volume=c(0.5,0.75,1,0.8,0.8),
          pan=c(0,0,0,-1,1))

writeWave(final,'temp.wav')
system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',paste0('backing','.mp3')))
file.remove('temp.wav')
