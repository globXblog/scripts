library(tidyr);library(dplyr)
library(sequenceR);library(musicXML)
load('extent_monthly.RData')
DF=DF %>% filter(year>1978,year+month-2>1978)

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
  vs=loudnessMapping(x,10,110)
  vs[mask]=0
  plot(vs,type='l')
  final[[what]]=getNotes(pitches=ps,loudnesses=vs)
  if(TRUE){final[[what]]=final[[what]] %>% tieNotes()}
  final[[what]]=final[[what]] %>% getMeasures(6,8)
}
s=score(final)
writeMXL(s,'TwoPoles.xml')
