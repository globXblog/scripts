library(musicXML)
library(dplyr)

load('extents_model5.RData')

all=list();k=0

chords=list(c('F2','F3','G3','C4','G4'),
            c('G2','Eb3','Bb3','D4','E4'),
            c('A2','E3','G3','C4','G4'),
            c('Bb2','F3','Bb3','C4','F4'),
            c('C3','G4','C4','D4','G4'),
            c('D3','A3','C4','G4','A4'))

# SWI ----
vMax=120
what='SWI'
DF=dat %>% filter(var==what)
x=(dat$extent[mask]/100)^1
years=unique(DF$year)
notes=list()
for(i in 1:length(years)){
  dy=DF %>% filter(year==years[i])
  x=dy$extent/100
  maxi=max(x)
  k=max(ceiling(maxi*length(chords)),1)
  vol=x*vMax
  p=pitchMapping(x,rev(chords[[k]]))
  notes=c(notes,getNotes(pitches=p,loudnesses=vol) %>% tieNotes())
}
foo=getMeasures(notes,beats=6,beatType=8)
writeMXL(score(foo),'foo.xml')















gamme=(c('F1','G2','A2','A2','B2','C3','D3','E3','G3','C4','E4','G4'))
var='SWI'
mask=dat$var==var
x=(dat$extent[mask]/100)^1
# x[diff(x)<0]=0
vol=loudnessMapping(x,lMin=40,lMax=110)
for(i in 2:length(x)){if(x[i]==0)x[i]=x[i-1]};plot(x[1000:1120],type='b')
p=pitchMapping(x,gamme)
k=k+1;all[[k]]=getNotes(pitches=p,loudnesses=vol) %>% tieNotes() %>%
  getMeasures(beats=6,beatType=8)

# FWI ----
gamme=rev(c('A3','C4','D4','E4','G4',
            'A4','B4','C5','E5'))
var='FWI'
mask=dat$var==var
x=(dat$extent[mask]/100)^1
vol=loudnessMapping(x,lMin=0,lMax=110)
for(i in 2:length(x)){if(x[i]==0)x[i]=x[i-1]};plot(x[1000:1120],type='l')
p=pitchMapping(x,gamme)
k=k+1;all[[k]]=getNotes(pitches=p,loudnesses=vol) %>% tieNotes() %>%
  getMeasures(beats=6,beatType=8)

# Q -----
var='Q'
mask=dat$var==var
x=(dat$extent[mask]/100)^0.5
plot(x,type='l')
# Q - cymbals
# gamme=rev(c('Db3','A3','G3',
#             'F3','B3','B3','B3','B3'))
gamme=rev(c('F3','B3','B3','B3','B3'))
vol=loudnessMapping(x,lMin=60,lMax=120)
p=pitchMapping(x,gamme)
k=k+1;all[[k]]=getNotes(pitches=p,loudnesses=vol) %>% #tieNotes() %>%
  getMeasures(beats=6,beatType=8)
# Q - Toms
gamme=rev(c('B1','C2','F2','G2','A2','B2'))
vol=loudnessMapping(x,lMin=64,lMax=110)
p=pitchMapping(x,gamme)
k=k+1;all[[k]]=getNotes(pitches=p,loudnesses=vol) %>% tieNotes() %>%
  getMeasures(beats=6,beatType=8)


writeMXL(score(all),'score.xml')

