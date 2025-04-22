library(musicXML)
library(dplyr)

load('extents_model5.RData')

all=list();k=0

chords=list(
  c('E3','B3','D4','E4','G4','B4'),
  c('Bb3','D4','Bb4','D5','E5','F5'),
  c('B3','Gb4','G4','B4','D5','E5'),
  c('C4','G5','C5','D5','G5','B5'),
  c('E4','B4','Eb5','E5','Gb5','B5'))

chords=list(
  c('E3','B3','E4','G4','D5','E5'),
  c('F3','C4','F4','G4','C5','G5'),
  c('A3','E4','G4','C5','G5','A5'),
  c('B3','D4','A4','D5','F5','A5'),
  c('C4','G4','C5','D5','G5','B5'),
  # c('E4','B4','Eb5','E5','Gb5','B5')
  c('E3','E4','B4','F5','A5','B5'),
  c('E3','E4','C5','E5','G5','B5'),
  c('E3','E4','B4','E5','G5','B5'))

# First pass to get chords from SWI----
what='SWI'
DF=dat %>% filter(var==what) %>% mutate(pitch=extent^0.5)
plot(DF$tim,DF$pitch,type='l')
years=unique(DF$year)
iChords=rep(NA,length(years))
for(i in 1:length(years)){
  dy=DF %>% filter(year==years[i])
  x=dy$pitch/max(DF$pitch)
  maxi=1-max(x)
  iChords[i]=max(ceiling(maxi*length(chords)),1)
}
plot(iChords,type='b')

# Q ----
vMin=80;vMax=110
what='Q'
DF=dat %>% filter(var==what) %>% mutate(pitch=extent^0.75)
plot(DF$tim,DF$pitch,type='l')
notes=list()
for(i in 1:length(years)){
  dy=DF %>% filter(year==years[i])
  x=dy$pitch/max(DF$pitch)
  vol=vMin+x*(vMax-vMin)
  p=pitchMapping(c(x,0.2),rev(chords[[iChords[i]]]))[1:12]
  notes=c(notes,getNotes(pitches=p,loudnesses=vol) %>% tieNotes())
}
k=k+1;all[[k]]=getMeasures(notes %>% tieNotes(),beats=6,beatType=8)
writeMXL(score(all),'Q.xml')

# FWI ----
vMin=80;vMax=110
what='FWI'
DF=dat %>% filter(var==what) %>% mutate(pitch=extent^0.75)
plot(DF$tim,DF$pitch,type='l')
notes=list()
for(i in 1:length(years)){
  dy=DF %>% filter(year==years[i])
  x=dy$pitch/max(DF$pitch)
  vol=vMin+x*(vMax-vMin)
  p=pitchMapping(c(x,0.2),rev(chords[[iChords[i]]]))[1:12]
  notes=c(notes,getNotes(pitches=p,loudnesses=vol) %>% tieNotes())
}
k=k+1;all[[k]]=getMeasures(notes %>% tieNotes(),beats=6,beatType=8)
writeMXL(score(all),'FWI.xml')

# SWI -----
what='SWI'
DF=dat %>% filter(var==what) %>% mutate(pitch=extent^0.5)
plot(DF$tim,DF$pitch,type='l')

# Toms
gamme=rev(c('B1','C2','F2','G2','A2','B2'))
vol=loudnessMapping(DF$pitch,lMin=64,lMax=110)
p=pitchMapping(DF$pitch,gamme)
k=k+1;all[[k]]=getNotes(pitches=p,loudnesses=vol) %>% tieNotes() %>%
  getMeasures(beats=6,beatType=8)
# 
# # cymbals
# gamme=rev(c('F3','B3','B3','B3','B3'))
# vol=loudnessMapping(x,lMin=60,lMax=120)
# p=pitchMapping(x,gamme)
# k=k+1;all[[k]]=getNotes(pitches=p,loudnesses=vol) %>% #tieNotes() %>%
#   getMeasures(beats=6,beatType=8)

writeMXL(score(all),'score.xml')

