library(musicXML)
library(dplyr)

load('extents_model5.RData')
gamme=rev(c('E2','F2','G2',
            'A2','C3','D3','F3',
            'A3','C4','D4','F4','G4',
            'A4','B4','C5','E5','F5','A5'))
out=list()
for(var in c('SWI','FWI','Q')){
  mask=dat$var==var
  x=(dat$extent[mask]/100)^0.5
  vol=loudnessMapping(x,lMin=54)
  p=pitchMapping(x,gamme)
  out[[var]]=getNotes(pitches=p,loudnesses=vol) %>% tieNotes() %>%
    getMeasures(beats=6,beatType=8)
}


writeMXL(score(out),'score.xml')

