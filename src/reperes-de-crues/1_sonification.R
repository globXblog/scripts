library(musicXML)  # https://github.com/benRenard/musicXML
DF=read.table('HCIs.csv',header=TRUE)
# Note duration is constant and corresponds to a quarter note
dlist=durationMapping(rep(0,NROW(DF)),expMin=3,expMax=3)
# chords defining the scale used for each 10/8 bar
scales=list(c('E3','B3','F#4','G4','D5','E5','G5','D6'),
            c('E3','C4','F#4','G4','D5','E5','G5','C6'),
            c('G3','D4','F#4','B4','D5','E5','G5','B5'),
            c('D3','A3','D4','F#4','A4','B4','D5','E5','G5'),
            c('C3','G3','C4','E4','G4','B4','E5','G5','B5'),
            c('A3','C4','E4','G4','C5','E5','G5','D6'),
            c('E3','B3','E4','A4','B4','E5','A5','B5'),
            c('G3','C4','E4','G4','B4','E5','G5','D6'),
            c('A3','C4','G4','A4','C5','E5','A5','B5'),
            c('F3','C4','G4','A4','C5','E5','G5','A5'),
            c('D3','A3','D4','F4','G4','A4','C5','F5'),
            c('Bb3','D4','F4','A4','C5','D5','G5','A5'),
            c('A3','E4','G4','C5','E5','G5','A5','B4'),
            c('G3','B3','D4','G4','A4','D5','G5','D6'),
            c('G3','B3','D4','G4','A4','D5','G5','D6'),
            # repeat
            c('E3','B3','F#4','G4','D5','E5','G5','D6'),
            c('E3','C4','F#4','G4','D5','E5','G5','C6'),
            c('G3','D4','F#4','B4','D5','E5','G5','B5'),
            c('D3','A3','D4','F#4','A4','B4','D5','E5','G5'),
            c('C3','G3','C4','E4','G4','B4','E5','G5','B5'),
            c('A3','C4','E4','G4','C5','E5','G5','D6'),
            c('E3','B3','E4','A4','B4','E5','A5','B5'),
            c('G3','C4','E4','G4','B4','E5','G5','D6'),
            c('A3','C4','G4','A4','C5','E5','A5','B5'),
            c('F3','C4','G4','A4','C5','E5','G5','A5'),
            c('D3','A3','D4','F4','G4','A4','C5','F5'),
            c('Bb3','D4','F4','A4','C5','D5','G5','A5'),
            c('A3','E4','G4','C5','E5','G5','A5','B4'),
            c('G3','B3','D4','G4','A4','D5','G5','D6'),
            c('G3','B3','D4','G4','A4','D5','G5','D6'),
            # finish
            c('E3','B3','E4','A4','B4','E5','A5','B5') )
for(i in 1:3){
  var=DF[[paste0('HCI',i)]] # HCI values
  uvar=DF[[paste0('uHCI',i)]] # HCI uncertainties
  # Note volume is controled by both the HCI absolute value and its uncertainty
  # Also time is reversed - song is going back in time
  foo=rev(abs(var)/uvar)
  llist=loudnessMapping(foo,lMin=21,lMax=107)
  # Note pitch is controled by the HCI value
  foo=-1*rev(var)
  plist=pitchMapping(foo,pitches='D5') #initialize pitch list
  for(j in 1:length(scales)){
    # indices for the jth bar
    indx=((j-1)*10+1):min(NROW(DF),j*10)
    # pitch mapping of the whole series but according to the jth chord
    full=pitchMapping(foo,pitches=scales[[j]])
    # keep only relevant indices
    plist[indx]=full[indx]
  }
  # assemble score
  notes=getNotes(pitches=plist,loudnesses=llist,durations=dlist)
  notes=tieNotes(notes)
  m=getMeasures(notes=notes,beats=10,beatType=8)
  s=score(m)
  writeMXL(s,file=paste0('HCI',i,'.xml'))
}
