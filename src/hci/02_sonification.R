library(musicXML)
what='HCI' # HCI or SCI
X=read.table('ClimateIndices.csv',header = T, sep = ';')
if(what=='HCI'){DF=X[,1:3]} else {DF=X[,4:6]}
# Note duration is constant and corresponds to a quarter note
dlist <- durationMapping(rep(0,NROW(DF)),expMin=2,expMax=2)
# Note volume is controlled by the first component
llist <- loudnessMapping(DF[,1],lMax=124)

# Guitar chords are controlled by the 2nd component
k=2
foo=DF[,k]
scale <- c("E2","E2", "B2","C#3","D#3","E3","E3")
plist <- pitchMapping(foo,pitches=scale)
notes <- getNotes(pitches=plist,loudnesses=llist,durations=dlist)
m <- getMeasures(notes=notes)
s <- score(m)
writeMXL(s,file=paste0(what,k,'.xml'))

# High notes are controlled by the 3rd component
k=3
foo=DF[,k]
scale <- c("E3","E3","B3","D#4","E4","B4","D#5","E5","E5")
plist <- pitchMapping(foo,pitches=scale)
notes <- getNotes(pitches=plist,loudnesses=llist,durations=dlist)
m <- getMeasures(notes=notes)
s <- score(m)
writeMXL(s,file=paste0(what,k,'.xml'))



