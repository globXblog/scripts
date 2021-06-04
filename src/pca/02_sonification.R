library(musicXML);library(dplyr);library(ggplot2)

# Read PCA components
DF0=read.table('Components.txt',header=T)

# First Component
DF=DF0[,1]
scale <- c("E2","G2","B2","C#3","D3","E3","G3","G#3","D4","E4")
plist <- pitchMapping(DF,pitches=scale)
llist <- loudnessMapping(abs(DF),lMin = 37, lMax = 124)
notes <- getNotes(pitches=plist,loudnesses=llist)
notes <- tieNotes(notes)
m <- getMeasures(notes=notes,beats=3,beatType=4)
s <- score(m)
writeMXL(s,file=paste0('PC1.xml'))

# Second Component
DF=-1*DF0[,2]
scale <- c("E2","G2","B2","C#3","D3","E3","G3","G#3","D4","E4")
plist <- pitchMapping(DF,pitches=scale)
llist <- loudnessMapping(abs(DF),lMin = 37, lMax = 124)
notes <- getNotes(pitches=plist,loudnesses=llist)
notes <- tieNotes(notes)
m <- getMeasures(notes=notes,beats=3,beatType=4)
s <- score(m)
writeMXL(s,file=paste0('PC2.xml'))
