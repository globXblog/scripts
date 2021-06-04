library(musicXML) # https://github.com/benRenard/musicXML
WaggaWagga=read.table('WaggaWagga.csv',header=TRUE,sep=';')
# Temperature is mapped to note loudness
llist <- loudnessMapping(WaggaWagga$Temperature,lMin=20,lMax=141)
# Precipitation is mapped to note  pitch
scale <- c("A3", "C4", "D4", "E4", "G4", "A4", "C5", "D5", "E5", "G5") # 2-octave pentatonic A 
plist <- pitchMapping(WaggaWagga$Precipitation,pitches=scale)
# Take the lists of pitches/loudnesses and create a list of notes
notes <- getNotes(pitches=plist,loudnesses=llist)
# Take the list of notes and stack them into a list of measures.
m <- getMeasures(notes=notes,beats=4,beatType=4)
# Finally, define the score and write it into a musicXML file.
s <- score(m)
writeMXL(s,file='WaggaWaggaMelody.xml')
# That’s it! The dataset has been transformed into a musical score. 
# A music software such as museScore is required to transform the score 
# into an audio .mp3 file. 
