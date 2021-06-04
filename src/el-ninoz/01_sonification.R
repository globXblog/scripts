library(musicXML)
DF=read.table('SOI_EastOzPrecip.csv',header=T,sep=';')

# SOI ------ 
foo=-1*DF$SOI # opposite SOI so that La Nina <-> low values
neutral=c(-10,10) # define bounds of "neutral SOI state"
foo[foo>neutral[1] & foo<neutral[2]]=0 # Set values in neutral SOI to zero so they won't be heard
scale <- c("E3", "G3", "A3", "B3", "D4","E4", "G4", "A4", "B4", "D5","E5")
#scale <- c("E3", "G3", "A3", "B3", "C4","D4","E4", "G4", "A4", "B4","C5", "D5","E5")
plist <- pitchMapping(foo,pitches=scale)
llist <- loudnessMapping(abs(foo))
notes <- getNotes(pitches=plist,loudnesses=llist)
notes <- tieNotes(notes)
m <- getMeasures(notes=notes,beats=3,beatType=4)
s <- score(m)
writeMXL(s,file='SOI.xml')

# Precipitation P  ------ 
foo=DF$P
foo[is.na(foo)]=0;foo[foo<=40]=0 # Set NA and small values to 0 so they won't be heard
scale <- c("E3", "G3", "A3", "B3", "D4","E4", "G4", "A4", "B4", "D5","E5")
plist <- pitchMapping(foo,pitches=scale)
llist <- loudnessMapping(abs(foo))
notes <- getNotes(pitches=plist,loudnesses=llist)
notes <- tieNotes(notes)
m <- getMeasures(notes=notes,beats=3,beatType=4)
s <- score(m)
writeMXL(s,file='P.xml')

# seasonal SOI values ------ 
ma=3 # moving averaging window (3 months here)
foo=stats::filter(x=-1*DF$SOI,filter=rep(1,ma)/ma) # apply moving averaging
threshold=-5 # only seasonal SOI values below the threshold will be heard
foo[is.na(foo)]=threshold;foo[foo>threshold]=threshold 
foo=foo[seq(1,length(foo),ma)] # subsample to get only 1 value per season rather than per month
scale <- c("E1","G1","B1","E2","G2","B2","E3")
plist <- pitchMapping(foo,pitches=scale)
# Set note duration to dotted 8th.
durations=durationMapping(rep(0,length(plist)),expMin=3,expMax=3) # 8th note = 2^3
for(i in 1:length(durations)){durations[[i]]$dot=T} # Add dot
# Create notes, score and save
notes <- getNotes(pitches=plist,durations=durations)
m <- getMeasures(notes=notes,beats=3,beatType=4)
s <- score(m)
writeMXL(s,file='SOIseasonal.xml')
