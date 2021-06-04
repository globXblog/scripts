library(musicXML)
scale <- 'arabic' # 'pentatonic' # 'rumba'
addBeat <- FALSE # if TRUE, add 1 beat to create a 4-beat measure
orderCol=14 # column in the dataset used for ordering stations

# read data
load('dataRunoff.RData')
w=sort.int(dataRunoff[,orderCol],index.return = TRUE)
Q <- dataRunoff[w$ix,]
names(Q)[5] <- 'QoverSum'
# choose scale 
pitches=switch(scale,
               arabic=c("E3", "F3", "G#3", "A3", "B3","C4",#"D#4",
                        "E4", "F4", "G#4", "A4", "B4","C5",#"D#5",
                        "E5", "F5", "G#5", "A5", "B5","C6"),#,"D#6","E6")
               pentatonic=c("A3", "C4", "D4", "E4", "G4",
                            "A4", "C5", "D5", "E5", "G5",
                            "A5", "C6", "D6", "E6", "G6"),
               rumba=c("A3", "B3", "C4", "D4", "E4","F4",
                       "A4", "B4", "C5", "D5", "E5","F5",
                       "A5", "B5", "C6", "D6", "E6","F6"),
               NULL)
# create notes
notes0 <- getNotes(pitches=pitchMapping(Q$QoverSum,pitches=pitches),
                  loudnesses=loudnessMapping(Q$meanQyear,lMin=71,lMax=124),
                  durations=durationMapping(Q$missingQ,expMin=4,expMax=4))
notes0 <- tieNotes(notes0)
# Add one beat to get 4/4 measures
if(addBeat){
  k <- 0
  notes <- c()
  while(k+12<=length(notes0)){
    # handle ties and create 12-note chunk
    notes0[[k+1]]$tie2previous=FALSE # remove tie on 1st note
    notes0[[k+1]]$tie2next=TRUE # add tie on 12th note
    twelve <- notes0[k+(1:12)]
    # create additional 1-beat note
    foo <- notes0[[k+12]]
    nunote <- note(p=foo$p,l=foo$l,tie2next=T,tie2previous=T,
                   d=duration(type=16,mxlDivision=foo$d$mxlDivision,
                              mxlDuration=foo$d$mxlDuration))
    nunotes <- rep(list(nunote),4)
    nunotes[[4]]$tie2next=FALSE
    # rbind it all together
    sixteen <- c(twelve,nunotes)
    notes <- c(notes,sixteen)
    k <- k+12
  }
} else {
  notes <- notes0
}
# get score
m <- getMeasures(notes=notes, beats=ifelse(addBeat,4,3), beatType = 4)
s <- score(m)
# write XML file
fname=paste0('OZdataset_',scale,'_',ifelse(addBeat,'4beats','3beats'),
             '_',names(Q)[orderCol],'.xml')
writeMXL(s,file=fname)
