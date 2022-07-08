source('00_funk.R')
instdir='/home/benjamin.renard/BEN/GitHub/sequenceR/instruments'
# NOTE: You need to create the instruments and to change instdir to the directory where they're stored.
#       This can be doned with the package sequenceR (https://github.com/benRenard/sequenceR)
#       1. Download sample packs following the instructions in subfolders of data-raw/samplePacks
#       2. Run the script data-raw/samplepack2instrument.R

# Load data and instruments
inst=loadInstruments(instdir)
load('France207.RData')

for (k in 1:NROW(sites)){
  # Show data
  message(k)
  dataset=dat[[k]]
  uglyPlot(dataset)
  # Sonify
  band=getBand(dataset$style,inst)
  song=sonify(dataset,band)
  # Save as wave then transform to mp3
  fname=paste0(sites$ID[k],'_',dataset$style,'_',dataset$bpm,'bpm')
  writeWave(song,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',paste0(fname,'.mp3')))
  file.remove('temp.wav' )
}
