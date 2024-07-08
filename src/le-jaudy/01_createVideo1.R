ID='J2023010'
coord=data.frame(Lon=-3.270208,Lat=48.713804)
nom='Le Jaudy à Mantallot '
resfactor=2 # Resolution factor (0.5 poor, 1 OKish, 2 good quality)
bpm=90 # Tempo
intro=0 # Number of years before first note is played
outro=10 # Number of years after last note is played
instdir='/home/benjamin.renard/BEN/GitHub/sequenceR/instruments' # Instrument directory
# NOTE: You need to create the instruments and to change instdir to the directory where they're stored.
#       This can be done with the package sequenceR (https://github.com/benRenard/sequenceR)
#       1. Download sample packs following the instructions in subfolders of data-raw/samplePacks
#       2. Run the script data-raw/samplepack2instrument.R

# PRELIMINARIES ----
seasons=c('DJF','MAM','JJA','SON') # Definition of seasons
source('00_funk1.R') # Load useful functions
if(!exists('inst')) inst=loadInstruments(instdir)
bps=bpm/60 # beat per second
tps=bps*4 # time step per second (here time step = 16th note i.e. 1/4 of a beat)
spt=1/tps # seconds per time step

QJfile=paste0(ID,'.csv')

S=getQJ(QJfile) %>% getSeasonalData()

for(style in c('oriental','minor','major')){
  
  fname=paste0(ID,'_',style)
  
  createSound(S,ID,fname,style=style)
  
  # CREATE MAIN VIDEO ----
  makeplot <- function(){
    tmin=1-intro*4
    tmax=NROW(S)+outro*4
    for(tstep in tmin:tmax){
      g=plotOneStep(S,tstep,tmax,what='Q',coord=coord,nom,seasons=seasons)
      print(g)
    }
  }
  av_capture_graphics(makeplot(),output=file.path(paste0(fname,'.mp4')),
                      audio=file.path(paste0(fname,'.mp3')),
                      res=72*resfactor, width=1280*resfactor,height=720*resfactor,
                      framerate=tps)
  
}
