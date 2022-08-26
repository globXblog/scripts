# TUNINGS ----
bpm=90 # Tempo
normalizeAnom=TRUE # Normalize seasonnal anomalies?
resfactor=2 # Resolution factor (0.5 poor, 1 OKish, 2 good quality)
intro=0 # Number of years before first note is played
outro=10 # Number of years after last note is played
doIntroVid=TRUE # if FALSE, intro videos are skipped 
doMainVid=FALSE # if FALSE, main video are skipped
instdir='/home/benjamin.renard/BEN/GitHub/sequenceR/instruments' # Instrument directory
# NOTE: You need to create the instruments and to change instdir to the directory where they're stored.
#       This can be doned with the package sequenceR (https://github.com/benRenard/sequenceR)
#       1. Download sample packs following the instructions in subfolders of data-raw/samplePacks
#       2. Run the script data-raw/samplepack2instrument.R

# PRELIMINARIES ----
seasons=c('DJF','MAM','JJA','SON') # Definition of seasons
source('00_funk.R') # Load useful functions
getQRcode('https://globxblog.inrae.fr/fetedelascience2022/') # Create QRcode image
if(!exists('inst')) inst=loadInstruments(instdir)
bps=bpm/60 # beat per second
tps=bps*4 # time step per second (here time step = 16th note i.e. 1/4 of a beat)
spt=1/tps # seconds per time step

for(k in 202:202){ # Loop on stations
  for (j in 1:3){ # Loop on variables
    # get style, datasets, output file name etc.
    what=c('P','E','Q')[j]
    style=switch(what,Q='oriental',E='minor',P='major')
    load(paste0(what,'_SAFRAN.RData'))
    dataset=dat[[k]];ID=names(dat)[k]
    coord=sites[k,c('Lon','Lat')]
    nom=as.character(sites$name2[k])
    fname=paste0(ID,'_',j,'_',what)
    seasonnal=getData(dataset,normalizeAnom)
    
    if(doMainVid){
      # CREATE SOUND ----
      message('-----------------------------------------')
      message(paste0('k=',k,' - ',ID, ' - ',nom, ' - ',what))
      createSound(seasonnal,ID,fname,normalizeAnom,style,bpm,seasons,t0=intro*4*spt)
      
      # CREATE MAIN VIDEO ----
      makeplot <- function(){
        tmin=1-intro*4
        tmax=NROW(seasonnal)+outro*4
        for(tstep in tmin:tmax){
          g=plotOneStep(seasonnal,tstep,tmax,what,coord,nom,seasons=seasons)
          print(g)
        }
      }
      av_capture_graphics(makeplot(),output=paste0(fname,'.mp4'),
                          audio=paste0(fname,'.mp3'),
                          res=72*resfactor, width=1280*resfactor,height=720*resfactor,
                          framerate=tps)
    }
  }
  # CREATE INTRO VIDEO ----
  if(doIntroVid){
    # Intro scene
    dIntroScene=8;tFadeIn=1;tFadeOut=2
    # Create sound
    createTheme(t1=tFadeIn,t2=2,t3=4.5,t4=5.5,trimTo=dIntroScene)
    # Create animation
    makeplot <- function(){
      tmin=1;tmax=round(tps*dIntroScene)
      for(tstep in tmin:tmax){
        print(plotMap(coord=coord,nom=paste0('Rivière : ',nom)))
      }
    }
    fadeFilter <- paste0("fade=t=in:st=0:d=", tFadeIn, ":alpha=0:color=#011a27", 
                         ", fade=t=out:st=", dIntroScene-tFadeOut-0.5, ":d=", tFadeOut, ":alpha=0:color=#011a27")
    av_capture_graphics(makeplot(),output=paste0(ID,'_0.mp4'),audio='theme.mp3',
                        res=72*resfactor, width=1280*resfactor,height=720*resfactor,
                        framerate=tps,vfilter=fadeFilter)
  }
  
  # FINAL CONCATENATION INTRO + 3 VARS ----
  concatVids(ID)
}
  
