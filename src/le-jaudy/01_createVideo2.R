ID='J2023010'
nom='Le Jaudy à Mantallot '

musicPar=data.frame(bpm=c(82,88,94,103,112),
                    style=c('arabic','blues','penta','flamenco','funk'))

instdir='/home/benjamin.renard/BEN/GitHub/sequenceR/instruments' # Instrument directory
# NOTE: You need to create the instruments and to change instdir to the directory where they're stored.
#       This can be done with the package sequenceR (https://github.com/benRenard/sequenceR)
#       1. Download sample packs following the instructions in subfolders of data-raw/samplePacks
#       2. Run the script data-raw/samplepack2instrument.R

# PRELIMINARIES ----
source('00_funk2.R') # Load useful functions
if(!exists('inst')) inst=loadInstruments(instdir)
wd <- getwd()
unlink('anim_im',recursive=TRUE);dir.create('anim_im') # Create folder to store image for animation 

QJfile=paste0(ID,'.csv')
daily=getQJ(QJfile) 
dat=getData(daily,period=1982:2023)

# Monthly and annual data
data_site <- data.frame("year"=dat$monthly$year,
                        "month"=dat$monthly$month,
                        "date"=lubridate::make_date(dat$monthly$year,dat$monthly$month),
                        "paf_regime"=dat$regime$paf[dat$monthly$month],
                        "paf_monthly"=dat$monthly$paf)
n <- NROW(data_site)
data_site$idx_monthly <- seq(1:n)
data_site$annual <- dat$annual$value[match(dat$monthly$year,dat$annual$year)]
data_site$idx_annual <- match(dat$monthly$year,dat$annual$year)

# Daily data
data_site_daily <- data.frame("year"=dat$daily$year,
                              "month"=dat$daily$month,
                              "value_daily"=dat$daily$value)
data_site_daily$idx <- seq(1:NROW(data_site_daily))
data_site_daily$id <- match(paste0(data_site_daily$year,data_site_daily$month),paste0(data_site$year,data_site$month))

# Calculate new x for daily data to stick with annual plot x-axis range
stepx <- ((n+0.5)-0.5)/(NROW(data_site_daily)-1)
data_site_daily$new_x <- 0.5+((data_site_daily$idx-1)*stepx)

# Calculate polygon coordinates to draw bars
pafPolygons <- getPafPolygons(pafData=data_site)
annualPolygons <- getAnnualPolygons(annualData=data_site)

# -----Plot-----
# Create empty plots
pafPlot0 <- getPafPlot0(annualData=data_site, pafPolygons=pafPolygons,regimeColour=offColour)
annualDailyPlot0 <- getAnnualDailyPlot0(dailyData=data_site_daily, annualPolygons=annualPolygons,
                                        annualColour=annualColour, dailyFullColour=offColour)
regimePlot0 <- ggplot(data=data_site[1:12,])+
  geom_col(aes(x=idx_monthly, y=paf_regime), fill=offColour)+
  coord_polar()+
  scale_x_continuous(limits=c(0.5,12.5))+
  theme_void()+
  geom_text(aes(x=regimeLabels$x,y=rep((max(data_site$paf_regime[1:12])*1.1),12),label=regimeLabels$month,
                angle=regimeLabels$angle),colour=labelColour, size=14)

# Save 12 first plots (only regimePlot is changing)
for (i in 1:12){
  print(paste0(i,"/",12))
  regime_mask <- data_site[data_site$idx_monthly==i,]
  regimePlot <- regimePlot0+geom_col(data=regime_mask, aes(x=month,y=paf_regime),fill=regimeColour)
  finalPlots <- regimePlot+annualDailyPlot0/pafPlot0
  png(filename=file.path(wd,"anim_im",paste0(sprintf("im_1_%03d", i),".png")),
      width=(1280*resfactor), height=(0.25*720*resfactor), res=36)
  print(finalPlots)
  dev.off()
}

# Create all other plots
for (i in 1:n){
  print(paste0(i,"/",n))
  # Create regime circular plot
  regime_mask <- data_site[data_site$idx_monthly==i,]
  regimePlot <- regimePlot0+geom_col(data=regime_mask, aes(x=month,y=paf_regime),fill=regimeColour)
  # Add monthly paf (coloured bars) to pafPlot0
  paf_mask <- pafPolygons[pafPolygons$id<=i,]
  pafPlot <- pafPlot0+geom_polygon(data=paf_mask, aes(x=x,y=y,group=id,fill=fill))+
    scale_fill_manual(values=c("wet"=wetColour, "dry"=dryColour), guide=NULL)
  # Add annual and daily values (grey bars and black line) to annualDailyPlot0
  annual_mask <- annualPolygons[annualPolygons$id<=i,]
  daily_mask <- data_site_daily[data_site_daily$id<=i,]
  iyear <- max(daily_mask$year, na.rm=TRUE)
  # Get annual segment coordinates
  annual_seg <- dplyr::group_by(.data=annual_mask, id)
  annual_seg <-  summarise(.data=annual_seg,xstart=min(x),xend=max(x),ystart=min(y), yend=max(y))
  annualDailyPlot <- annualDailyPlot0+geom_polygon(data=annual_mask, aes(x=x,y=y,group=id),fill=annualColour, alpha=0.2)+
    geom_line(data=daily_mask, aes(x=new_x,y=value_daily),colour=dailyColour)+
    geom_segment(data=annual_seg, aes(x=xstart, y=yend, xend=xend,yend=yend), colour=annualColour)+
    geom_text(aes(x=daily_mask$new_x[daily_mask$year==iyear][1],y=max(data_site_daily$value_daily, na.rm=TRUE)*0.97, label=iyear),
              hjust=0, colour=regimeColour, size=14)
  # Gather all plots
  finalPlots <- regimePlot+annualDailyPlot/pafPlot
  # Save finalPlot
  png(filename=file.path(wd,"anim_im",paste0(sprintf("im_2_%03d", i),".png")),
      width=(1280*resfactor), height=(0.25*720*resfactor), res=36)
  print(finalPlots)
  dev.off()
}

# Save 12 last plots (only regimePlot is changing)
for (i in 1:12){
  print(paste0(i,"/",12))
  regime_mask <- data_site[data_site$idx_monthly==i,]
  regimePlot <- regimePlot0+geom_col(data=regime_mask, aes(x=month,y=paf_regime),fill=regimeColour)
  finalPlots <- regimePlot+annualDailyPlot/pafPlot
  png(filename=file.path(wd,"anim_im",paste0(sprintf("im_3_%03d",i),".png")),
      width=(1280*resfactor), height=(0.25*720*resfactor), res=36)
  print(finalPlots)
  dev.off()
}

for (k in 1:NROW(musicPar)){
  dat$bpm=musicPar$bpm[k]
  dat$style=musicPar$style[k]
  bps=dat$bpm/60 # beat per second
  tps=bps*4 # time step per second (here time step = 16th note i.e. 1/4 of a beat)
  dat$spt=1/tps # seconds per time step
  
  # Sonify
  band=getBand(dat$style,inst)
  song=sonify(dat,band)
  # Save as wave then transform to mp3
  fname=paste0(ID,'_',dat$style,'_',dat$bpm,'bpm')
  writeWave(song,'temp.wav')
  system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',paste0(fname,'.mp3')))
  file.remove('temp.wav' )
  
  # -----Animation-----
  anim_im <- list.files("anim_im", full.names=TRUE)  # List all images
  anim_im_full <- c(rep(anim_im[1:12],2),anim_im[13:(n+12)],rep(anim_im[(n+13):(n+24)],4))
  
  site_audio <- list.files(full.names=TRUE, pattern=fname)  # List WAV files
  
  # Video
  videoFramerate <- (dat$bpm*4)/60
  audioDuration <- length(anim_im_full)/videoFramerate
  av_encode_video(input=anim_im_full,
                  audio=site_audio,
                  output=file.path(paste0(fname,".mp4")),
                  framerate=videoFramerate)
}

