# ----------
# Load required packages and data
library(ggplot2);library(dplyr);library(lubridate);library(patchwork);library(av)
wd <- getwd()
source(file.path(wd,'00_funk.R'))
source(file.path(wd,'00_funk2.R'))
load(file.path(wd,"France207.RData"))
unlink('anim_im',recursive=TRUE);dir.create('anim_im') # Create folder to store image for animation 
unlink(file.path('www'),recursive=TRUE);dir.create(file.path('www')) # Create folder to store animation

# ----------
# Animation settings
# period <- 1981:2000
wetColour="#27807f"   #blue
dryColour="#b98c5a"   #brown
regimeColour= "#102128"
annualColour <- "#27807f"
dailyColour <- "#102128"
offColour="#ede3cc"
labelColour <- "grey18"
resfactor <- 2
regimeLabels <- data.frame("month"=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                           "x"=seq(1,12),
                           stringsAsFactors=FALSE)
regimeLabels$angle <- (360-(((((regimeLabels$x)*2)-1)*360)/24))+1.5

# ----------
# Data
list_sites <- as.vector(sites$ID)

for (j in 1:length(list_sites)) {
  # -----Data-----
  site <- list_sites[j]
  dati <- dat[[site]]
  
  # Monthly and annual data
  data_site <- data.frame("year"=dati$monthly$year,
                          "month"=dati$monthly$month,
                          "date"=lubridate::make_date(dati$monthly$year,dati$monthly$month),
                          "paf_regime"=dati$regime$paf[dati$monthly$month],
                          "paf_monthly"=dati$monthly$paf)
  n <- NROW(data_site)
  data_site$idx_monthly <- seq(1:n)
  data_site$annual <- dati$annual$value[match(dati$monthly$year,dati$annual$year)]
  data_site$idx_annual <- match(dati$monthly$year,dati$annual$year)
  
  # Daily data
  data_site_daily <- data.frame("year"=dati$daily$year,
                                "month"=dati$daily$month,
                                "value_daily"=dati$daily$value)
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
  
  # -----Animation-----
  anim_im <- list.files("anim_im", full.names=TRUE)  # List all images
  anim_im_full <- c(rep(anim_im[1:12],2),anim_im[13:(n+12)],rep(anim_im[(n+13):(n+24)],4))
  
  site_audio <- list.files(full.names=TRUE, pattern=paste0(site,"_"))  # List WAV files
  site_bpm <- dati$bpm
  
  # Video
  videoFramerate <- (site_bpm*4)/60
  audioDuration <- length(anim_im_full)/videoFramerate
  av_encode_video(input=anim_im_full,
                  audio=site_audio,
                  output=file.path('www',paste0(site, ".mp4")),
                  framerate=videoFramerate)
}
