library(ggplot2);library(dplyr)
library(av)
resfactor=2 # Resolution factor (0.5 poor, 1 OKish, 2 good quality)
bpm=95
intro=8
compt=4
outro=24
ctext='#000050'



bps=bpm/60 # beat per second
tps=bps*4 # time step per second (here time step = 16th note i.e. 1/4 of a beat)
tp4=1/(bpm/60)
tp16=tp4/4
t0=compt*tp16*7

load('fourYears.RData')
load('stations.RData')
world = map_data("world") %>% filter(region=='France')
seasons=scales::hue_pal(l=90,c=20,h.start=-150,direction=-1)(366)

time=data.frame(date=unique(dat$date),iT=1:length(unique(dat$date)))
space=data.frame(station=stations$CODE10,lon=stations$lon,lat=stations$lat)
DF=dat %>% select(station,date,year,month,day,anomaly_smooth,normalizedQ_smooth) %>%
  left_join(time, by='date') %>%
  left_join(space, by='station') %>% 
  mutate(normalizedQ_smooth=2*(0.5-pnorm(normalizedQ_smooth)),
         foo=2*(0.5-pnorm(anomaly_smooth)),
         anomaly_smooth=foo*(foo>=0))

g0=ggplot()+geom_polygon(data=world,aes(long,lat,group = group),fill=ctext)+
  theme_void()+coord_fixed(ratio=111/79)

# colorLims=max(abs(DF$normalizedQ_smooth),na.rm=TRUE)*c(-1,1)
# sizeLims=c(min(DF$anomaly_smooth,na.rm=TRUE),0)
colorLims=c(-1,1)
sizeLims=c(0,1)

onePlot <- function(df){
  day=as.integer(df$date[1]-as.Date(paste0(df$year[1],'-01-01'))+1)
  g=g0+
    geom_point(data=df,aes(lon,lat,
                           color=-sign(normalizedQ_smooth)*abs(normalizedQ_smooth)^(1/2),
                           size=anomaly_smooth^10),alpha=0.8)+
    scale_color_distiller('River streamflow  \n',palette='BrBG',limits=colorLims,
                          direction=1,breaks=c(-1,1),labels=c('  low flow','  high flow'))+
    scale_size('\n\nAnomaly\n',range=resfactor*c(1,12),limits=sizeLims,
               breaks=c(0.1,0.5,0.9),
               labels=c('  wetter than usual','      ⇣⇣⇣⇣⇣','  drier than usual'))+
    labs(title=format(df$date[1],"%d/%m/%Y"))+
    theme(legend.position='left',legend.title.position='top',
          legend.ticks = element_blank(),
          legend.title=element_text(size=resfactor*36,hjust=0.5,face='bold',color=ctext),
          legend.text=element_text(size=resfactor*28,color=ctext),
          plot.title=element_text(size=resfactor*46,hjust=0.5,face='bold',color=ctext),
          plot.background=element_rect(fill=seasons[day]),
          plot.margin=unit(resfactor*c(1,9.6,0,9.6), "lines"))+
    guides(size=guide_legend(theme=theme(legend.text.position='right')),
           color=guide_colorbar(order=1,
                                theme=theme(legend.key.width=unit(resfactor*6,"lines"),
                                            legend.key.height=unit(resfactor*8,"lines"),
                                            legend.key.spacing.y=unit(resfactor*3,"lines")))
    )
  return(g)
}

makeplot <- function(){
  dirname='img'
  dir.create(dirname)
  k=1
  m=0
  df=DF %>% filter(iT==k) %>% mutate(i=row_number(date))
  # counting
  for(i in 1:(compt*7)){
    m=m+1
    png(filename=file.path(dirname,paste0('img',sprintf("%05d",m),'.png')),
                  width=(1280*resfactor),height=(720*resfactor),res=72)
    print(onePlot(df))
    dev.off()
    }
  # years
  for (year in unique(DF$year)){
    # intro
    for(i in 1:(intro*7)){
      m=m+1
      png(filename=file.path(dirname,paste0('img',sprintf("%05d",m),'.png')),
          width=(1280*resfactor),height=(720*resfactor),res=72)
      print(onePlot(df))
      dev.off()
    }  
    # data
    for(i in 1:(365+(year==2024))){
      message(paste0(year,':',i))
      df=DF %>% filter(iT==k)
      m=m+1
      png(filename=file.path(dirname,paste0('img',sprintf("%05d",m),'.png')),
          width=(1280*resfactor),height=(720*resfactor),res=72)
      print(onePlot(df))
      dev.off()
      k=k+1
    }  
  }
  # outro
  for(i in 1:(outro*7)){
    m=m+1
    png(filename=file.path(dirname,paste0('img',sprintf("%05d",m),'.png')),
        width=(1280*resfactor),height=(720*resfactor),res=72)
    print(onePlot(df))
    dev.off()
  }
}

makeplot()

flist=list.files("img", full.names=TRUE)
fadeFilter <- paste0("fade=t=in:st=0:d=", 7*(compt+intro)/tps, ":alpha=0", ", fade=t=out:st=", length(flist)/tps-7*outro/tps, ":d=", 7*outro/tps, ":alpha=0")
av_encode_video(input=flist,audio='backing.mp3',output='fourYears.mp4',
                framerate=tps,vfilter=fadeFilter)

