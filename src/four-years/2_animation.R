library(ggplot2);library(dplyr);library(patchwork)
library(av)
resfactor=2 # Resolution factor (0.5 poor, 1 OKish, 2 good quality)
bpm=95
intro=8
compt=4
outro=40
ctext='#0a0d1b' # '#000050' 
seasons=c('#419dcc','#a7dc2c','#f0be00','#c73331')
white='#f1f1fe'
pal=colorRampPalette(c(seasons,seasons[1]))(366)

bps=bpm/60 # beat per second
tps=bps*4 # time step per second (here time step = 16th note i.e. 1/4 of a beat)
tp4=1/(bpm/60)
tp16=tp4/4
t0=compt*tp16*7

load('fourYears.RData')
load('stations.RData')
world = map_data("world") %>% filter(region=='France')

time=data.frame(date=unique(dat$date),iT=1:length(unique(dat$date)))
space=data.frame(station=stations$CODE10,lon=stations$lon,lat=stations$lat)
DF=dat %>% select(station,date,year,month,day,anomaly_smooth,normalizedQ_smooth) %>%
  left_join(time, by='date') %>%
  left_join(space, by='station') %>% 
  mutate(normalizedQ_smooth=2*(0.5-pnorm(normalizedQ_smooth)),
         foo=2*(0.5-pnorm(anomaly_smooth)),
         anomaly_smooth=foo*(foo>=0))

sz=28
monthNames=c('January','February','March','April','May','June','July',
            'August','September','October','November','December')
gmonth=ggplot(data.frame(x=1,y=-2*(1:12)+2,label=monthNames))+
  geom_text(aes(x,y,label=label),size=sz,fontface='bold',hjust=1,vjust=0.5)+
  xlim(c(-1,1))+theme_void()+theme(panel.border=element_rect(color=NA,fill=NA))
# gyear=ggplot(data.frame(x=0,y=-2*(1:4)+2,label=sprintf("%02d",unique(DF$year))))+
#   geom_text(aes(x,y,label=label),size=sz,fontface='bold',hjust=0.5,vjust=0.5)+
#   xlim(c(-1,1))+theme_void()+theme(panel.border=element_rect(color=NA,fill=NA))
g0=ggplot()+geom_polygon(data=world,aes(long,lat,group = group),fill=ctext)+
  theme_void()+coord_fixed(ratio=111/79)

colorLims=c(-1,1)
sizeLims=c(0,1)

onePlot <- function(df,a=0,b=1){
  day=as.integer(df$date[1]-as.Date(paste0(df$year[1],'-01-01'))+1)
  bckg=scales::muted(pal[day],c=10,l=150)
  # bckg=pal[day]
  g=g0+
    geom_point(data=df,aes(lon,lat,
                           color=-sign(normalizedQ_smooth)*abs(normalizedQ_smooth)^(10),
                           size=anomaly_smooth^10),alpha=0.9)+
    # scale_color_distiller('River streamflow  \n',palette='BrBG',limits=colorLims,
    #                       direction=1,breaks=c(-1,1),labels=c('  low flow','  high flow'))+
    scale_color_gradient('\n\nRiver streamflow  \n',low=pal[day],high=white,limits=colorLims,
                          breaks=c(-1,1),labels=c('  low flow','  high flow'))+
    scale_size('Anomaly\n',range=resfactor*c(1,12),limits=sizeLims,
               breaks=c(0.05,0.5,0.95),
               labels=c('  wetter than usual','      ⇣⇣⇣⇣⇣','   drier than usual'))+
    # labs(title=format(df$date[1],"%d/%m/%Y"))+
    labs(title=' ')+
    theme(legend.position='left',legend.title.position='top',
          legend.ticks = element_blank(),
          legend.title=element_text(size=resfactor*36,hjust=0.5,face='bold',color=ctext),
          legend.text=element_text(size=resfactor*28,color=ctext),
          plot.title=element_text(size=resfactor*46,hjust=0.5,face='bold',color=ctext),
          # plot.background=element_rect(fill='white'),
          plot.background=element_rect(fill=bckg),
          plot.margin=unit(resfactor*c(1,9.6,0,9.6), "lines"))+
    guides(size=guide_legend(order=1,theme=theme(legend.text.position='right')),
           color=guide_colorbar(order=2,
                                theme=theme(legend.key.width=unit(resfactor*3,"lines"),
                                            legend.key.height=unit(resfactor*8,"lines"),
                                            legend.key.spacing.y=unit(resfactor*3,"lines")))
    )
  # clock
  d1=data.frame(x=1:366,y=0.8)
  d2=d1;d2$y[abs(d2$x-day)>=3]=0
  g2=ggplot(d1)+
    # geom_col(aes(x=x,y=y,fill=factor(x)),color=NA,width=1)+
    geom_col(aes(x=x,y=y),fill=bckg,color=NA,width=1)+
    lims(y=c(0,1.2))+
    geom_hline(yintercept=0.8)+
    scale_fill_manual(values=pal)+
    geom_col(data=d2,aes(x=x,y=y),fill='black',width=1)+
    geom_text(x=1,y=1,label='January',angle=0,size=resfactor*6)+
    geom_text(x=92,y=1,label='April',angle=-90,size=resfactor*6)+
    geom_text(x=183,y=1,label='July',angle=0,size=resfactor*6)+
    geom_text(x=275,y=1,label='October',angle=90,size=resfactor*6)+
    coord_polar()+
    theme_void()+theme(legend.position='none')
  out=g+inset_element(g2,left=0.75,bottom=0.8,right=1.05,top=1.1)
  
  dim=c(31,28,31,30,31,30,31,31,30,31,30,31)
  buf=1.5;n=100
  y0=2*12*(sum(dim[1:df$month[1]])-dim[df$month[1]]+df$day[1]-1)/366
  ymin=-y0-buf;ymax=-y0+buf;w=(ymax-ymin)/n
  z0=data.frame(xmin=rep(-1,n),xmax=rep(1,n),ymin=seq(ymin,ymax-w,w),
                ymax=seq(ymin+w,ymax,w),
                f=abs(0.5*(ymin+ymax)-seq(ymin+w/2,ymax,w)))
  z1=data.frame(xmin=-1,xmax=1,ymin=ymin,ymax=ymax)
  g2a=gmonth+coord_cartesian(ylim=c(ymin,ymax),expand=FALSE)+
    geom_rect(data=z0,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,alpha=f),fill=bckg,color=NA)+
    geom_rect(data=z1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=NA,color=bckg,linewidth=resfactor*2)+
    scale_alpha(range=c(0,1),guide=NULL)
  g2b=ggplot(data.frame(x=0,y=0,label=sprintf("%02d",unique(df$year))))+
    geom_text(aes(x,y,label=label),size=sz,fontface='bold',hjust=0.5,vjust=0.5)+
    xlim(c(-1,1))+theme_void()+theme(panel.border=element_rect(color=NA,fill=NA))
  # y0=2*((df$year[1]-2021)*365+df$day[1]-1)/(365.25)
  # ymin=-y0-buf;ymax=-y0+buf;w=(ymax-ymin)/n
  # z0=data.frame(xmin=rep(-1,n),xmax=rep(1,n),ymin=seq(ymin,ymax-w,w),
  #               ymax=seq(ymin+w,ymax,w),
  #               f=abs(0.5*(ymin+ymax)-seq(ymin+w/2,ymax,w)))
  # g2b=gyear+coord_cartesian(ylim=c(ymin,ymax),expand=FALSE)+
  #   geom_rect(data=z0,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,alpha=f),fill=bckg,color=NA)+
  #   scale_alpha(range=c(0,1),guide=NULL)
  g2=wrap_plots(g2a,g2b,widths=c(2,1),nrow=1)
  out=g+inset_element(g2,left=0,bottom=0.97,right=0.7,top=1.07)
  z3=data.frame(xmin=0,xmax=1,ymin=0,ymax=1)
  g3=ggplot(z3)+theme_void()+
    geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=bckg,alpha=abs(a)/b)+
    coord_cartesian(xlim=c(0,1),ylim=c(0,1),expand=FALSE)
  out=out+inset_element(g3,left=0,bottom=0,right=1,top=1,align_to = 'full')
  return(out)
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
    a=i-(compt*7+intro*7);b=compt*7+intro*7
    print(onePlot(df,a,b))
    dev.off()
  }
  # years
  for (year in unique(DF$year)){
    # intro
    for(i in 1:(intro*7)){
      m=m+1
      if(year==unique(DF$year)[[1]]){
        a=compt*7+i-(compt*7+intro*7);b=compt*7+intro*7
      } else {
        a=i;b=intro*7
      }
      png(filename=file.path(dirname,paste0('img',sprintf("%05d",m),'.png')),
          width=(1280*resfactor),height=(720*resfactor),res=72)
      print(onePlot(df,a,b))
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

