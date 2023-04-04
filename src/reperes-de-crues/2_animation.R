library(ggplot2);library(patchwork);library(dplyr);library(av)
library(sf);library(rnaturalearth)
library(BFunk) # https://github.com/benRenard/BFunk

bpm=120
intro=10
outro=60
resfactor=3

#colors
cmap='gray96'
criver='#000050'
csite='darkgray'
csiteActive='#16253D'
cmark='#ff7407'
cstation='#16253D'
cstationActive='#ff7407'
cflood='#ff7407'
cHCI='#16253D'
cpoint='#ff7407'
smallDot=resfactor*2
largeDot=resfactor*4
hugeDot=resfactor*8
stitle=resfactor*26
smedium=resfactor*20
ssmall=resfactor*14
syear=resfactor*18
ssegment=resfactor*1

# --- READ DATA ---- 
HCI=read.table('HCIs.csv',header=TRUE)
# Data read below can be downloaded at https://doi.org/10.5281/zenodo.6793500
sites=read.table(file.path('data','sites.txt'),header=TRUE)
marks=read.table(file.path('data','marksAtSites.txt'),header=TRUE)
stations=read.table(file.path('data','stations.txt'),header=TRUE)
peaks=read.table(file.path('data','peaksAtStations.txt'),header=TRUE)
reconstruct=read.table('reconstructions.txt',header=TRUE)
# France
France=ne_countries(country="France",scale='large',type='map_units',returnclass='sf') %>%
  filter(geounit=='France')
# Rivers - data from HYDROSHEDS, https://www.hydrosheds.org/products/hydrorivers
rivers=read_sf(dsn = 'HydroRIVERS_v10_eu_shp', layer = 'HydroRIVERS_v10_eu') %>% 
  filter(ORD_STRA>1) %>%st_intersection(France)

# Add empirical return period to peaks
peaks=peaks %>% group_by(ID) %>% mutate(rank=rank(amax),n=n(),RP=1/(1-(rank-0.5)/n))
# rearrange HCIs
HCIs=rbind(data.frame(time=HCI$time,val=HCI$HCI1,u=HCI$uHCI1,component='HCI1'),
           data.frame(time=HCI$time,val=HCI$HCI2,u=HCI$uHCI2,component='HCI2'))

# base plots and maps
g0=ggplot(rivers) +
  geom_sf(data=France,fill=cmap)+
  geom_sf(aes(linewidth=ORD_STRA,alpha=ORD_STRA),color=criver,show.legend=FALSE)+
  scale_linewidth(range=c(0.4,1))+scale_alpha(range=c(0.2,1))+
  theme_void()

g0marks=g0+scale_linewidth(range=c(0.2,0.5))+
  labs(title='Flood marks')+
  theme(plot.title=element_text(size=stitle))

g0peaks=g0+scale_linewidth(range=c(0.2,0.5))+
  geom_point(data=stations,aes(lon,lat),size=smallDot,shape=21,color='gray30',fill=cstation)+
  labs(title='Active stations')+
  theme(plot.title=element_text(size=stitle))

g0HCI=ggplot(HCIs)+
  geom_ribbon(aes(x=time,ymin=val-0.5*u,ymax=val+0.5*u),fill=cHCI,color=NA,alpha=0.2)+
  facet_wrap(vars(component),ncol=1)+
  labs(title='Hidden Climate Indices')+
  coord_cartesian(ylim=c(-3,3))+
  theme_void()+theme(strip.text=element_blank(),plot.title=element_text(size=stitle))

g0rec=g0+coord_sf(expand=FALSE)+
  labs(title='Probability that a 10-year flood occured')+
  theme(legend.position=c(0.12,0.35),legend.title=element_text(size=smedium),legend.text=element_text(size=ssmall),
        legend.key.width=unit(0.02,'npc'),legend.key.height=unit(0.04,'npc'),
        plot.title=element_text(size=stitle))

from=min(HCI$time)-outro
upto=max(HCI$time)+intro
ylist=upto:from
for(year in ylist){
  message(year)
  fname=file.path("anim_im",paste0(sprintf("%04d",year),".png"))
  now=ifelse(year<min(HCI$time),min(HCI$time),ifelse(year>max(HCI$time),max(HCI$time),year))
  # marks
  gmarks=g0marks+
    geom_point(data=sites %>% filter(firstMark<=now,lastMark>=now),aes(lon,lat),
               shape=21,color='gray30',fill=csiteActive,size=smallDot)+
    geom_point(data=marks %>% filter(hydroYear==now) %>% left_join(sites,by='ID'),aes(lon,lat),
               shape=21,color='gray30',fill=cmark,size=largeDot)
  
  # peaks
  df=peaks %>% filter(hydroYear==now)%>% left_join(stations,by='ID')
  gpeaks=g0peaks+
    geom_point(data=df,aes(lon,lat),shape=21,color='gray30',fill=cstationActive,size=largeDot)
    
  # First two HCIs
  gHCI=g0HCI+
    geom_ribbon(data=HCIs %>% filter(time<=now),aes(x=time,ymin=val-0.5*u,ymax=val+0.5*u),fill=cHCI,color=NA)+
    geom_segment(data=HCIs %>% filter(time==now),aes(x=time,xend=time,y=val-0.5*u,yend=val+0.5*u),color=cpoint,linewidth=ssegment)+
    geom_point(data=HCIs %>% filter(time==now),aes(x=time,y=val),shape=21,color=cpoint,fill=cpoint,size=largeDot)+
    facet_wrap(vars(component),ncol=1)
  
  # reconstructions
  df=reconstruct %>% filter(year==now)
  grec=g0rec+geom_point(data=df,aes(lon,lat,fill=prob,size=prob),shape=21,color='gray30')+
    scale_fill_distiller('Probability',palette='YlGnBu',trans='sqrt',direction=1,limits=c(0,1),breaks=c(0,0.1,1))+
    scale_size(trans='exp',limits=c(0,1),range=c(smallDot,hugeDot),guide=NULL)+
    annotate(geom='text',label=paste('\u23EA',now),x=5.5,y=42,size=syear)
    
  design <- "DDBA
             DDCC"
  png(filename=fname,width=(1280*resfactor), height=(720*resfactor), res=72)
  print(wrap_plots(A=gpeaks,B=gmarks,D=grec,C=gHCI,design=design))
  dev.off()
}

fps=(bpm*2)/60
flist=list.files("anim_im", full.names=TRUE)
av_encode_video(input=rev(flist),audio='ReperesDeCrues_backward.wav',output='ReperesDeCrues.mp4',framerate=fps)
 