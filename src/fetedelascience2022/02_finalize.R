# TUNINGS ----
normalizeAnom=TRUE # Normalize seasonnal anomalies?
period=1968:2019
# PRELIMINARIES ----
source('00_funk.R') # Load useful functions

# TREND MAPS ----
if(!file.exists('trends.RData')){
  trends=data.frame()
  for(k in 1:202){ # Loop on stations
    for (j in 1:3){ # Loop on variables
      # get data
      what=c('P','E','Q')[j]
      stxt=switch(what,Q='Débit dans la rivière',P='Précipitations',E='Évapotranspiration potentielle','')
      load(paste0(what,'_SAFRAN.RData'))
      dataset=dat[[k]];ID=names(dat)[k]
      coord=sites[k,c('Lon','Lat')]
      nom=as.character(sites$name2[k])
      seasonnal=getData(dataset,normalizeAnom)
      message(paste0('k=',k,' - ',ID, ' - ',nom, ' - ',what))
      # get trend on annual data
      annual=seasonnal %>% filter(year %in% period) %>% group_by(year) %>% summarise(amean=mean(value))
      foo=glm(data=annual,amean~year)
      changePct=100*(foo$fitted.values[NROW(foo$fitted.values)]-foo$fitted.values[1])/foo$fitted.values[1]
      isSignif=coef(summary(foo))[2,4]<0.1
      trends=rbind(trends,data.frame(ID=ID,nom=nom,lon=coord$Lon,lat=coord$Lat,
                                     variable=stxt,
                                     trend=changePct,signif=isSignif,
                                     mean=mean(annual$amean,na.rm=TRUE)))
    }
  }
  save(trends,file='trends.RData')
}
load('trends.RData')
ncolors=7
colorMap='#c4dfe6';colorBkg= '#011a27';colorTxt='#c4dfe6'
frMap = map_data("world") %>% filter(region=='France')
DF=trends
DF$symbol=0;DF$symbol[DF$signif & DF$trend>0]=1;DF$symbol[DF$signif & DF$trend<0]=-1
DF$sign=1;DF$sign[DF$trend<0]=-1

lev=paste0('\n',levels(DF$variable))
DF$variable=factor(paste0('\n',DF$variable),levels=lev)
zlim=max(abs(DF$trend))*c(-1,1)
ltxt=paste('Évolution sur la période',period[1],'-',period[length(period)],'[%]')
pal=RColorBrewer::brewer.pal(11,'BrBG')
br=round(seq(-70,70,length.out = ncolors+1))
DF=arrange(DF,abs(trend))

g0=ggplot()+
  geom_polygon(data=frMap,aes(long,lat,group=group),fill=colorMap,alpha=0.15)+
  coord_map()+theme_void()+
  theme(plot.background=element_rect(fill=colorBkg),
        legend.position='bottom',
        legend.title = element_text(color=colorTxt,size=16),
        legend.text = element_text(color=colorTxt,size=12),
        strip.text = element_text(color=colorTxt,size=18,vjust=1.5))
g=g0+geom_point(data=DF,aes(lon,lat,fill=trend,shape=factor(sign),size=abs(trend)),alpha=0.8)+
  scale_fill_stepsn(ltxt,colors=pal,breaks=br,limits=zlim)+
  # scale_fill_distiller(ltxt,palette='BrBG',direction=1,limits=zlim)+
  # scale_shape_manual(values=c('triangle down filled','circle filled','triangle filled'),guide=NULL)+
  # scale_shape_manual(values=c('triangle down filled','triangle filled'),guide=NULL)+
  scale_shape_manual(values=c('circle filled','circle filled','circle filled'),guide=NULL)+
  # scale_size_manual(values=c(2,4),guide=NULL)+
  scale_size_continuous(range=c(1,5),guide=NULL)+
  facet_wrap(.~variable,nrow=1)+
  guides(fill = guide_colourbar(direction='horizontal',title.position='top',title.hjust=0.5,
                                barwidth=unit(4,'in'),frame.colour=colorTxt,ticks=FALSE))

pdf(file='trends.pdf',height=5,width=11,useDingbats=FALSE)
g
dev.off()

system('pdftoppm trends.pdf trends -jpeg -rx 300 -ry 300')

# Final multi-station videos ----
load('Q_SAFRAN.RData')
set.seed(123456)
ix=sample(1:NROW(sites))
# concatStations(sites$ID[ix],'FeteDeLaScience2022_full')
nvid=12
frMap = map_data("world") %>% filter(region=='France')
g0=ggplot(frMap)+geom_polygon(aes(long,lat,group=group))+theme_void()+coord_map()
gs=list()
for(k in 1:((NROW(sites)%/%nvid)+1)){
  jx=((k-1)*nvid)+(1:12)
  jx=jx[jx<=NROW(sites)]
  if(length(jx)>0){
    concatStations(sites$ID[ix[jx]],paste0('FeteDeLaScience2022_',k))
    gs[[k]]=g0+geom_point(data=sites[ix[jx],],aes(Lon,Lat),col='yellow')
  }
}

pdf(file='vids.pdf',height=5,width=11,useDingbats=FALSE)
gridExtra::grid.arrange(grobs=gs,ncol=5)
dev.off()
