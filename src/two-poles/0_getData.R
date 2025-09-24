library(tidyr);library(dplyr)
library(ggplot2);library(patchwork)

url0='https://noaadata.apps.nsidc.org/NOAA/G02135'

# Monthly extents
temp=tempdir()
xlist=vector('list',12)

for(i in 1:12){
  foo=c()
  for(what in c('north','south')){
    prefix=ifelse(what=='north','N','S')
    fname=paste0(prefix,'_',sprintf('%02i',i),'_extent_v4.0.csv')
    target=file.path(url0,what,'monthly','data',fname,fsep='/')
    dest=file.path(temp,'foo.csv')
    download.file(url=target,destfile=dest)
    X=read.table(dest,header=TRUE,sep=',',na.strings='  -9999')
    foo=rbind(foo,X)
  }
  xlist[[i]]=foo
}
DF=bind_rows(xlist) %>% rename(month=mo) %>% mutate(region=factor(trimws(region)))
clim=DF %>% group_by(month,region) %>% summarise(m=mean(extent,na.rm=TRUE),s=sd(extent,na.rm=TRUE))
DF=DF %>% left_join(clim) %>% 
  mutate(anomaly=extent-m,sanomaly=anomaly/s)

g1=ggplot(DF)+geom_line(aes(month,extent,group=year,color=year))+
  scale_color_distiller(palette='Spectral')+
  scale_y_continuous(trans='log10')+
  facet_wrap(.~region,scales='free_y')+
  theme_bw()
g2=ggplot(DF)+geom_line(aes(month,sanomaly,group=year,color=year))+
  scale_color_distiller(palette='Spectral')+
  facet_wrap(.~region)+
  theme_bw()
wrap_plots(g1,g2,ncol=1)
save(DF,file='extent_monthly.RData')
