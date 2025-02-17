n=366
four=c()
four=rbind(four,c('#2E9599','#F7DC68','#F46C3F','#A7226F'))
four=rbind(four,c('#D8EAF0','#136C4F','#F0BD42','#FD8111'))
four=rbind(four,c('#01BFFF','#A7FC01','#FFFE00','#FF7F00'))
four=rbind(four,c('#6E6E6E','#719A32','#F5CB48','#985D0A'))
four=rbind(four,c('#269fd8','#b0cd44','#ede428','#e51b24'))
four=rbind(four,c('#283371','#0c4729','#bf850d','#510e19'))
four=rbind(four,c('#4d84c4','#4cb477','#e6ce12','#e56773'))
four=rbind(four,c('#3858a1','#097c39','#e6cd11','#b9172e'))
four=rbind(four,c('#419dcc','#a7dc2c','#f0be00','#c73331'))
four=rbind(four,c('#419dcc','#a7dc2c','#f0be00','#dd513c'))
# four=c('#','#','#','#')
plot(NA,xlim=c(0,366),ylim=c(0,NROW(four)+1))
for(i in 1:NROW(four)){
  points(rep(i,n),col=(colorRampPalette(c(four[i,],four[i,1]))(n)),pch=19,cex=3)
}

library(ggplot2)
df=data.frame(x=1:366,y=1)
df2=df;df2$y[abs(df2$x-275)>=2]=0
ramp=colorRampPalette(c(four[i,],four[i,1]))(n)
ggplot(df)+geom_col(aes(x=x,y=1,fill=factor(x)),color=NA,width=1)+
  scale_fill_manual(values=ramp)+
  geom_col(data=df2,aes(x=x,y=y),fill='black',width=1)+
  geom_text(x=1,y=1.1,label='January',angle=0)+
  geom_text(x=92,y=1.1,label='April',angle=-90)+
  geom_text(x=183,y=1.1,label='July',angle=0)+
  geom_text(x=275,y=1.1,label='October',angle=90)+
  coord_polar()+
  theme_void()+theme(legend.position='none')

library(dplyr)
load('fourYears.RData')
df=dat %>% filter(station==dat$station[1]) %>% select(date,day,month,year) %>% 
  mutate(k=1:NROW(df))
g0=ggplot(df)+geom_text(aes(y=k,label=day),x=0)+
  theme_void()
i=10
g0+coord_cartesian(ylim=c(i-1,i+1))

