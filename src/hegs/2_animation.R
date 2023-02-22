source('funk.R')
bpm=120
resfactor=3
nIntro=4*4*6
nOutro=2*4*4*6
# # grays
# bcolor='#020406' # '#063852'
# mcolor='#0e181e' #'#011A27'
# pcolor='white' # '#E6DF44'
# fcolor= '#FFC619' #'#F0810F'
# oranges
bcolor='#112330'
mcolor='#09101d' 
pcolor='#fe3608'
fcolor='#ffff78' 
acolor='#cbe1f5'
tcolor='#a2b4c4'

pal=colorRampPalette(c(pcolor,fcolor))(10)
vars=c('Q','P')

for(k in 1:2){
  # Data ----
  v=vars[k]
  load(paste0(v,'.RData'))
  D$time=D$year+(D$month-0.5)/12
  D$monthFromStart=(D$year-1916)*12+D$month
  # Base figures ----
  g0=BFunk::getWorldMap(fill=mcolor)
  g0=g0+geom_point(data=sites,aes(lon,lat),size=1,alpha=0.6,shape=16,color=acolor)+
    theme(plot.background=element_rect(fill=bcolor,color=bcolor),
          panel.background=element_rect(fill=NA,color=NA),
          panel.border=element_rect(fill=NA,color=NA))
  avail=D %>% group_by(monthFromStart,year,month,time) %>% summarise(n=n())
  foo=data.frame(x=range(avail$time),y=NROW(sites))
  h0=ggplot(avail,aes(time,n))
  for(s in seq(1,0,-0.1)){
    h0=h0+geom_line(data=foo,aes(x,y),color=acolor,linewidth=10*s,alpha=0.03/s)
  }
  h0=h0+geom_line(data=foo,aes(x,y),color=acolor,linewidth=0.1)+
    annotate('text',x=min(avail$time),y=NROW(sites),label=paste0('Total: ',NROW(sites)),size=14,color=acolor,hjust=-0.2,vjust=-0.5,alpha=0.6)+
    scale_y_continuous(name='Available stations',limits=c(0,NROW(sites)*1.15),expand=expansion())+
    scale_x_continuous(name='Time',limits=range(avail$time),expand=expansion())+
    theme(plot.background=element_rect(fill=bcolor,color=bcolor),
          panel.background=element_rect(fill=NA,color=NA),
          panel.border=element_rect(fill=NA,color=NA),
          panel.grid=element_blank(),
          axis.text=element_blank(),axis.title=element_text(size=54,color=tcolor),
          axis.line=element_line(color=tcolor,arrow=arrow(length=unit(0.35,"inches"),type='closed')))
  h=h0+annotate('text',x=mean(avail$year),y=NROW(sites)/2,label=min(avail$year)-1,size=180,color=acolor,alpha=0.1)
  fig=g0/h+plot_layout(heights=c(2.3,1))
  png(filename=file.path(paste0('var',k,sprintf("_im_%04d",0),".png")),
      bg=bcolor,width=(1280*resfactor), height=(720*resfactor), res=72)
  print(fig)
  dev.off()
  # Intro ----
  upto=ifelse(k==1,nIntro,nIntro/2)
  lab=ifelse(k==1,'100-year Floods','100-year Precipitation')
  foo=ggplot(data.frame(x=0,y=0))+theme_void()
  for (i in 1:upto){
    fname=file.path("anim_im",paste0('intro_var',k,sprintf("_im_%04d",i),".png"))
    if(!file.exists(fname)){
      alf=1-(i/upto)^3
      top=g0+geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf),alpha=alf,fill=bcolor)+
        geom_text(aes(x=0,y=0,label=lab),color=fcolor,size=90,alpha=alf)
      hide=foo+geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=alf,fill=bcolor)
      bottom=h+inset_element(hide,right=1,bottom=0,left=0,top=1,align_to='full')
      fig=top/bottom+plot_layout(heights=c(2.3,1))
      png(filename=fname,bg=bcolor,width=(1280*resfactor), height=(720*resfactor), res=72)
      print(fig)
      dev.off()
    }
  }
  # individual plots
  previous=NULL
  upto=ifelse(k==1,nIntro*0.5,nOutro)
  for(i in 1:(1200+upto)){ # 700:709# 1:1200
    fname=file.path("anim_im",paste0('var',k,sprintf("_im_%04d",i),".png"))
    if(!file.exists(fname)){
      DF=D %>% filter(monthFromStart==i) %>% left_join(sites,by='ID')
      g=g0
      for(s in seq(8,1,-1)){
        g=g+geom_point(data=DF,aes(lon,lat),size=s,alpha=0.02,shape=16,color=pcolor)
      }
      g=g+geom_point(data=DF,aes(lon,lat),size=2,alpha=0.6,shape=16,color=pcolor)
      
      floods=DF %>% filter(rp>100)
      floods$code=floods$rp/1000;floods$code[floods$code>1]=1
      floods$alpha=0.9
      floods$color=length(pal)
      floods$size=6+as.integer(20*floods$code)
      combo=rbind(floods,previous)
      if(NROW(combo)>0) g=g+geom_point(data=combo,aes(lon,lat),size=combo$size,alpha=combo$alpha,shape=16,color=pal[combo$color])
      
      h=h0+geom_area(data=avail %>% filter(monthFromStart<=i),aes(time,n),fill=pcolor,alpha=0.5,color=NA)+
        annotate('text',x=mean(avail$year),y=NROW(sites)/2,label=DF$year[1],size=180,color=acolor,alpha=0.1)
      if(i<=1200){
        fig=g/h+plot_layout(heights=c(2.3,1))
      } else {
        alf=((i-1200)/upto)^3
        top=g+geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf),alpha=alf,fill=bcolor)
        hide=foo+geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=alf,fill=bcolor)
        bottom=h+inset_element(hide,right=1,bottom=0,left=0,top=1,align_to='full')
        fig=top/bottom+plot_layout(heights=c(2.3,1))
      }
      
      png(filename=fname,bg=bcolor,width=(1280*resfactor), height=(720*resfactor), res=72)
      print(fig)
      dev.off()
      
      combo1=combo;combo1$size=combo1$size-2;combo1$color=combo1$color-1
      combo2=combo;combo2$alpha=combo2$alpha-0.2
      previous=rbind(combo1,combo2) %>% filter(size>0,alpha>0)
      previous=previous[!duplicated(previous),]
      previous$color[previous$color<1]=1
    }
  }
}

fps=(bpm*6)/60
anim_im=list.files("anim_im", full.names=TRUE)
intro=anim_im[grep('intro_var1',anim_im)]
Q=anim_im[grep('/var1',anim_im)]
inter=anim_im[grep('intro_var2',anim_im)]
P=anim_im[grep('/var2',anim_im)]
flist=c(intro,Q,inter,P)
av_encode_video(input=flist,audio='HEGS.mp3',output='HEGS.mp4',framerate=fps)
