source('funk.R')
bpm=120
resfactor=3
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

tcolor='#917d79'
  
pal=colorRampPalette(c(pcolor,fcolor))(10)
vars=c('Q','P')

for(k in 1:2){
  v=vars[k]
  load(paste0(v,'.RData'))
  
  D$time=D$year+(D$month-0.5)/12
  D$monthFromStart=(D$year-1916)*12+D$month
  
  g0=BFunk::getWorldMap(fill=mcolor)
  g0=g0+geom_point(data=sites,aes(lon,lat),size=1,alpha=0.6,shape=16,color=fcolor)+
    theme(plot.background=element_rect(fill=bcolor,color=NA),
          panel.background=element_rect(fill=bcolor,color=NA))
  png(filename=file.path(paste0('var',k,sprintf("_im_%04d",0),".png")),
      bg=bcolor,width=(1280*resfactor), height=(720*resfactor), res=72)
  print(g0)
  dev.off()
  
  avail=D %>% group_by(monthFromStart,year,month,time) %>% summarise(n=n())
  
  previous=NULL
  
  for(i in 1:1200){ # 700:709# 1:1200
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
      
      png(filename=fname,bg=bcolor,width=(1280*resfactor), height=(720*resfactor), res=72)
      print(g)
      dev.off()
      
      combo1=combo;combo1$size=combo1$size-4;combo1$color=combo1$color-1
      combo2=combo;combo2$alpha=combo2$alpha-0.3
      previous=rbind(combo1,combo2) %>% filter(size>0,alpha>0)
      previous=previous[!duplicated(previous),]
      previous$color[previous$color<1]=1
    }
  }
}

fps=(bpm*6)/60
anim_im=list.files("anim_im", full.names=TRUE)
anim_im_Q=c(rep(anim_im[1],16*6),anim_im[1:1200])
anim_im_P=c(rep(anim_im[1200],16*6),anim_im[1201:2400],rep(anim_im[2400],16*6))
# anim_im_P=c(rep(anim_im[1200],16*6),anim_im[1:1200],rep(anim_im[1200],16*6))
av_encode_video(input=c(anim_im_Q,anim_im_P),
                audio='HEGS.mp3',
                output='HEGS.mp4',
                framerate=fps)
