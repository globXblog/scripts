library(ggplot2);library(dplyr)
library(patchwork);library(av)

resfactor=2 # Resolution factor (0.5 poor, 1 OKish, 2 good quality)
bpm=120 # Tempo

load('extents_model5.RData')
tps=(bpm/60)*4 # time step per second (here time step = 16th note i.e. 1/4 of a beat)

makeplot=function(){
  months=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  temps=unique(dat %>% select(year,month))
  nT=NROW(temps)
  vars=c('SWI','FWI','Q')
  colors=c('#0F1B07','#150101','#1E1F26')
  lcolors=c('#89DA59','#F34A4A','#D0E1F9')
  black='#080808'
  gray='#f5cc4d'
  thick=4
  normal=1.5
  thin=0.1
  persistence=12*5
  for(k in 1:12){
    message(k)
    gs=vector('list',length(vars))
    for(i in 1:length(vars)){
      DF=dat %>% filter(var==vars[i])
      DF=DF[1:k,]
      DF$month[DF$month==12]=12.5
      DF$month[DF$month==1]=0.5
      n=NROW(DF)
      DF$ix=(1:n)-n+persistence;DF$ix[DF$ix<0]=0;DF$ix=as.integer(DF$ix)
      g=ggplot()+
        geom_hline(yintercept=0,color=lcolors[i],linewidth=normal)
      # +
      #   geom_hline(yintercept=0,color=lcolors[i],linewidth=thick,alpha=0.2)
      if(NROW(DF)>1) {
        g=g+geom_path(data=DF,aes(month,extent,group=year),color=lcolors[i],linewidth=thin)+
          geom_path(data=DF,aes(month,extent,group=year,alpha=ix),linewidth=thick,color=lcolors[i])+
          geom_path(data=DF,aes(month,extent,group=year,linewidth=ix),color=lcolors[i])+
          scale_linewidth(range=c(thin,normal),guide=NULL)+
          scale_alpha(range=c(0,0.2),guide=NULL)
      }
      g=g+geom_point(data=DF[k,],aes(month,extent),fill=lcolors[i],size=4,shape=21)+
        coord_radial(start=9*pi/12,inner.radius=0.2,expand=FALSE)+
        scale_x_continuous(breaks=seq(2,12,2),labels=months[seq(2,12,2)],limits=c(0.5,12.5))+
        scale_y_continuous(breaks=c(0,100),limits=c(0,100))+
        annotate('text',x=11,y=0,label='0%  ',hjust=0.95,color=lcolors[i],size=6)+
        annotate('text',x=11,y=100,label=' 100%',hjust=0,color=lcolors[i],size=6)+
        # labs(title=vars[i])+
        theme_light()+
        theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank(),
              axis.ticks=element_blank(),panel.border = element_blank(),
              axis.title=element_blank(),axis.text.y=element_blank(),
              panel.background=element_rect(fill=colors[i]),
              axis.text=element_text(size=18,color=lcolors[i]),
              plot.title=element_text(size=36,color=lcolors[i],hjust=0.5),
              plot.background=element_rect(fill=black,color=black))
       # g
      gs[[i]]=g
    }
    tbig=36
    tlarge=20
    tmed=10
    tsmall=8
    alf=0.9
    yg=ggplot()+ylim(0,1)+theme_void()+
      annotate('text',0,1,label=DF$year[k],color=gray,size=tbig,vjust=1,alpha=alf)+
      theme(plot.background=element_rect(fill=black,color=black))
    rightg=ggplot()+coord_cartesian(ylim=c(0,1),xlim=c(0,1))+theme_void()+
      theme(plot.background=element_rect(fill=black,color=black))+
      annotate('text',0,0.7,label='SWI = Soil Wetness Index',color=lcolors[1],size=tmed,hjust=0,vjust=0,alpha=alf)+
      annotate('text',0,0.5,label='FWI = Fire Weather Index',color=lcolors[2],size=tmed,hjust=0,vjust=0,alpha=alf)+
      annotate('text',0,0.3,label='Q = River Streamflow',color=lcolors[3],size=tmed,hjust=0,vjust=0,alpha=alf)
    lab1=paste0('Curves show the spatial extent\n',
               '[in % of France surface] of\n',
               'drought and fire conditions, for\n',
               'each month of the period 1959-2099\n')
    lab2=paste0('Years between 1959 and 2023\n',
                'correspond to observations, \n',
                'years between 2024 and 2099 to\n',
                'simulations from a climate model.\n')
    leftg=ggplot()+coord_cartesian(ylim=c(0,1),xlim=c(0,1),expand=FALSE)+theme_void()+
      theme(plot.background=element_rect(fill=black,color=black))+
      annotate('text',0.05,0.95,label=lab1,color=gray,size=tsmall,hjust=0,vjust=1,alpha=alf)+
      annotate('text',0.05,0,label=lab2,color=gray,size=tsmall,hjust=0,vjust=0,alpha=alf)
    titleg=ggplot()+coord_cartesian(ylim=c(0,1),xlim=c(0,1),expand=FALSE)+theme_void()+
      theme(plot.background=element_rect(fill=black,color=black))+
      annotate('text',0,0.95,label='Droughts and Fire Weather in France, 1959-2099',color=gray,size=tlarge,hjust=0,vjust=1,alpha=alf)
    
    top=wrap_plots(leftg,gs[[1]],rightg,widths=c(1,1.25,1),nrow=1)
    bottom=wrap_plots(gs[[2]],yg,gs[[3]],widths=c(1,0.5,1),nrow=1)
    final=wrap_plots(titleg,top,bottom,ncol=1,heights=c(0.2,1,1))&
      theme(plot.background=element_rect(fill=black,color=black))
    print(final)
  }
}


av_capture_graphics(makeplot(),output='video.mp4',
                    audio='myScore.mp3',
                    res=72*resfactor, width=1280*resfactor,height=720*resfactor,
                    framerate=tps)
