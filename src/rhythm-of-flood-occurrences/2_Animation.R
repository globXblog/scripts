library(ggplot2);library(gridExtra)
bkgCol='#333652'
txtCol='#e9eaec'
barCol='#fad02c'
lineCol='#90adc6'
qCol='#F62A00'

dat=read.table('Q.txt',header=T)
Q=dat$Q;Q10=dat$threshold[1]

getBackground <- function(title='This is the rhythm of...',
                          textsize=14,xmax=60,xbin=4,ymax=40,
                          bkgcol=bkgCol,txtcol=txtCol){
  g=ggplot()+theme_bw()+coord_cartesian(ylim=c(0,ymax),expand=F)
  g=g+labs(x='Time since last event [years]',y='Count',title=title,subtitle=' ')
  g=g+scale_x_continuous(breaks=seq(2,xmax,xbin),limits=c(0,xmax))
  g=g+theme(axis.title=element_text(size=textsize*2,color=txtcol),
            axis.text=element_text(size=textsize,color=txtcol),
            plot.title=element_text(size=textsize*3,color=txtcol),
            plot.subtitle=element_text(size=textsize*2,color=txtcol),
            panel.grid=element_blank(),
            legend.position='none',
            panel.background=element_rect(fill=bkgcol),
            plot.background=element_rect(fill=bkgcol))
  return(g)
}

getSubPlot <- function(DF=data.frame(x=1:100,y=rnorm(100)),threshold=qnorm(0.9),
                       k=50,lcol=lineCol,tcol=qCol,labcol=txtCol,textsize=6,alpha=0.1){
  foo=cbind(DF,t=threshold)
  xmax=max(DF$x)
  g=ggplot(foo)+geom_line(aes(x,y),color=lcol,alpha=alpha,size=0.25)+
    geom_line(aes(x,t),color=tcol,alpha=alpha,size=1)+
    geom_line(data=foo[1:k,],aes(x,y),color=lcol,size=0.25)+
    geom_line(data=foo[1:k,],aes(x,t),color=tcol,size=1)+
    coord_cartesian(xlim=c(-0.08*xmax,xmax))+
    #labs(title='Flood time series')+
    annotate('text',x=0,y=0.95*max(DF$y),color=labcol,label='  Flood time series',size=textsize)+
    annotate('text',x=0,y=threshold,color=tcol,hjust=1,label='10-year  \n  flood  ',size=textsize-1)+
    theme_void()+theme(plot.background = element_rect(fill=NA,color='grey20'),
          plot.title=element_text(size=textsize,color=labcol))
  return(g)
}

getMainPlot <- function(g0=getBackground(),DF=data.frame(x=seq(1,60,4),y=0.1),currentWait=-1,
                        barcol=barCol,wcol=lineCol,subtitle='...10-year floods'){
  g=g0+geom_col(data=DF,aes(x,y),fill=barcol,orientation='x')
  g=g+labs(subtitle=subtitle)
  g=g+geom_segment(data=data.frame(x=0,xend=currentWait,y=0,yend=0),
                   aes(x=x,y=y,xend=xend,yend=yend),
                   alpha=0.7,size=10,lineend='round',color=wcol)
  return(g)
}

animationPlots <- function(Q,Q10,intro_nstep=4*10,nRefresh=8*10+1,nOutro=12*10,xbin=4,eps=0.1){
  occ=as.numeric(Q>=Q10) # flood occurrences
  wait=diff(which(occ==1))
  # Compute bins
  bounds=seq(1,max(wait)+xbin,xbin)-0.5
  centers=bounds+xbin/2
  xmax=max(centers)+xbin/2+1
  ymax=max(hist(wait,breaks=bounds,plot=F)$counts)+1
  # initialize data frame, get background plot
  g0=getBackground(xmax=xmax,xbin=xbin,ymax=ymax)
  DF=data.frame(x=centers,y=eps)
  boxDF=data.frame(x=1:(length(Q)-nRefresh+1),y=Q[nRefresh:length(Q)])
  # Intro
  for(i in 1:intro_nstep){
    message(paste0('Intro: ',i,' / ',intro_nstep))
    print(getMainPlot(g0,DF,subtitle = ' '))
  }
  # Occurrences
  current=0;k=0
  for(i in 1:length(occ)){
    # for(i in 1:80){
    message(paste0('Main: ',i,' / ',length(occ)))
    if(i==nRefresh){DF$y=eps}
    if(i<nRefresh){
      tit='... events occurring every 10 years'
    } else {
      tit='...10-year floods'
    }
    if(occ[i]==0){
      current=current+1
    } else {
      currentSave=current
      current=0
      if(i>1){
        k=k+1
        j=which.min(abs(wait[k]-centers))
        DF$y[j]=DF$y[j]+1
      }
    }
    g=getMainPlot(g0,DF,subtitle=tit,currentWait=current)
    if(i>=nRefresh){ # Add Q time series
      box=getSubPlot(boxDF,Q10,i-nRefresh+1)
      g=g+annotation_custom(ggplotGrob(box),0.5*xmax,0.95*xmax,0.5*ymax,0.95*ymax)
    }
    print(g)
  }
  for(i in 1:nOutro){
    message(paste0('Outro: ',i,' / ',nOutro))
    print(g)
  }
}

resfactor=2 # resolution factor. 1 is OKish, 2 recommended for good quality, but longer to run
# Create video
doV2=TRUE
if(doV2){
  bpm=115 # tempo in beats per minute
  audio='HydrologyBasics2_v2.wav'
  output='Q10_v2.mp4'
  nRefresh=16*10+1
  nOutro=36*10
} else{
  bpm=120
  audio='Q10.mp3'
  output='Q10.mp4'
  nRefresh=8*10+1
  nOutro=12*10
}
fps=bpm*4/60
av::av_capture_graphics(animationPlots(Q,Q10,nRefresh=nRefresh,nOutro=nOutro),
                        output=output,audio=audio,
                        res=72*resfactor,width=1280*resfactor,height=720*resfactor,framerate=fps)
 
