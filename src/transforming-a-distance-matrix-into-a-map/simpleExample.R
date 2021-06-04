library(ggplot2);library(gridExtra) # Plotting
set.seed(66) #make results repeatable
# white space
g0=ggplot()+theme_void()

# CASE 1: 3 points A, B, C ------
# 2-D
DF=data.frame(point=c('A','B','C'),x=c(0,1,0.5),y=c(0,0,sqrt(0.75)))
g=ggplot(DF,aes(x,y,label=point))+geom_point(size=4)+geom_text(nudge_y=0.15)
g=g+annotate('rect',xmin=-0.3,xmax=1.3,ymin=-0.35,ymax=sqrt(0.75)+0.25,color='black',alpha=0)
g=g+annotate('text',x=1,y=-0.35,label='2-D world',color='gray',vjust=-0.5)
g1=g+theme_void()+coord_equal()

# 1-D
DF=data.frame(point=c('A','B','C???'),x=c(0,1,0.5),y=c(0,0,0))
g=ggplot(DF,aes(x,y,label=point))+geom_hline(yintercept=0)
g=g+geom_point(size=4)+geom_text(nudge_y=0.1)
g=g+theme_void()+coord_equal()+ylim(-0.5,0.5)+xlim(0,2)
g2=g+annotate('text',x=2,y=0,label='1-D world',color='gray',hjust=1,vjust=-0.5)

pdf('2Dto1D_1.pdf',width=8,height=2,useDingbats=F)
grid.arrange(grobs=list(g1,g0,g2),layout_matrix=matrix(c(1,1,2,3,3),nrow=1))
dev.off()

png('2Dto1D_1.png',width=200,height=50,units='mm',res=300)
grid.arrange(grobs=list(g1,g0,g2),layout_matrix=matrix(c(1,1,2,3,3),nrow=1))
dev.off()

# CASE 2: 2 points A, B + a bunch of random points ------
nrep=10;sdev=0.08
# 2-D
DF=data.frame(point=c('A','B',rep('',nrep)),
              x=c(0,1,0.5+rnorm(nrep,sd=sdev)),
              y=c(0,0,sqrt(0.75)+rnorm(nrep,sd=sdev)))
g=ggplot(DF,aes(x,y,label=point,color=point))+geom_point(size=4,alpha=0.5)+geom_text(nudge_y=0.15)
g=g+annotate('rect',xmin=-0.3,xmax=1.3,ymin=-0.35,ymax=sqrt(0.75)+0.25,color='black',alpha=0)
g=g+annotate('text',x=1,y=-0.35,label='2-D world',color='gray',vjust=-0.5)
g1=g+theme_void()+coord_equal()+theme(legend.position='none')

# 1-D
DF=data.frame(point=c('A','B',rep('',nrep)),
              x=c(0,1,0.5+rnorm(nrep,sd=sdev)),
              y=c(0,0,rep(0,nrep)))
g=ggplot(DF,aes(x,y,label=point,color=point))+geom_hline(yintercept=0)
g=g+geom_point(size=4,alpha=0.5)+geom_text(nudge_y=0.1)
g=g+theme_void()+coord_equal()+theme(legend.position='none')
g=g+ylim(-0.5,0.5)+xlim(0,2)
g2=g+annotate('text',x=2,y=0,label='1-D world',color='gray',hjust=1,vjust=-0.5)

DF=data.frame(point=c('A','B',rep('',nrep)),
              x=c(0,0.75,1.5+rnorm(nrep,sd=sdev)),
              y=c(0,0,rep(0,nrep)))
g=ggplot(DF,aes(x,y,label=point,color=point))+geom_hline(yintercept=0)
g=g+geom_point(size=4,alpha=0.5)+geom_text(nudge_y=0.1)
g=g+theme_void()+coord_equal()+theme(legend.position='none')
g=g+ylim(-0.5,0.5)+xlim(0,2.5)
g3=g+annotate('text',x=2.5,y=0,label='1-D world',color='gray',hjust=1,vjust=-0.5)

pdf('2Dto1D_2.pdf',width=12,height=2,useDingbats=F)
grid.arrange(grobs=list(g1,g0,g2,g0,g3),layout_matrix=matrix(c(1,1,2,3,3,4,5,5),nrow=1))
dev.off()

png('2Dto1D_2.png',width=300,height=50,units='mm',res=300)
grid.arrange(grobs=list(g1,g0,g2,g0,g3),layout_matrix=matrix(c(1,1,2,3,3,4,5,5),nrow=1))
dev.off()
