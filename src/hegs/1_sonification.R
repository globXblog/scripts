source('funk.R')
bpm=120

wguitar=getGuitar(bpm)
wclave=getClave(bpm)
wbell=getBell(bpm)
foo=getX('Q',bpm,intro=4)
Qleft=foo$left;Qright=foo$right
foo=getX('P',bpm,intro=58)
Pleft=foo$left;Pright=foo$right

# Bass ---------------
load('/home/benjamin.renard/BEN/GitHub/sequenceR/instruments/bassStandup.RData')
ff=1;f=0.8;m=0.6;p=0.3;random_tim=0.02;random_vol=0.02
bass=bassStandup
tp4=1/(bpm/60)
tp16=tp4/4
tp2=2*tp4
tp1=4*tp4
t0=tp1*4
# Main theme 
not=c('E2', 'Gb1','E1', 'Gb1',  rep('Gb2',8),    'E2', 'Gb1','E1', 'Gb1',  rep('Gb2',8));nT=length(not)
vol=c(f,    m,    m,    m,      m,0,p,m,0,m,0,p, m,    m,    m,    m,      m,0,p,m,0,m,0,p)*rbeta(nT,1/random_vol,1) 
dur=c(2*tp1,2*tp1,2*tp1,1.5*tp1,rep(tp16,8),     2*tp1,2*tp1,2*tp1,1.5*tp1,rep(tp16,8))
fade=rep(Inf,length.out=nT)
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=tim;VOL=vol;NOT=not;FADE=fade
# Descending part 
not= c('A2',  'C3',  'G3',   'Ab2', 'D3',  'Gb3',  'G2',  'D3',  'Gb3',  'Gb2', 'D3',  'Gb3',  'F2',  'D3',  'F3',  'F2',  'D3',  'F3');nT=length(not)
vol= c(f,     p,     f,      f,     p,     f,      f,     p,     f,      f,     p,     f,      f,     p,     f,      f,     m,     f)*rbeta(nT,1/random_vol,1) 
dur= c(3*tp16,3*tp16,26*tp16,3*tp16,3*tp16,26*tp16,3*tp16,3*tp16,10*tp16,3*tp16,3*tp16,10*tp16,3*tp16,3*tp16,10*tp16,3*tp16,3*tp16,10*tp16)
fade=c(Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf,    Inf,   0,     Inf)
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
# Transition to Flamenco part
not= c('F2',  'F2',  'B2','F3','B2','F2',  'F2', 'B2', 'F3','F3','F2',  'F2',  'B2','F3','B2','F3','F3','B2','F3','F2','F2','F3','F3');nT=length(not)
vol= c(ff,    f,     p,   f,   f,   f,     f,     p,   f,   m,   ff,    f,     p,   f,   f,   f,   f,   p,   f,   p,   p,   f,  f)*rbeta(nT,1/random_vol,1) 
dur= c(3*tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16,tp16)
fade=rep(0,length.out=nT)
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
# Flamenco part 
not= rep(c('E1',  'E3',  'D3','E3','B2','E2','E3','D3','E3',  'D2','E2','B1','E1',  'E3',  'E3','G3','B2','E2','F3','D3','F3',  'D2','E2','B1'),3);nT=length(not)
dur= rep(c(3*tp16,2*tp16,tp16,tp16,tp16,tp16,tp16,tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,tp16,tp16,tp16,tp16,tp16,2*tp16,tp16,tp16,tp16),3)
vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
fade=rep(0,length.out=nT)
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
not= c('E1',  'E3',  'D3','E3','B2','E1',  'E3',  'D3','E3',  'E2',  'E2',  'E2',  'E2',  'A1', 'B1');nT=length(not)
dur= c(3*tp16,2*tp16,tp16,tp16,tp16,3*tp16,2*tp16,tp16,2*tp16,3*tp16,3*tp16,2*tp16,3*tp16,3*tp16,2*tp16)
vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
fade=rep(0,length.out=nT)
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
# Cuban part 
# not= c('C2',  'B2',  'A2',  'Eb2', 'G2',  'Gb2', 'E2',  'G2',  'A2',  'B2',  'A2',  'Eb2', 'E2',  'D3',  'C2');nT=length(not)
not= c('C2',  'E2',  'B1',  'Eb2', 'E2',  'G2',  'Db2', 'G2',  'A2',  'E2',  'B1',  'Eb2', 'E2',  'G2',  'D3',  'G2',  'Gb2', 'E2');nT=length(not)
dur= c(3*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,2*tp16,3*tp16,3*tp16,2*tp16)
vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
fade=rep(0,length.out=nT);fade[15]=Inf
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
not= c('C2',  'B2',  'A2',  'Eb2', 'E2',  'G2',  'Db2', 'A2',  'C2',  'E2',  'B1',  'Eb2', 'E2',  'B1',  'G2',  'A2',  'Db2');nT=length(not)
dur= c(3*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,5*tp16,3*tp16,2*tp16)
vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
fade=rep(0,length.out=nT)
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
# outro
not= c('Db2', 'Db2', 'Db2',  'Db2', 'Db2',  'C2',  'D3',  'G3',   'B1',  'Gb3');nT=length(not)
dur= c(3*tp16,3*tp16,10*tp16,6*tp16,10*tp16,3*tp16,3*tp16,10*tp16,6*tp16,10*tp16)
fade=c(0,     0,     0,      0,     0,      Inf,   0,     Inf,    Inf,   Inf)
vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
not= c('Bb1', 'A3',  'Bb2', 'Bb1',  'B2',   'B1');nT=length(not)
dur= c(6*tp16,10*tp16,6*tp16,10*tp16,16*tp16,16*tp16)
vol= rep(f,length.out=nT)*rbeta(nT,1/random_vol,1) 
fade=rep(0,length.out=nT)
foo=cumsum(c(t0,dur));tim=foo[1:length(dur)];t0=foo[length(foo)]
TIM=c(TIM,tim);VOL=c(VOL,vol);NOT=c(NOT,not);FADE=c(FADE,fade)
# repeat 
TIM=c(TIM,t0+TIM);VOL=c(VOL,VOL);NOT=c(NOT,NOT);FADE=c(FADE,FADE)
# randomize 
TIM=TIM+rnorm(length(TIM),sd=tp16*random_tim)
# avoid slight shift in bass instrument
TIM=TIM-0.05
# Play it 
wbass=play.instrument(bass,notes=NOT,time=TIM,volume=VOL,nmax=20*10^6,fadeout=FADE+0.2)
# play(wbass)


final=mix(list(wclave,wbell,wbass,wbass,wguitar,wguitar,Qleft,Qright,Pleft,Pright),
          volume=c(0.2,0.3,1,1,0.4,0.4,1,1,1,1),pan=c(-0.5,0.5,0,0,-0.8,0.8,-1,1,-1,1))

writeWave(final,'temp.wav')
system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav','HEGS.mp3'))
file.remove('temp.wav')

