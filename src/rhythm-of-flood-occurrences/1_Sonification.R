library(sequenceR);library(evd)
bpm=120 # tempo in beats per minute
spt=(60/bpm)/4 # seconds per time step (here time step = 16th note i.e. 1/4 of a beat)
set.seed(54321)
RP=10 # return period
nregular=8*RP # number of time steps for regular event
nflood=96*RP # number of time steps for random flood occurrences
intro_nstep=4*RP # guitar-only intro

# Generate dataset
Q10=qgumbel(0.9) # 10-year flood
regular=rep_len(c(Q10,rep(0,RP-1)),nregular) # event occurring every 10 year exactly
flood=c(Q10,rgumbel(nflood-1)) # flood values
Q=c(regular,flood)
write.table(data.frame(Q=Q,threshold=Q10),file='Q.txt',row.names=F)
occ=as.numeric(Q>=Q10) # flood occurrences
tocc=which(occ==1) # occurrence times
wait=c(RP,diff(tocc)) # waiting time between occurrences
# Cumulated waiting time: advance by 1 at each step and goes back to 0 at each occurrence
cwait=0*occ
for(i in 1:length(occ)){cwait[i]=ifelse(occ[i]|i==1,0,cwait[i-1]+1)}
cwait=c(rep(0,intro_nstep),cwait) # Add 0's for guitar intro

# Load instruments
w=readMP3(file.path('samples','guitar1.mp3'))
guitar1=soundSample(0.5*(w@left+w@right))
w=readMP3(file.path('samples','guitar2.mp3'))
guitar2=soundSample(0.5*(w@left+w@right))
w=readMP3(file.path('samples','organ1.mp3'))
organ1=soundSample(0.5*(w@left+w@right))
w=readMP3(file.path('samples','organ2.mp3'))
organ2=soundSample(0.5*(w@left+w@right))
# Instrument: bass
f=list.files(file.path('samples','bass'),'.mp3',full.names=TRUE)
samlist=vector(mode='list',length=length(f))
for(i in 1:length(f)){
  w=readMP3(f[[i]])
  samlist[[i]]=soundSample(0.5*(w@left+w@right))
}
bass=instrument(samlist)

# Drums
n=length(occ)
t0=(intro_nstep+1)*spt-which(guitar1$wave!=0)[1]/guitar1$rate
tim=t0+((1:n)-1)*spt
# hi-hat
m=0.6;p=0.08 
hhVol=rep_len(c(m,0,p,0,p,0,p,0,p,0),n)
hh=sequence(hiHat,time=tim,volume=hhVol)
# ride
riVol=rep_len(c(1,0,1,1,0,1,0,1,1,0),n)
ri=sequence(ride,time=tim,volume=riVol)
# snare
f=1;mf=0.75;m=0.5;mp=0.2;p=0.1;pp=0.03
snVol=rep_len(c(0,p,0,f,p,0,p,mf,pp,p,0,mp,pp,pp,f,pp,0,p,mf,p),n)
sn=sequence(snare,time=tim,volume=snVol)
# kick
ki=sequence(kick,time=tim,volume=occ*rescale(Q))
drum=mix(list(hh,ri,sn,ki),volume=c(0.5,0.3,1,1),pan=c(-0.7,0.7,0,0))

n=length(cwait)
tim=c(0,((1:n)+1)*spt/guitar1$duration,1)
# guitar 1
g1=sequence(guitar1,0)
# organ 1 - start playing when cwait > 10 and increase volume until cwait= 20
v=as.numeric(cwait>=10)*(cwait-10)/(20-10);v[v>1]=1
env=envelope(t=tim,v=c(0,v,0))
o1=sequence(applyEnvelope(organ1,env),0)
# guitar2 - same starting at 20 until 30
v=as.numeric(cwait>=20)*(cwait-20)/(30-20);v[v>1]=1
env=envelope(t=tim,v=c(0,v,0))
g2=sequence(applyEnvelope(guitar2,env),0)
# organ 2 - same above 30
v=as.numeric(cwait>=30)*(cwait-30)/(40-30);v[v>1]=1
env=envelope(t=tim,v=c(0,v,0))
o2=sequence(applyEnvelope(organ2,env),0)

# bass: short waits are high-pitched
notes=NA*wait
notes[wait==1]=length(bass)
notes[wait==2]=length(bass)-1
notes[wait==3]=length(bass)-2
notes[wait==4]=length(bass)-3
notes[wait==5]=length(bass)-4
notes[wait==6]=length(bass)-5
notes[wait>6 & wait<=8]=length(bass)-6
notes[wait>8 & wait<=12 ]=length(bass)-7
notes[wait>12 & wait<=20 ]=length(bass)-8
notes[wait>20 & wait<=32 ]=length(bass)-9
notes[wait>32]=length(bass)-10
notes=as.integer(notes)
ba=play.instrument(bass,notes=notes,time=t0+(tocc-1)*spt,
                   volume=rescale(Q[occ==1],low=0.2)) # proportional to flood value
                   #volume=rescale(-1*notes,0.5,1))

# Save as MP3
final=mix(list(drum,g1,o1,g2,o2,ba),pan=c(0,0,-1,1,0,0),volume=c(1,0.25,0.3,0.25,0.5,0.6))
writeWave(final,'temp.wav')
system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav Q10.mp3'))
file.remove('temp.wav' )

