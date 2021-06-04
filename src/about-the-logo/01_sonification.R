library(sequenceR);library(dplyr)

# 1. Read data -------------------------------------
DF0=read.table('logoData.csv',sep=';',header=T)
DF=summarize(group_by(DF0,year,month),n=sum(nb))
# read sounds
nS=9;k=0
sound=vector(mode='list',length=nS)
w=readMP3(file.path('samples','guitar_G2_very-long_piano_normal.mp3'))
k=k+1;sound[[k]]=soundSample(w@left)
w=readMP3(file.path('samples','guitar_C3_very-long_piano_normal.mp3'))
k=k+1;sound[[k]]=soundSample(w@left)
w=readMP3(file.path('samples','guitar_G3_very-long_piano_normal.mp3'))
k=k+1;sound[[k]]=soundSample(w@left)
w=readMP3(file.path('samples','guitar_B3_very-long_piano_normal.mp3'))
k=k+1;sound[[k]]=soundSample(w@left)
w=readMP3(file.path('samples','guitar_G4_very-long_forte_harmonics.mp3'))
k=k+1;sound[[k]]=soundSample(w@left)
w=readMP3(file.path('samples','guitar_B4_very-long_forte_harmonics.mp3'))
k=k+1;sound[[k]]=soundSample(w@left)
w=readMP3(file.path('samples','splash.mp3'))
k=k+1;sound[[k]]=soundSample(w@left)
k=k+1;sound[[k]]=kick
k=k+1;sound[[k]]=hiHat
vmix=c(rep(1,4),0.7,0.7,0.5,1,0.7)

# 2. Sequencing -------------------------------------
# Define sequencing time
# Times are slightly randomized except first and last notes
rand=c(0,runif(NROW(DF)-2,-0.25,0.25),0)
tim=((1:NROW(DF))-1+rand)/(12*1.5)
# Define sequencing volume
# use param=1 for an affine mapping between n and volume
param=1;minVol=0.02
vol=rescale(qbeta(rescale(DF$n),1/param,1*param),low=minVol)
plot(vol,type='l')
# Define sequencing panoramic
pan=sin(2*pi*((1:NROW(DF))-1)/12-pi/2)
# randomly choose instrument to be sequenced
inote=sample.int(nS,NROW(DF),replace=T)

# 2. Create sound -------------------------------------
allVoices=vector(mode='list',length=nS)
for(i in 1:nS){
  m= inote==i
  m[length(m)]=TRUE # all instruments are played for last data
  allVoices[[i]]=sequence(sound[[i]],time=tim[m],pan=pan[m],volume=vol[m])
}
s=mix(allVoices,volume=vmix[1:nS])
writeWave(s,'logo.wav')
