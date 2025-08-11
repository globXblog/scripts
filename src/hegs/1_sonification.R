source('funk.R')
bpm=120

wguitar=getGuitar(bpm)
wguitar_v2=getGuitar_v2(bpm)
wbass=getBass(bpm)
wbass_v2=getBass_v2(bpm)
wkick=getDrumKick(bpm)
whihat=getHiHat(bpm)
wride=getRide(bpm)
wclave=getClave(bpm)
wbell=getBell(bpm)
wconga=getConga(bpm)
foo=getX('Q',bpm,intro=4)
Qleft=foo$left;Qright=foo$right
foo=getX('P',bpm,intro=58)
Pleft=foo$left;Pright=foo$right
foo=getX_v2('P',bpm,intro=58)
Pleft_v2=foo$left;Pright_v2=foo$right

noGuitar=TRUE
v2=TRUE
if(noGuitar){vg=0} else {vg=0.4}
if(v2){wguitar=wguitar_v2;wbass=wbass_v2;Pleft=Pleft_v2;Pright=Pright_v2}
final=mix(list(wclave,wbell,wconga,wkick,whihat,wride,wbass,wguitar,wguitar,Qleft,Qright,Pleft,Pright),
          volume=c(0.15,0.2,0.5,1,0.3,0.3,1,vg,vg,0.8,0.8,0.8,0.8),pan=c(-0.5,0.5,-0.3,0,0.3,-0.3,0,-0.8,0.8,-1,1,-1,1))

writeWave(final,'temp.wav')
fname=paste0('HEGS',ifelse(noGuitar,'_noGuitar',''),ifelse(v2,'_v2',''),'.mp3')
system(paste('cd',getwd(),'&& ffmpeg -y -i temp.wav',fname))
file.remove('temp.wav')
