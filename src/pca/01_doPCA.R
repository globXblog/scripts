# Read matrix of monthly discharges, 552 time steps * 207 stations
QM=read.table('QM.txt',header=F)
# Get normal scores
QM.ns=QM
for (i in 1:ncol(QM)){
  r=rank(QM[,i],na.last='keep',ties.method='random')
  p=(r-0.5)/sum(!is.na(r))
  QM.ns[,i]=qnorm(p)
}
# Perform PCA
QM.noMV=QM.ns
for (i in 1:ncol(QM)){
  QM.noMV[is.na(QM.ns[,i]),i]=0 # replace missing data by 0 in normal-score space
}
w=prcomp(x=QM.noMV,center=T,scale=F)
# Save components and effects
PC=data.frame(w$x)
write.table(PC,file='Components.txt',row.names = FALSE)
Effects=data.frame(w$rotation)
write.table(Effects,file='Effects.txt',row.names = FALSE)
