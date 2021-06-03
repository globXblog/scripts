library(tuneR) # used to generate the sound

#***************************************************************
# 1. Define series size ----
nYear=40 # Number of years in record
n=nYear*365.25*24 # Number of hourly time steps
timeSteps=(1:n)/(365.25*24) # Time steps [expressed in years]

#***************************************************************
# 2. Build time series (think of it as hourly temperatures) ----
# 2.1 Long-term increasing trend starting at 15 C
trend=15+0.2*timeSteps
# 2.2 Annual signal
freq=1 # 1 cycle per year
amplitude=8 # => 16 C difference between winter and summer 
annual=amplitude*sin(2*pi*timeSteps*freq) # Create annual cycle
# 2.3 'Diurnal' signal
freq=300+65.25*exp(-0.1*timeSteps) # starts at 365.25 cycles per year and exponentially decreases to 300
amplitude=5 # => 10 C difference between night and day 
diurnal=amplitude*sin(2*pi*timeSteps*freq) # Create 'diurnal' cycle
# 2.4 Random component
n1=sum(timeSteps<=nYear/2);n2=n-n1 # first and second half of the series
ro1=0.98;ro2=0.95 # lag-1 autocorrelation of each half
e1=sqrt(1-ro1^2)*arima.sim(model=list(ar=ro1),n=n1) # Generate noise for first half 
e2=sqrt(1-ro2^2)*arima.sim(model=list(ar=ro2),n=n2) # Generate noise for second half 
e=c(e1,e2) # paste them together
# 2.5 Assemble full series
y=trend+annual+diurnal+as.numeric(e)

#***************************************************************
# 3. Plot time series ----
jpeg(filename='hourlySeries.jpg',width=1000,height=400)
par(mar=c(5,5,2,2))
plot(timeSteps,y,type='l',cex.axis=1.5,cex.lab=1.8,
     xlab='Time [Year]',ylab='Hourly Values',col='cadetblue4')
dev.off()

#***************************************************************
# 4. Generate sound with package tuneR  ----
z=(y-mean(y))/sd(y) # center and scale
w0=Wave(left=z,right=z) # create waveform (stereo)
w=normalize(w0,unit = "16") # normalize waveform values to make it audible
writeWave(w,filename='hourlySeries.wav') # Write to file

