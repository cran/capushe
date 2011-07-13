library(capushe)
data(datacapushe)
data(datavalidcapushe)
data(datapartialcapushe)

dataDDSE=DDSE(datacapushe)
print(dataDDSE)
summary(dataDDSE)
plot(dataDDSE)

DDSE(datacapushe,100)
DDSE(datacapushe,pct=10)
DDSE(datacapushe,pct=-0.5)
DDSE(datacapushe,pct="a")


DDSE(datacapushe,point=10)
DDSE(datacapushe,point=45)
DDSE(datacapushe,point=51)
DDSE(datacapushe,point=1.5)
DDSE(datacapushe,point="a")
DDSE(datacapushe,point=-3)

DDSE(datacapushe,psi.rlm=psi.bisquare)
DDSE(datacapushe,psi.rlm="lm")
DDSE(datacapushe,psi.rlm=1)
DDSE(datacapushe,psi.rlm=psi.huber)
DDSE(datacapushe,psi.rlm=psi.hampel)

DDSE(datacapushe,scoef=3)
DDSE(datacapushe,scoef="a")
DDSE(datacapushe,scoef=-1)
DDSE(datacapushe,scoef=Inf)

plot(dataDDSE,newwindow=FALSE)
plot(dataDDSE,newwindow=3)

DDSE(datacapushe[1:5,])


dataNan=datacapushe
dataNan[2,3]=NaN
DDSE(dataNan)
dataNan[15,1]=NaN
DDSE(dataNan)
dataNan[15,3]=NaN
DDSE(dataNan)
dataNan[8:50,4]=rep(NaN,43)
DDSE(dataNan)

datacomplex=datacapushe
datacomplex[4,3]=-3
DDSE(datacomplex)
datacomplex[4,3]=0.001
DDSE(datacomplex)

datapen=datacapushe
datapen[6,]=datapen[5,]
datapen[6,4]=1
plot(DDSE(datapen))
datapen[6,4]=0.005
plot(DDSE(datapen))

DDSEpartial=DDSE(datapartialcapushe)
validation(DDSEpartial,datavalidcapushe)

dataNan=datavalidcapushe
dataNan[2,3]=NaN
validation(DDSEpartial,dataNan)
dataNan[,3]=rep(NaN,3)
validation(DDSEpartial,dataNan)

datacontrast=datacapushe
datacontrast[6,4]=-3
DDSE(datacontrast)