library(capushe)
data(datacapushe)

dataDjump=Djump(datacapushe)
print(dataDjump)
summary(dataDjump)
plot(dataDjump)

Djump(datacapushe[1:5,])


dataNan=datacapushe
dataNan[2,3]=NaN
Djump(dataNan)
dataNan[15,1]=NaN
Djump(dataNan)
dataNan[15,3]=NaN
Djump(dataNan)
dataNan[8:50,4]=rep(NaN,43)
Djump(dataNan)

Djump(datacapushe,scoef=3)
Djump(datacapushe,scoef="a")
Djump(datacapushe,scoef=-1)
Djump(datacapushe,scoef=Inf)

datacomplex=datacapushe
datacomplex[4,3]=-3
Djump(datacomplex)
datacomplex[4,3]=0.001
Djump(datacomplex)

datapen=datacapushe
datapen[6,]=datapen[5,]
datapen[6,4]=1
Djump(datapen)
datapen[6,4]=0.005
Djump(datapen)

datacontrast=datacapushe
datacontrast[6,4]=-3
Djump(datacontrast)