library(capushe)
data(datacapushe)

dataDjump=Djump(datacapushe)
print(dataDjump)
summary(dataDjump)
plot(dataDjump,newwindow=FALSE)

# Djump(datacapushe[1:5,])


dataNan=datacapushe
dataNan[2,3]=NaN
Djump(dataNan)
dataNan[15,1]=NaN
Djump(dataNan)
dataNan[15,3]=NaN
Djump(dataNan)
dataNan[8:50,4]=rep(NaN,43)
# Djump(dataNan)

Djump(datacapushe,scoef=3)
# Djump(datacapushe,scoef="a")
# Djump(datacapushe,scoef=-1)
# Djump(datacapushe,scoef=Inf)

Djump(datacapushe,Careajump=0.3)
# Djump(datacapushe,Careajump="a")
# Djump(datacapushe,Careajump=-1)
# Djump(datacapushe,Careajump=1)
# Djump(datacapushe,Careajump=Inf)

Djump(datacapushe,Ctresh=200)
# Djump(datacapushe,Ctresh="a")
# Djump(datacapushe,Ctresh=250)
# Djump(datacapushe,Ctresh=1)
# Djump(datacapushe,Ctresh=Inf)

datacomplex=datacapushe
datacomplex[4,3]=-3
# Djump(datacomplex)
datacomplex[4,3]=0.001
# Djump(datacomplex)

datapen=datacapushe
datapen[6,]=datapen[5,]
datapen[6,4]=1
Djump(datapen)
datapen[6,4]=0.005
Djump(datapen)

datacontrast=datacapushe
datacontrast[6,4]=-3
Djump(datacontrast)
