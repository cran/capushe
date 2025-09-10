library(capushe)
data(datacapushe)

AICcapushe(datacapushe,10)
BICcapushe(datacapushe,10)

# AICcapushe(datacapushe,-5)
# BICcapushe(datacapushe,-5)
# AICcapushe(datacapushe,pi)
# BICcapushe(datacapushe,pi)
# AICcapushe(datacapushe,Inf)
# BICcapushe(datacapushe,Inf)
# AICcapushe(datacapushe,"a")
# BICcapushe(datacapushe,"a")

dataNan=datacapushe
dataNan[2,3]=NaN
AICcapushe(dataNan,10)
BICcapushe(dataNan,10)
dataNan[15,1]=NaN
AICcapushe(dataNan,10)
BICcapushe(dataNan,10)
dataNan[15,3]=NaN
AICcapushe(dataNan,10)
BICcapushe(dataNan,10)
dataNan[8:50,4]=rep(NaN,43)
AICcapushe(dataNan,10)
BICcapushe(dataNan,10)
dataNan[1:50,4]=rep(NaN,50)
# AICcapushe(dataNan,10)
# BICcapushe(dataNan,10)



datacomplex=datacapushe
datacomplex[4,3]=-3
# AICcapushe(datacomplex,10)
# BICcapushe(datacomplex,10)
datacomplex[4,3]=0.001
AICcapushe(datacomplex,10)
BICcapushe(datacomplex,10)

datapen=datacapushe
datapen[6,]=datapen[5,]
datapen[6,4]=0.005
AICcapushe(datapen,10)
BICcapushe(datapen,10)

datacontrast=datacapushe
datacontrast[6,4]=-3
AICcapushe(datacontrast,10)
