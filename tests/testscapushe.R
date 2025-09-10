library(capushe)
data(datacapushe)
data(datavalidcapushe)
data(datapartialcapushe)

datacapushe2=capushe(datacapushe,10)
print(datacapushe2)
plot(datacapushe2,newwindow=FALSE,ask=FALSE)
summary(datacapushe2)

capushe(datacapushe,100)
# capushe(datacapushe,pct=10)
# capushe(datacapushe,pct=-0.5)
# capushe(datacapushe,pct="a")
# capushe(datacapushe,pct=0.9)


capushe(datacapushe,point=10)
# capushe(datacapushe,point=45)
# capushe(datacapushe,point=51)
# capushe(datacapushe,point=1.5)
# capushe(datacapushe,point="a")
# capushe(datacapushe,point=-3)

library(MASS)
capushe(datacapushe,psi.rlm=psi.bisquare)
capushe(datacapushe,psi.rlm="lm")
capushe(datacapushe,psi.rlm=1)
capushe(datacapushe,psi.rlm=psi.huber)
capushe(datacapushe,psi.rlm=psi.hampel)

capushe(datacapushe,scoef=3)
# capushe(datacapushe,scoef="a")
# capushe(datacapushe,scoef=-1)
# capushe(datacapushe,scoef=Inf)

# capushe(datacapushe,n=-5)
# capushe(datacapushe,n=pi)
# capushe(datacapushe,n=Inf)
# capushe(datacapushe,n="a")

capushe(datacapushe,Careajump=0.3)
# capushe(datacapushe,Careajump="a")
# capushe(datacapushe,Careajump=-1)
# capushe(datacapushe,Careajump=1)
# capushe(datacapushe,Careajump=Inf)

capushe(datacapushe,Ctresh=200)
# capushe(datacapushe,Ctresh="a")
# capushe(datacapushe,Ctresh=250)
# capushe(datacapushe,Ctresh=1)
# capushe(datacapushe,Ctresh=Inf)

plot(datacapushe2,newwindow=FALSE,ask=FALSE)
# plot(datacapushe2,newwindow=3)

# plot(datacapushe2,ask=FALSE)
# plot(datacapushe2,ask=3)
# plot(datacapushe2,ask="a")

# capushe(datacapushe[1:5,])


dataNan=datacapushe
dataNan[2,3]=NaN
capushe(dataNan)
dataNan[15,1]=NaN
capushe(dataNan)
dataNan[15,3]=NaN
capushe(dataNan)
dataNan[8:50,4]=rep(NaN,43)
# capushe(dataNan)

datacomplex=datacapushe
datacomplex[4,3]=-3
# capushe(datacomplex)
datacomplex[4,3]=0.001
# capushe(datacomplex)

datapen=datacapushe
datapen[6,]=datapen[5,]
datapen[6,4]=1
plot(capushe(datapen)@DDSE,newwindow=FALSE)
datapen[6,4]=0.005
plot(capushe(datapen)@DDSE,newwindow=FALSE)

capushepartial=capushe(datapartialcapushe)
validation(capushepartial,datavalidcapushe,newwindow=FALSE)

dataNan=datavalidcapushe
dataNan[2,3]=NaN
validation(capushepartial,dataNan,newwindow=FALSE)
dataNan[,3]=rep(NaN,3)
validation(capushepartial,dataNan,newwindow=FALSE)

datacontrast=datacapushe
datacontrast[6,4]=-3
capushe(datacontrast)




