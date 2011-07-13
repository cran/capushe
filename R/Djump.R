setClass(
	Class="Djump",
        representation=representation(
	model="character",
	ModelHat="list",
	graph="list")
)

setMethod("print","Djump",
	function(x,...){
		print(x@model)
	}
)

setMethod("show","Djump",
	function(object){
		print(object@model)
	}
)

setMethod("summary","Djump",
function(object,checking=FALSE,checkgraph=FALSE){
	cat("\nSelected model: ",as.character(object@model),"\n",sep="")

	cat("\nEstimated penalty constant: ",as.character(signif(object@ModelHat$kappa[object@ModelHat$JumpMax],3)),"\n",sep="")
	cat("Selected model complexity: ",as.character(object@graph$complexity[object@graph$Modopt]),"\n\n",sep="")

	if (checking){
		result=list(object@model,object@ModelHat)
		names(result)=c("model","ModelHat")
		if (checkgraph==TRUE){
			Tempo=list(object@graph)
			names(Tempo)=c("graph")
			result=c(result,Tempo)
			}
		result
		}
	}
)

Djump = function(data,scoef=2){

if (is.character(data)){data=read.table(data)}

if (length(data[1,])!=4){
	stop("data must have 4 columns with name, penshape, complexity and contrast")
	}
data=data.frame(data)
names(data)=c("model","pen","complexity","contrast")

if(any(is.na(data))){
	bad=which(is.na(data),arr.ind=TRUE)[,1]
	repbad=which(duplicated(bad),arr.ind=TRUE)
	if (length(repbad)!=0){bad=bad[-repbad]}
	data=data[-bad,]
	if (length(bad)==1){
		warning(paste("1 line has been removed by Djump"))
		}
		else{warning(paste(as.character(length(bad)),"lines have been removed by Djump"))}
	}
leng=length(data$model)
if (leng<=10){
	stop("At least 10 observations are needed")
	}

if (!is.numeric(data[,2])){
	stop("pen must be numeric")
	}
if (!is.numeric(data[,3])){
	stop("complexity must be numeric")
	}
if (!is.numeric(data[,4])){
	stop("contrast must be numeric")
	}

if ((length(data$pen)!=leng)|(length(data$complexity)!=leng)|(length(data$contrast)!=leng)){
	stop("The lengths of the columns are not equal")
	}
if (!prod(data$complexity>0)){
	stop("Complexity must be positive")
	}
data=data[order(data$pen,data$contrast),]
Tempoligne=data$pen[order(data$complexity)]
if (!prod((Tempoligne[2:leng]-Tempoligne[1:(leng-1)])>=0)){
	stop("Penshape should be an increasing function of the complexity")
	}
if(!(is.numeric(scoef))){
	stop("scoef must be numeric")
	}
if (scoef<1){
	stop("scoef must be greater than one")
	}
if(scoef==Inf){
	stop("scoef must be different of Inf")
	}

kappa=c(0)
m=which.min(data$contrast)
mhat=c(m)
G=which((data$contrast>data$contrast[m])&(data$pen<data$pen[m])==TRUE)

while (length(G)>0){
	Ktempo=(data$contrast[m]-data$contrast[G])/(data$pen[G]-data$pen[m])
	Min=min(Ktempo)
	m=which.min(Ktempo)
	kappa=c(kappa,Min)
	mhat=c(mhat,m)
	G=which((data$contrast>data$contrast[m])&(data$pen<data$pen[m])==TRUE)
	}


mleng=length(mhat)
jump=data$complexity[mhat[1:(mleng-1)]]-data$complexity[mhat[2:mleng]]
maxjump=which(jump==max(jump))
if (length(maxjump)>1){
	warning("There are several maximum jump")
	}
JumpMax=max(maxjump)



Kopt=scoef*kappa[JumpMax+1]
test=which(((kappa[2:mleng]>Kopt)*(kappa[1:(mleng-1)]<=Kopt))==TRUE)

if (length(test)==0){
	Modelopt=mhat[mleng]
	}else{
	Modelopt=mhat[test]
}



check=list(jump,kappa,mhat,JumpMax,Kopt)
names(check)=c("jump","kappa","model_hat","JumpMax","Kopt")
graph=list(data$pen,data$complexity,data$model,Modelopt)
names(graph)=c("pen","complexity","model","Modopt")

new(Class="Djump",model=as.character(data$model[Modelopt]),ModelHat=check,graph=graph)

}



setMethod(
	f="plot",
	signature="Djump",
	definition=function(x,y,newwindow=TRUE,...){

	leng=length(x@graph$model)
	mleng=length(x@ModelHat$model_hat)
	scoef=x@ModelHat$Kopt/x@ModelHat$kappa[x@ModelHat$JumpMax+1]

	if (newwindow){	x11(width=15)}

	par(oma=c(0,3,0,0),xaxs="i")
	Absmax=(scoef+1)/scoef*x@ModelHat$Kopt
	Ordmin=max(which((x@ModelHat$kappa<=Absmax)==TRUE))
	plot(x=x@ModelHat$Kopt,y=x@graph$complexity[x@graph$Modopt],xlim=c(0,Absmax),ylim=c(x@graph$complexity[x@ModelHat$model_hat[Ordmin]],x@graph$complexity[x@ModelHat$model_hat[1]]),ylab="",xlab=expression(paste("Values of the penalty constant ",kappa)),yaxt="n",pch=4,col="blue",main="Dimension Jump",lwd=3,xaxt = "n")
	complex=x@graph$complexity[x@ModelHat$model_hat[1:Ordmin]]
	for (i in 1:(mleng-1)){
		lines(x=c(x@ModelHat$kappa[i],x@ModelHat$kappa[i+1]),y=c(complex[i],complex[i]))
		}
	if (x@ModelHat$kappa[i+1]<Absmax){
		lines(x=c(x@ModelHat$kappa[mleng],Absmax),y=c(complex[mleng],complex[mleng]))
		}

	lines(x=c(x@ModelHat$Kopt/scoef,x@ModelHat$Kopt/scoef),y=c(complex[x@ModelHat$JumpMax],complex[x@ModelHat$JumpMax+1]),col="blue",lty=2)
	ordon=paste(as.character(complex),"(",as.character(x@graph$model[x@ModelHat$model_hat[1:Ordmin]]),")")
	par(cex.axis=0.6)

	pas=(max(complex)-min(complex))/39*5.6/par("din")[2]
	leng2=length(complex)
	Coord=c()
	i=leng2
	j=leng2-1
	while (j>0){
		while ((j>0)*((complex[j]-complex[i])<pas)){
			Coord=c(Coord,-j)
			j=j-1		
			}
		i=j
		j=j-1
		}
	if (length(Coord)>0){
		complex=complex[Coord]
		ordon=ordon[Coord]
		}
	axis(2,complex,ordon,las=2)

	par(cex.axis=1)
	axis(2,labels="Selected Complexity (Model)",outer=TRUE,at=(x@graph$complexity[leng]+x@graph$complexity[x@ModelHat$model_hat[Ordmin]])/2,lty=0)
	if (scoef==1){
	  axis(1,labels=  expression(kappa[opt] == hat(kappa)^{dj}),at=x@ModelHat$Kopt)
	  }
	  else {  
      axis(1,labels=expression(hat(kappa)^{dj}),at=x@ModelHat$Kopt/scoef)
	     axis(1,labels=  expression(kappa[opt]),at=x@ModelHat$Kopt)
	     }
	
   

	if (scoef<=2){
	absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef/4)[c(-5)]
	Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/3*12.093749/par("din")[1]*scoef),which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/3*12.093749/par("din")[1]*scoef))
	 }
  if ((2<scoef)*(scoef<4)){
        absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef/3)[c(-4)]
	Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/5*12.093749/par("din")[1]*scoef),which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/5*12.093749/par("din")[1]*scoef))
        }
  if ((4<=scoef)*(scoef<=6)){
          absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef/2)[c(-3)]
	Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/26*12.093749/par("din")[1]^(1/3)*scoef),which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/26*12.093749/par("din")[1]*scoef))
          }
  if (scoef>6){
      absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef)[c(-2)]
	Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/35*12.093749/par("din")[1]*scoef),which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/35*12.093749/par("din")[1]*scoef))
          }
  if (length(Empty)>0){
    absci=absci[-Empty]
    }       
	axis(1,absci,signif(absci,2))

	legend("topright",legend="Maximal Jump",lty=2,col="blue")
	}
)


