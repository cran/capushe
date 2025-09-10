##' Model selection by dimension jump
##'
##' @name Djump
##'
##' @aliases Djump
##' @aliases Djump-class
##' @aliases print,Djump-method
##' @aliases show,Djump-method
##' @aliases summary,Djump-method
##' @aliases print.Djump
##' @aliases show.Djump
##' @aliases summary.Djump
##' @aliases djump
##' @aliases DJUMP
##' @aliases Dimensionjump
##' @aliases DimensionJump
##' @aliases Dimension_Jump
##' @aliases Dimension_jump
##'
##' @usage Djump(data,scoef=2,Careajump=0,Ctresh=0)
##'
##' @details
##'
##' \code{Djump} is a model selection function based on the slope heuristics.
##'
##' @slot model character. The \code{model} selected by the dimension jump method.
##' @slot ModelHat list. A list describing the algorithm.
##' \itemize{
##'  \item \code{jump} The vector of jump heights.
##'  \item \code{kappa} The vector of the values of \eqn{\kappa} at each jump.
##'  \item \code{model_hat} The vector of the selected models \eqn{m(\kappa)} by the jump.
##'  \item \code{JumpMax} The location of the greatest jump.
##'  \item \code{Kopt} \eqn{\kappa_{opt}=scoef\hat{\kappa}}.
##'  }
##'
##' @slot graph list.
##' @slot Area list.
##'
##' @details
##' The Djump algorithm proceeds in three steps:
##' \enumerate{
##'   \item{For all \eqn{\kappa>0}, compute
##'     \eqn{m(\kappa)\in argmin_{m\in M} \{\gamma_n(\hat{s}_m)+\kappa\times pen_{shape}(m)\}}
##'     This gives a decreasing step function \eqn{\kappa \mapsto C_{m(\kappa)}}.}
##'   \item{Find \eqn{\hat{\kappa}} such that \eqn{C_{m(\hat{\kappa})}} corresponds to the
##'     greatest jump of complexity if \eqn{C_{tresh}=0} else \eqn{\hat{\kappa}} such that
##'     \eqn{\hat{\kappa}=inf\{\kappa>0: C_{m(\kappa)}\leq C_{tresh}\}.}
##'   }
##'   \item{Select \eqn{\hat{m}=m(scoef\times\hat{\kappa})} (output \code{@model}).}
##' }
##' Arlot has proposed a jump area containing the maximal jump defined by :
##'   \eqn{[\kappa(1-Careajump);\kappa(1+Careajump)].}
##' If \eqn{Careajump>0}, \code{Djump} return the area with the greatest jump. In practice,
##' it is advisable to take \eqn{Careajump=\frac{log(n)}{n}} where \eqn{n} is the number of observations.
##'
##' @returns
##' \item{@model}{The \code{model} selected by the dimension jump method.}
##' \item{@ModelHat}{A list describing the algorithm.}
##' \item{@ModelHat$jump}{The vector of jump heights.}
##' \item{@ModelHat$kappa}{The vector of the values of \eqn{\kappa} at each jump.}
##' \item{@ModelHat$model_hat}{The vector of the selected models \eqn{m(\kappa)} by the jump.}
##' \item{@ModelHat$JumpMax}{The location of the greatest jump.}
##' \item{@ModelHat$Kopt}{ \eqn{\kappa_{opt}=scoef\hat{\kappa}}.}
##' \item{@graph}{A list computed for the \code{\link[=plot.Djump]{plot}} method.}
##'
##' @export
##'
##' @references
##' Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
##' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
##'
##' @author Vincent Brault
##'
##' @seealso \code{\link[=capushe]{capushe}} for a model selection function including \code{\link[=AICcapushe]{AIC}},
##' \code{\link[=BICcapushe]{BIC}}, the \code{\link[=DDSE]{DDSE}} algorithm and the \code{Djump} algorithm.
##' \code{\link[=plot.Djump]{plot}} for a graphical display of the \code{DDSE}
##' algorithm and the \code{Djump} algorithm.
##'
##' @examples data(datacapushe)
##' Djump(datacapushe)
##' res <- Djump(datacapushe)
##' plot(res,newwindow=FALSE)
##' res <- Djump(datacapushe,Careajump=sqrt(log(1000)/1000))
##' plot(res,newwindow=FALSE)
##' res <- Djump(datacapushe,Ctresh=1000/log(1000))
##' plot(res,newwindow=FALSE)

setClass(
  Class="Djump",
  representation=representation(
    model="character",
    ModelHat="list",
    graph="list",
    Area="list")
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

            cat("\nEstimated penalty constant: ",as.character(signif(object@ModelHat$kappa[object@ModelHat$JumpMax+1],3)),"\n",sep="")
            cat("Selected model complexity: ",as.character(object@graph$complexity[object@graph$Modopt]),"\n\n",sep="")

            if (length(object@Area)==4){
              cat("Jump area: [",as.character(signif(object@Area$kappainf,3)),";",as.character(signif(object@Area$kappasup,3)),"]\n",sep="")
              if (length(object@Area$Modeloptarea)>1){
                cat("Models selected by jump area: ",sep="")
                cat(cat(as.character(object@graph$model[sort(object@Area$Modeloptarea)]),sep=", "),"\n\n",sep="")
              }else{
                cat("Model selected by jump area: ",as.character(object@graph$model[object@Area$Modeloptarea]),"\n\n",sep="")
              }
            }
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

##' Model selection by dimension jump
##'
##' \code{Djump} is a model selection function based on the slope heuristics.
##'
##' @param data \code{data} is a matrix or a data.frame with four columns of the same length
##' and each line corresponds to a model:
##'   \enumerate{
##'     \item The first column contains the model names.
##'     \item The second column contains the penalty shape values.
##'     \item The third column contains the model complexity values.
##'     \item The fourth column contains the minimum contrast value for each model.
##'   }
##' @param scoef Ratio parameter. Default value is 2.
##' @param Careajump Constant of jump area (See \code{\link[=Djump]{Djump}} for more details). Default value is 0 (no area).
##' @param Ctresh Maximal treshold for the complexity associated to the penalty coefficient (See \code{\link[=Djump]{Djump}} for more details).
##' Default value is 0 (Maximal jump selected as the greater jump).
##'
##' @slot graph list.
##' @slot Area list.
##'
##' @details
##' The Djump algorithm proceeds in three steps:
##' \enumerate{
##'   \item{For all \eqn{\kappa>0}, compute
##'     \eqn{m(\kappa)\in argmin_{m\in M} \{\gamma_n(\hat{s}_m)+\kappa\times pen_{shape}(m)\}}
##'     This gives a decreasing step function \eqn{\kappa \mapsto C_{m(\kappa)}}.}
##'   \item{Find \eqn{\hat{\kappa}} such that \eqn{C_{m(\hat{\kappa})}} corresponds to the
##'     greatest jump of complexity if \eqn{C_{tresh}=0} else \eqn{\hat{\kappa}} such that
##'     \eqn{\hat{\kappa}=inf\{\kappa>0: C_{m(\kappa)}\leq C_{tresh}\}.}
##'   }
##'   \item{Select \eqn{\hat{m}=m(scoef\times\hat{\kappa})} (output \code{@model}).}
##' }
##' Arlot has proposed a jump area containing the maximal jump defined by :
##'   \eqn{[\kappa(1-Careajump);\kappa(1+Careajump)].}
##' If \eqn{Careajump>0}, \code{Djump} return the area with the greatest jump. In practice,
##' it is advisable to take \eqn{Careajump=\frac{log(n)}{n}} where \eqn{n} is the number of observations.
##'
##' @returns
##' \describe{
##' \item{\code{@model}}{The \code{model} selected by the dimension jump method.}
##' \item{\code{@ModelHat}}{A list describing the algorithm.}
##' \item{\code{@ModelHat$jump}}{The vector of jump heights.}
##' \item{\code{@ModelHat$kappa}}{The vector of the values of \eqn{\kappa} at each jump.}
##' \item{\code{@ModelHat$model_hat}}{The vector of the selected models \eqn{m(\kappa)} by the jump.}
##' \item{\code{@ModelHat$JumpMax}}{The location of the greatest jump.}
##' \item{\code{@ModelHat$Kopt}}{ \eqn{\kappa_{opt}=scoef\hat{\kappa}}.}
##' \item{\code{@graph}}{A list computed for the \code{\link[=plot.Djump]{plot}} method.}
##' }
##'
##' @export Djump
##'
##' @references
##' Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
##' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
##'
##' @author Vincent Brault
##'
##' @seealso \code{\link[=capushe]{capushe}} for a model selection function including \code{\link[=AICcapushe]{AIC}},
##' \code{\link[=BICcapushe]{BIC}}, the \code{\link[=DDSE]{DDSE}} algorithm and the \code{Djump} algorithm.
##' \code{\link[=plot.Djump]{plot}} for a graphical display of the \code{DDSE}
##' algorithm and the \code{Djump} algorithm.
##'
##' @examples data(datacapushe)
##' Djump(datacapushe)
##' plot(Djump(datacapushe),newwindow=FALSE)
##' Djump(datacapushe,Careajump=sqrt(log(1000)/1000))
##' plot(Djump(datacapushe,Careajump=sqrt(log(1000)/1000)),newwindow=FALSE)
##' Djump(datacapushe,Ctresh=1000/log(1000))
##' plot(Djump(datacapushe,Ctresh=1000/log(1000)),newwindow=FALSE)

Djump = function(data,scoef=2,Careajump=0,Ctresh=0){

  if (is.character(data)){data=utils::read.table(data)}

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
  if (!prod(data$complexity>=0)){
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
  if(!(is.numeric(Careajump))){
    stop("Caerajump must be numeric")
  }
  if(Careajump<0){
    stop("Careajump must be positive")
  }
  if(Careajump>=1){
    stop("Careajump must be less than one")
  }
  if(!(is.numeric(Ctresh))){
    stop("Ctresh must be numeric")
  }
  if(Ctresh<0){
    stop("Ctresh must be positive")
  }
  if(Ctresh>max(data$complexity)){
    stop("Ctresh must be less than the maximum complexity")
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

  if(Ctresh==0){
    maxjump=which(jump==max(jump))
    if (length(maxjump)>1){
      warning("There are several maximum jump")
    }
    JumpMax=max(maxjump)
  }else{
    if(length(which((data$complexity[mhat]<=Ctresh)==TRUE))){
      JumpMax=which.max(data$complexity[mhat]<Ctresh)-1
    }else{
      stop("Ctresh is too small")
    }
  }

  if (Careajump>0){
    Const=(1-Careajump)/(1+Careajump)
    kappajump=kappa*Const
    Jumpareasup=numeric(mleng-1)
    for (i in 2:mleng){
      Jumpareasup[i-1]=data$complexity[mhat[which(((kappa[2:mleng]>kappajump[i])*(kappa[1:(mleng-1)]<=kappajump[i]))==TRUE)]]
    }
    Jumparea=Jumpareasup-data$complexity[mhat[1:(mleng-1)]]
    Jumpareamax=max(which(Jumparea==max(Jumparea)))
    kappareainf=scoef*kappajump[Jumpareamax+1]
    kappareasup=scoef*kappa[Jumpareamax+1]
    area=which(((kappa[2:mleng]>kappareainf)*(kappa[1:(mleng-1)]<=kappareasup))==TRUE)
    if (kappareasup>kappa[mleng]){
      area=c(area,mleng)
    }
    Modeloptarea=mhat[area]
    Area=list(Modeloptarea=Modeloptarea,area=area,kappainf=kappajump[Jumpareamax+1],kappasup=kappa[Jumpareamax+1])
  }else{
    Area=list()
  }



  Kopt=scoef*kappa[JumpMax+1]
  test=which(((kappa[2:mleng]>Kopt)*(kappa[1:(mleng-1)]<=Kopt))==TRUE)

  if (length(test)==0){
    Modelopt=mhat[mleng]
  }else{
    Modelopt=mhat[test]
  }



  check=list(jump,kappa,mhat,JumpMax,Kopt)
  names(check)=c("jump","kappa","model_hat","JumpMax","Kopt")
  graph=list(data$pen,data$complexity,data$model,Modelopt,Ctresh)
  names(graph)=c("pen","complexity","model","Modopt","Ctresh")

  methods::new(Class="Djump",model=as.character(data$model[Modelopt]),ModelHat=check,graph=graph,Area=Area)

}



setMethod(
  f="plot",
  signature="Djump",
  definition=function(x,y,newwindow=TRUE,...){

    leng=length(x@graph$model)
    mleng=length(x@ModelHat$model_hat)
    scoef=x@ModelHat$Kopt/x@ModelHat$kappa[x@ModelHat$JumpMax+1]

    if (newwindow){	grDevices::x11(width=15)}

    graphics::par(oma=c(0,3,0,0),xaxs="i")
    Absmax=max((scoef+1)/scoef*x@ModelHat$Kopt,5/4*x@Area$kappasup)
    Ordmin=max(which((x@ModelHat$kappa<=Absmax)==TRUE))
    graphics::plot(x=x@ModelHat$Kopt,y=x@graph$complexity[x@graph$Modopt],xlim=c(0,Absmax),ylim=c(x@graph$complexity[x@ModelHat$model_hat[Ordmin]],x@graph$complexity[x@ModelHat$model_hat[1]]),ylab="",xlab=expression(paste("Values of the penalty constant ",kappa)),yaxt="n",pch=4,col="blue",main="Dimension Jump",lwd=3,xaxt = "n")
    complex=x@graph$complexity[x@ModelHat$model_hat[1:Ordmin]]
    for (i in 1:(mleng-1)){
      graphics::lines(x=c(x@ModelHat$kappa[i],x@ModelHat$kappa[i+1]),y=c(complex[i],complex[i]))
    }
    if (x@ModelHat$kappa[i+1]<Absmax){
      graphics::lines(x=c(x@ModelHat$kappa[mleng],Absmax),y=c(complex[mleng],complex[mleng]))
    }

    graphics::lines(x=c(x@ModelHat$Kopt/scoef,x@ModelHat$Kopt/scoef),y=c(complex[x@ModelHat$JumpMax],complex[x@ModelHat$JumpMax+1]),col="blue",lty=2)
    ordon=paste(as.character(complex),"(",as.character(x@graph$model[x@ModelHat$model_hat[1:Ordmin]]),")")
    graphics::par(cex.axis=0.6)

    pas=(max(complex)-min(complex))/39*5.6/graphics::par("din")[2]
    leng2=length(complex)
    Coord=c()
    i=leng2
    j=leng2-1
    while (j>0){
      while ((j>1)*((complex[j]-complex[i])<pas)){
        Coord=c(Coord,-j)
        j=j-1
      }
      while ((j==1)*((complex[j]-complex[i])<pas)){
        Coord=c(Coord,-j)
        j=j-1
      }
      i=j
      j=j-1
    }
    if (x@graph$Ctresh>0){
      Coord=c(Coord,-which(abs(complex-x@graph$Ctresh)<pas))
      graphics::lines(c(graphics::par("usr")[1],
                        graphics::par("usr")[2]),
                      c(x@graph$Ctresh,x@graph$Ctresh),lty=4,col="chocolate1")
      graphics::axis(2,x@graph$Ctresh,expression(C[tresh]),las=2)
    }
    if (length(Coord)>0){
      complex=complex[Coord]
      ordon=ordon[Coord]
    }
    graphics::axis(2,complex,ordon,las=2)

    graphics::par(cex.axis=1)
    graphics::axis(2,labels="Selected Complexity (Model)",outer=TRUE,at=(x@graph$complexity[leng]+x@graph$complexity[x@ModelHat$model_hat[Ordmin]])/2,lty=0)
    if (scoef==1){
      graphics::axis(1,labels=  expression(kappa[opt] == hat(kappa)^{dj}),at=x@ModelHat$Kopt)
    }
    else {
      graphics::axis(1,labels=expression(hat(kappa)^{dj}),at=x@ModelHat$Kopt/scoef)
      graphics::axis(1,labels=  expression(kappa[opt]),at=x@ModelHat$Kopt)
    }



    if (scoef<=2){
      absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef/4)[c(-5)]
      Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/3*12.093749/graphics::par("din")[1]*scoef),
              which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/3*12.093749/graphics::par("din")[1]*scoef))
    }
    if ((2<scoef)*(scoef<4)){
      absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef/3)[c(-4)]
      Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/5*12.093749/graphics::par("din")[1]*scoef),
              which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/5*12.093749/graphics::par("din")[1]*scoef))
    }
    if ((4<=scoef)*(scoef<=6)){
      absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef/2)[c(-3)]
      Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/26*12.093749/graphics::par("din")[1]^(1/3)*scoef),
              which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/26*12.093749/graphics::par("din")[1]*scoef))
    }
    if (scoef>6){
      absci=seq(0,Absmax,by=x@ModelHat$Kopt/scoef)[c(-2)]
      Empty=c(which(abs(absci-x@ModelHat$Kopt)<absci[2]/35*12.093749/graphics::par("din")[1]*scoef),
              which(abs(absci-x@ModelHat$Kopt/scoef)<absci[2]/35*12.093749/graphics::par("din")[1]*scoef))
    }
    if (length(Empty)>0){
      absci=absci[-Empty]
    }
    graphics::axis(1,absci,signif(absci,2))

    if (length(x@Area)==4){
      graphics::lines(c(x@Area$kappainf,x@Area$kappasup),
                      c(graphics::par("usr")[3],graphics::par("usr")[3]),col="green4",lwd=2)
      yarea=(x@graph$complexity[x@ModelHat$model_hat[Ordmin]]+graphics::par("usr")[3])/2
      graphics::lines(c(x@Area$kappainf,x@Area$kappainf),c(graphics::par("usr")[3],yarea),col="green4",lwd=2)
      graphics::lines(c(x@Area$kappasup,x@Area$kappasup),c(graphics::par("usr")[3],yarea),col="green4",lwd=2)
      graphics::legend("topright",legend=c("Maximal Jump","Jump area"),lty=c(2,1),col=c("blue","green4"),lwd=c(1,2),bg = "white")
    }else{
      graphics::legend("topright",legend="Maximal Jump",lty=2,col="blue",bg = "white")
    }


  }
)


