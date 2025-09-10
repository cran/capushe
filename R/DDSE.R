##' Model selection by Data-Driven Slope Estimation
##'
##' @name DDSE
##' @aliases DDSE
##' @aliases DDSE-class
##' @aliases print,DDSE-method
##' @aliases show,DDSE-method
##' @aliases summary,DDSE-method
##' @aliases validation,DDSE-method
##' @aliases print.DDSE
##' @aliases show.DDSE
##' @aliases summary.DDSE
##' @aliases ddse
##' @aliases Ddse
##' @aliases Data-Driven
##' @aliases data-driven
##' @aliases Data-driven
##'
##' @description
##' \code{DDSE} is a model selection function based on the slope heuristics.
##'
##' @usage DDSE(data, pct = 0.15, point = 0, psi.rlm = psi.bisquare, scoef = 2)
##'
##' @param data
##' \code{data} is a matrix or a data.frame with four columns of the same length
##' and each line corresponds to a model:
##' \enumerate{
##'     \item The first column contains the model names.
##'     \item The second column contains the penalty shape values.
##'     \item The third column contains the model complexity values.
##'     \item The fourth column contains the minimum contrast value for each model.
##' }
##' @param pct
##' Minimum percentage of points for the plateau selection.
##' It must be between 0 and 1. Default value is 0.15.
##' @param point
##' Minimum number of point for the plateau selection.
##' If \code{point} is different from 0, \code{pct} is obsolete.
##' @param psi.rlm
##' Weight function used by \code{\link[MASS:rlm]{rlm}}. \code{psi.rlm}="lm" for non robust
##' linear regression.
##' @param scoef
##' Ratio parameter. Default value is 2.
##'
##' @details
##' Let \eqn{M} be the model collection and \eqn{P=\{pen_{shape}(m),m\in M\}}.
##' The DDSE algorithm proceeds in four steps:
##' \enumerate{
##'     \item{If several models in the collection have the same penalty shape value (column 2),
##'     only the model having the smallest contrast value \eqn{\gamma_n(\hat{s}_m)} (column 4)
##'     is considered.}
##'     \item{For any \eqn{p\in P}, the slope \eqn{\hat{\kappa}(p)} (argument \code{@kappa}) of the linear regression
##'     (argument \code{psi.rlm}) on the couples of points \eqn{\{(pen_{shape}(m),-\gamma_n (\hat{s}_m)); pen_{shape}(m)\geq p\}}
##'     is computed.}
##'     \item{For any \eqn{p\in P}, the model fulfilling the following condition is selected:
##'     \eqn{\hat{m}(p)=} argmin \eqn{\gamma_n (\hat{s}_m)+scoef\times \hat{\kappa}(p)\times pen_{shape}(m)}.
##'     This gives an increasing sequence of change-points \eqn{(p_i)_{1\leq i\leq I+1}} (output
##'     \code{@ModelHat$point_breaking}). Let \eqn{(N_i)_{1\leq i\leq I}} (output \code{@ModelHat$number_plateau})
##'     be the lengths of each "plateau".}
##'     \item{If \code{point} is different from 0, let \eqn{\hat{i}=} max
##'     \eqn{\{1\leq i\leq I; N_i\geq point\}} else let \eqn{\hat{i}=} max
##'     \eqn{\{1\leq i\leq I; N_i\geq pct\sum_{l=1}^IN_l\}} (output \code{@ModelHat$imax}).
##'     The model \eqn{\hat{m}(p_{\hat{i}})} (output \code{@model})} is finally returned.
##' }
##' The "slope interval" is the interval \eqn{[a,b]} where \eqn{a=inf\{\hat{\kappa}(p),p\in[p_{\hat{i}},p_{\hat{i}+1}[\cap P\}}
##' and \eqn{b=sup\{\hat{\kappa}(p),p\in[p_{\hat{i}},p_{\hat{i}+1}[\cap P\}}.
##'
##' @returns
##' \item{@model}{The \code{model} selected by the DDSE algorithm.}
##' \item{@kappa}{The vector of the successive slope values.}
##' \item{@ModelHat}{A list describing the algorithm.}
##' \item{@ModelHat$model_hat}{The vector of preselected models \eqn{\hat{m}(p)}.}
##' \item{@ModelHat$point_breaking}{The vector of the breaking points \eqn{(p_i)_{1\leq i\leq I+1}}.}
##' \item{@ModelHat$number_plateau}{The vector of the lengths \eqn{(N_i)_{1\leq i\leq I}}.}
##' \item{@ModelHat$imax}{The rank \eqn{\hat{i}} of the selected plateau.}
##' \item{@interval}{A list about the "slope interval".}
##' \item{@interval$interval}{The slope interval.}
##' \item{@interval$percent_of_points}{The proportion \eqn{N_{\hat{i}}/\sum_{l=1}^IN_l}.}
##' \item{@graph}{A list computed for the \code{\link[=plot.DDSE]{plot}} method.}
##'
##' @references
##' Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
##' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
##'
##' @author Vincent Brault
##'
##' @seealso \code{\link[=capushe]{capushe}} for a model selection function including \code{\link[=AICcapushe]{AIC}},
##' \code{\link[=BICcapushe]{BIC}}, the \code{DDSE} algorithm and the \code{\link[=Djump]{Djump}} algorithm.
##' \code{\link[=plot,DDSE-method]{plot}} for graphical dsiplays of the \code{DDSE} algorithm
##' and the \code{Djump} algorithm.
##'
##' @export DDSE
##' @export validation
##'
##' @keywords models
##' @importFrom MASS rlm
##' @importFrom MASS psi.bisquare
##' @importFrom graphics plot
##' @importFrom grDevices x11
##' @importFrom graphics legend
##' @importFrom graphics points
##' @importFrom graphics par



setClass(
  Class="DDSE",
  representation=representation(
    model="character",
    kappa="numeric",
    ModelHat="list",
    interval="list",
    graph="list")
)

setMethod("print","DDSE",
          function(x,...){
            print(x@model)
          }
)

setMethod("show","DDSE",
          function(object){
            print(object@model)
          }
)

setMethod("summary","DDSE",
          function(object,checking=FALSE,checkgraph=FALSE){
            cat("\nSelected model: ",as.character(object@model),"\n",sep="")
            cat("\nCorresponding Slope interval: [",as.character(signif(object@interval$interval[1],3)),";",as.character(signif(object@interval$interval[2],3)),"](",as.character(signif(object@interval$percent_of_points,3)*100),"% of points)\n",sep="")
            cat("\nSelected model complexity: ",as.character(object@graph$Complexity),"\n",sep="")
            if (checking){
              result=list(object@model,object@kappa,object@ModelHat,object@interval)
              names(result)=c("model","kappa","ModelHat","interval")
              if (checkgraph==TRUE){
                Tempo=list(object@graph)
                names(Tempo)=c("graph")
                result=c(result,Tempo)
              }
              result
            }
          }
)


setMethod(
  f="plot",
  signature="DDSE",
  definition=function(x,y,newwindow=TRUE,...){


    plength=length(x@graph$model)-1
    p=plength-x@interval$point_using+2

    Model=x@ModelHat$model_hat[x@ModelHat$point_breaking[x@ModelHat$imax]]

    if (newwindow){	grDevices::x11(width=14)}
    par(mgp = c(3, 0.5, 0))
    graphics::layout(matrix(c(1,1,1,1,1,1,1,1,2,3,2,3,2,3),ncol=7))
    Couleur=c(rep("blue",(p-1)),rep("red",(plength-p+2)))
    plot(x=x@graph$pen,y=-x@graph$contrast,main="Contrast representation",xlab=expression(paste("\n\n",pen[shape](m)," (labels : Model names)")),ylab=expression(-gamma[n] (hat(s)[m])),col=Couleur,xaxt = "n")
    legend("bottomright",legend=c(paste("The regression line is computed with",as.character(plength-p+2),"points")),lty=1,col=c("red"))

    labelpen=x@graph$pen
    labelmodel=x@graph$model
    pas=(max(labelpen)-min(labelpen))/60*11.5/par("din")[1]
    leng=length(labelpen)
    Coord=c()
    i=1
    j=2
    while (j<leng){
      while ((j<leng)*((labelpen[j]-labelpen[i])<pas)){
        Coord=c(Coord,-j)
        j=j+1
      }
      i=j
      j=j+1
    }

    if (length(Coord)>0){
      labelpen=labelpen[Coord]
      labelmodel=labelmodel[Coord]
    }
    graphics::axis(1,labelpen,labelmodel,las=2)

    Cof=x@graph$reg$coefficients
    graphics::abline(a=Cof[1],b=Cof[2],col="red")
    abscisse=seq(plength+1,2)
    plot(x@kappa,main="Successive slope values",xlab=expression(paste("Number of points (",pen[shape](m),",",-gamma[n] (hat(s)[m]),") for the regression")),ylab=expression(kappa),type="l",xaxt = "n")

    abscisse2=seq(1,plength+1,length=floor(13*11.5/par("din")[1]))
    graphics::axis(1,abscisse2,abscisse[abscisse2])

    points(x@kappa,col="red",pch=18)
    plot(x@ModelHat$model_hat,main="Selected models with respect to the successive slope values",xlab=expression(paste("Number of points (",pen[shape](m),",",-gamma[n] (hat(s)[n]),") for the regression")),ylab="Model",pch=20,yaxt="n",xaxt = "n")
    graphics::axis(1,abscisse2,abscisse[abscisse2])

    labelhat=x@ModelHat$model_hat
    pas=(max(labelhat)-min(labelhat))/61*5.75/par("din")[2]
    leng=length(labelhat)
    Coord=c()
    i=1
    j=2
    while (j<leng){
      while ((j<leng)*((labelhat[j]-labelhat[i])<pas)){
        Coord=c(Coord,-j)
        j=j+1
      }
      i=j
      j=j+1
    }
    Coord=c(Coord,-which(duplicated(labelhat)==TRUE))
    if (length(Coord)>0){
      labelhat=labelhat[Coord]
    }

    graphics::axis(2,labelhat,x@graph$model[labelhat],las=1)
    graphics::lines(x=x@ModelHat$point_breaking[x@ModelHat$imax]:(x@ModelHat$point_breaking[x@ModelHat$imax]+x@ModelHat$number_plateau[x@ModelHat$imax]-1),y=rep(Model,x@ModelHat$number_plateau[x@ModelHat$imax]),col="blue")
  }
)


DDSE = function(data,pct=0.15,point=0,psi.rlm=psi.bisquare,scoef=2){
  if(!(is.numeric(pct))){
    stop("pct must be numeric")
  }
  if(!(is.numeric(point))){
    stop("point must be numeric")
  }
  if (!(floor(point)==point)|(point<0)){
    stop("point must be an positive integer")
  }
  if(!(is.numeric(scoef))){
    stop("scoef must be numeric")
  }

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
      warning(paste("1 line has been removed by DDSE"))
    }
    else{warning(paste(as.character(length(bad)),"lines have been removed by DDSE"))}
  }
  mlength=length(data$model)
  if (mlength<10){
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

  if ((length(data$pen)!=mlength)|(length(data$complexity)!=mlength)|(length(data$contrast)!=mlength)){
    stop("The lengths of the columns are not equal")
  }

  if ((pct<0)|(pct>1)){
    stop("pct must be between 0 and 1")
  }
  if (point>mlength){
    stop("point must be smaller than the number of models")
  }
  if (!prod(data$complexity>=0)){
    stop("Complexity must be positive")
  }

  data=data[order(data$pen,data$contrast),]
  plength=length(data$pen)
  for (i in plength:2){
    if (data$pen[i]==data$pen[i-1]){
      data=data[-i,]
      mlength=mlength-1
    }
  }
  Tempoligne=data$pen[order(data$complexity)]
  if (!prod((Tempoligne[2:mlength]-Tempoligne[1:(mlength-1)])>0)){
    stop("Penshape should be an increasing function of the complexity")
  }

  P=data$pen
  plength=length(P)-1
  kappa=numeric(plength)
  couplepen=P
  couplegamma=-data$contrast
  if (!is.function(psi.rlm)){
    warning("The function lm is used instead of rlm")
    for (p in 1:(plength)){
      kappa[p]=stats::lm(couplegamma~couplepen)$coefficients[2]
      couplepen=couplepen[-1]
      couplegamma=couplegamma[-1]
    }
  }
  else{
    options("warn"=-1)
    for (p in 1:(plength)){
      kappa[p]=MASS::rlm(couplegamma~couplepen,psi=psi.rlm)$coefficients[2]
      couplepen=couplepen[-1]
      couplegamma=couplegamma[-1]
    }
    options("warn"=0)
  }
  if (!prod(kappa>0)){
    warning("Some elements in Kappa are negative")
  }

  mhat=numeric(plength)
  for (p in 1:plength){
    mhat[p]=which.min(data$contrast+scoef*kappa[p]*data$pen)
  }

  Pi=c(1)
  N=c()
  number=1
  for (p in 2:plength){
    if (mhat[p]!=mhat[p-1]){
      Pi=c(Pi,p)
      N=c(N,number)
      number=0
    }
    number=number+1
  }
  N=c(N,number)
  Pi=c(Pi,p)

  Imax=length(N)
  Ntot=sum(N)
  if (point==0){
    const=pct*Ntot
    while (N[Imax]<const){
      Imax=Imax-1
      if (Imax==0){
        stop("pct is too high")
      }
    }
  }
  else {
    while (N[Imax]<point){
      Imax=Imax-1
      if (Imax==0){
        stop("point is too high")
      }
    }
  }

  No=N[Imax]
  p1=Pi[Imax]
  p=p1+floor(No/2)
  Model=mhat[p]


  result=list(data$model[Model])
  names(result)=c("Model")

  ModelHat=list(mhat,Pi,N,Imax)
  names(ModelHat)=c("model_hat","point_breaking","number_plateau","imax")

  Interval=kappa[Pi[Imax]:(Pi[Imax+1]-1)]
  inter=c(min(Interval),max(Interval))
  names(inter)=c("min","max")
  pourcent=(No+1)/(Ntot+1)
  interval = list(plength-p+2,inter,pourcent)
  names(interval)=c("point_using","interval","percent_of_points")

  if (!is.function(psi.rlm)){
    reg=stats::lm(-data$contrast[p:(plength+1)]~data$pen[p:(plength+1)])
  }
  else{
    options(warn=-1)
    reg=MASS::rlm(-data$contrast[p:(plength+1)]~data$pen[p:(plength+1)],psi=psi.rlm)
    options(warn=0)
  }
  graph=list(reg,data$pen,data$contrast,data$model,data$complexity[mhat[Pi[Imax]]])
  names(graph)=c("reg","pen","contrast","model","Complexity")


  methods::new(Class="DDSE",model=as.character(data$model[Model]),kappa=kappa,ModelHat=ModelHat,interval=interval,graph=graph)

}


setMethod(
  f="validation",
  signature="DDSE",
  definition=function(x,data2,newwindow=TRUE,...){

    if (is.character(data2)){data2=utils::read.table(data2)}
    if (length(data2[1,])!=4){
      stop("data2 must have 4 columns with name, penshape, complexity and contrast")
    }
    if(any(is.na(data2))){
      bad=which(is.na(data2),arr.ind=TRUE)[,1]
      repbad=which(duplicated(bad),arr.ind=TRUE)
      if (length(repbad)!=0){bad=bad[-repbad]}
      data2=data2[-bad,]
      if (length(bad)==1){
        warning(paste("1 line in data2 has been removed by validation"))
      }
      else{warning(paste(as.character(length(bad)),"lines in data2 have been removed by validation"))}
    }
    data2=data.frame(data2)
    names(data2)=c("model","pen","complexity","contrast")

    plength2=length(data2[,1])
    plength=length(x@graph$model)-1
    p=plength-x@interval$point_using+2
    pen=c(x@graph$pen,data2$pen)
    contrast=c(x@graph$contrast,data2$contrast)
    model=c(x@graph$model,data2$model)

    if (newwindow){	grDevices::x11(width=14) }
    par(mgp = c(3, 0.5, 0))
    Couleur=c(rep("blue",(p-1)),rep("red",(plength-p+2)))
    Cof=x@graph$reg$coefficients
    plot(xlim=c(min(pen),max(pen)),ylim=c(min(-contrast),max(-contrast,Cof[1]+Cof[2]*data2$pen)),x=x@graph$pen,y=-x@graph$contrast,main="Contrast representation",xlab=expression(paste(pen[shape](m)," (labels : Model names)")),ylab=expression(-gamma[n] (hat(s)[m])),col=Couleur,xaxt = "n")
    points(x=data2$pen,y=-data2$contrast,col="black",pch="*")

    legend("bottomright",legend=c(paste("The regression line is computed with",as.character(plength-p+2),"points"),"Validation points"),lty=c(1,0),col=c("red","black"),pch=c(".","*"))

    labelpen=pen
    labelmodel=model
    pas=(max(labelpen)-min(labelpen))/100*11.5/par("din")[1]
    leng=length(labelpen)
    Coord=c()
    i=1
    j=2
    while (j<leng){
      while ((j<leng)*((labelpen[j]-labelpen[i])<pas)){
        Coord=c(Coord,-j)
        j=j+1
      }
      i=j
      j=j+1
    }

    if (length(Coord)>0){
      labelpen=labelpen[Coord]
      labelmodel=labelmodel[Coord]
    }
    graphics::axis(1,labelpen,labelmodel,las=2)

    graphics::abline(a=Cof[1],b=Cof[2],col="red")
    abscisse=seq(plength+1,2)
  }
)
