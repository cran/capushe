#' AICcapushe
#'
#' These functions return the model selected by the Akaike Information Criterion (AIC).
#'
#' @usage AICcapushe(data,n)
#'
#' @param data \code{data} is a matrix or a data.frame with four columns of the same length
#' and each line corresponds to a model:
#'   \enumerate{
#' \item The first column contains the model names.
#' \item The second column contains the penalty shape values.
#' \item The third column contains the model complexity values.
#' \item The fourth column contains the minimum contrast value for each model.
#'   }
#' @param n \code{n} is the sample size.
#'
#' @details
#' The penalty shape value should be increasing with respect to the complexity value (column 3).
#' The complexity values have to be positive.
#' \code{n} is necessary to compute AIC and BIC criteria. \code{n} is the size of
#' sample used to compute the contrast values given in the \code{data} matrix.
#' Do not confuse \code{n} with the size of the model collection which is the number
#' of rows of the \code{data} matrix.
#'
#'
#' @return model The model selected by AIC.
#' @export AICcapushe
#' @export BICcapushe
#'
#' @examples
#' data(datacapushe)
#' AICcapushe(datacapushe,n=1000)

AICcapushe = function(data,n){

  if(!(is.numeric(n))){
    stop("n must be numeric")
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
      warning(paste("1 line has been removed by AICcapushe"))
    }
    else{warning(paste(as.character(length(bad)),"lines have been removed by AICcapushe"))}
  }
  leng=length(data$model)
  if (leng==0){
    stop("We need more correct observation")
  }
  if ((length(data$pen)!=leng)|(length(data$complexity)!=leng)|(length(data$contrast)!=leng)){
    stop("The lengths of the columns must be equal")
  }
  if (!prod(data$complexity>=0)){
    stop("Complexity must be positive")
  }
  if (!(floor(n)==n)|(n<0)){
    stop("The number of observation must be an integer positive")
  }
  if(n==Inf){
    stop("n must be different of Inf")
  }

  AIC = data$contrast+data$complexity/n
  modelmin=which.min(AIC)
  result=list(data$model[modelmin],AIC[modelmin])
  names(result)=c("model","AIC")
  return(result)
}

#' BICcapushe
#'
#' These functions return the model selected by the Bayesian Information Criterion (BIC).
#'
#' @usage BICcapushe(data,n)
#'
#' @param data \code{data} is a matrix or a data.frame with four columns of the same length
#' and each line corresponds to a model:
#'   \enumerate{
#' \item The first column contains the model names.
#' \item The second column contains the penalty shape values.
#' \item The third column contains the model complexity values.
#' \item The fourth column contains the minimum contrast value for each model.
#'   }
#' @param n \code{n} is the sample size.
#'
#' @details
#' The penalty shape value should be increasing with respect to the complexity value (column 3).
#' The complexity values have to be positive.
#' \code{n} is necessary to compute AIC and BIC criteria. \code{n} is the size of
#' sample used to compute the contrast values given in the \code{data} matrix.
#' Do not confuse \code{n} with the size of the model collection which is the number
#' of rows of the \code{data} matrix.
#'
#'
#' @return model The model selected by BIC.
#' @export BICcapushe
#'
#' @examples
#' data(datacapushe)
#' BICcapushe(datacapushe,n=1000)

BICcapushe = function(data,n){

  if(!(is.numeric(n))){
    stop("n must be numeric")
  }

  if (is.character(data)){data=utils::read.table(data)}

  if (length(data[1,])==2){
    data=matrix(c(data[,1],data[,1],data[,1],data[,2]),ncol=4)
    warning("The names of model are 'complexity'")
  }
  if (length(data[1,])==3){
    data=matrix(c(data[,1],data[,2],data[,2],data[,3]),ncol=4)
  }
  if (length(data[1,])!=4){
    stop("data must have 2, 3 or 4 columns")
  }
  data=data.frame(data)
  names(data)=c("model","pen","complexity","contrast")
  if(any(is.na(data))){
    bad=which(is.na(data),arr.ind=TRUE)[,1]
    repbad=which(duplicated(bad),arr.ind=TRUE)
    if (length(repbad)!=0){bad=bad[-repbad]}
    data=data[-bad,]
    if (length(bad)==1){
      warning(paste("1 line has been removed by BICcapushe"))
    }
    else{warning(paste(as.character(length(bad)),"lines have been removed by BICcapushe"))}
  }
  leng=length(data$model)
  if (leng==0){
    stop("We need more correct observation")
  }
  if ((length(data$pen)!=leng)|(length(data$complexity)!=leng)|(length(data$contrast)!=leng)){
    stop("The lengths of the columns must be equal")
  }
  if (!prod(data$complexity>=0)){
    stop("Complexity must be positive")
  }
  if (!(floor(n)==n)|(n<0)){
    stop("The number of observation must be an integer positive")
  }
  if(n==Inf){
    stop("n must be different of Inf")
  }

  BIC = data$contrast+data$complexity*log(n)/(2*n)
  modelmin=which.min(BIC)
  result=list(data$model[modelmin],BIC[modelmin])
  names(result)=c("model","BIC")
  return(result)
}

##' validation
##'
##' @name validation
##' @aliases validation
##' @aliases validation-methods
##' @aliases validation.capushe
##' @aliases validation.DDSE
##' @aliases validationcapushe
##' @aliases validationDDSE
##'
##' @description
##' \code{validation} checks that the slope heuristics can be applied confidently.
##'
##' @usage validation(x,data2,\dots)
##'
##' @param x
##' \code{x} must be an object of \code{\link[=Capushe-class]{class capushe}}
##' or \code{\link[=DDSE-class]{DDSE}}, in practice an
##' output of the \code{\link[=capushe]{capushe}} function or the \code{\link[=DDSE]{DDSE}} function.
##' @param data2
##' \code{data2} is a matrix or a data.frame with four columns of the same length
##' and each line corresponds to a model:
##' \enumerate{
##'   \item The first column contains the model names.
##'   \item The second column contains the penalty shape values.
##'   \item The third column contains the model complexity values.
##'   \item The fourth column contains the minimum contrast value for each model.
##' }
##' @param \dots
##' \itemize{
##'     \item{
##'       If \code{newwindow==TRUE}, a new window is created for the plot.
##'     }
##' }
##'
##' @details
##' The \code{validation} function plots the additional and more complex models \code{data2}
##' to check that the linear relation between the penalty shape values and the contrast
##' values (which is recorded in \code{x}) is valid for the more complex models.
##'
##' @references
##' Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
##' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
##'
##' @author Brault Vincent
##'
##' @seealso \code{\link[=capushe]{capushe}} for a more general model selection function including
##' \code{\link[=AICcapushe]{AIC}}, \code{\link[=BICcapushe]{BIC}}, the \code{\link[=DDSE]{DDSE}}
##' algorithm and the \code{\link[=Djump]{Djump}} algorithm.
##'
##' @examples
##' data(datapartialcapushe)
##' capushepartial=capushe(datapartialcapushe)
##' data(datavalidcapushe)
##' validation(capushepartial,datavalidcapushe,newwindow=FALSE) ## The slope heuristics should not
##' ## be applied for datapartialcapushe.
##' data(datacapushe)
##' plot(capushe(datacapushe),newwindow=FALSE)
##'
##' @keywords models
##' @export validation
##'

setGeneric("validation",function(x,data2,...){standardGeneric("validation")})
