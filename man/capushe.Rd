\name{capushe}
\alias{capushe}
\alias{Capushe-class}
\alias{plot,Capushe-method}
\alias{print,Capushe-method}
\alias{show,Capushe-method}
\alias{summary,Capushe-method}
\alias{validation,Capushe-method}
\alias{print.capushe}
\alias{show.capushe}
\alias{summary.capushe}
\alias{Capushe}
\alias{CAPUSHE}
\alias{Calibrating}
\alias{CAlibrating}

\title{
CAlibrating Penalities Using Slope HEuristics (CAPUSHE)
}
\description{
The \code{capushe} function proposes two algorithms based on the slope heuristics
to calibrate penalties in the context of model selection via penalization.
}
\usage{
capushe(data,n=0,pct=0.15,point=0,psi.rlm=psi.bisquare,scoef=2,
			Careajump=0,Ctresh=0)
}

\arguments{
  \item{data}{
\code{data} is a matrix or a data.frame with four columns of the same length
 and each line corresponds to a model:
  \enumerate{
    \item The first column contains the model names. 
    \item The second column contains the penalty shape values.   
    \item The third column contains the model complexity values. 
    \item The fourth column contains the minimum contrast value for each model. 
    }
}
  \item{n}{
  \code{n} is the sample size.  
}
  \item{pct}{
  Minimum percentage of points for the plateau selection. 
   See \code{\link[=DDSE]{DDSE}} for more details.
}
  \item{point}{
  Minimum number of point for the plateau selection (See \code{\link[=DDSE]{DDSE}} for more details).
   If \code{point} is different from 0, \code{pct} is obsolete.
}
  \item{psi.rlm}{
  Weight function used by \code{\link[MASS:rlm]{rlm}}.
  See \code{\link[=DDSE]{DDSE}} for more details. \code{psi.rlm}="lm" for non robust
  linear regression.
}
  \item{scoef}{
  Ratio parameter. Default value is 2.   
}
  \item{Careajump}{
  Constant of jump area (See \code{\link[=Djump]{Djump}} for more details). Default value is 0 (no area).
}
  \item{Ctresh}{
  Maximal treshold for the complexity associated to the penalty coefficient (See \code{\link[=Djump]{Djump}} for more details).
  Default value is 0 (Maximal jump selected as the greater jump).
}
}
\details{
The model \eqn{\hat{m}} selected by the procedure fulfills

\eqn{\hat{m}=} argmin \eqn{\gamma_n (\hat{s}_m)+scoef\times \kappa\times pen_{shape}(m)}

where \itemize{
  \item{\eqn{\kappa} is the penalty coefficient.}
  \item{\eqn{\gamma_n} is the empirical contrast.}
  \item{\eqn{\hat{s}_m} is the estimator for the model \eqn{m}.}
  \item{\eqn{scoef} is the ratio parameter.}
  \item{\eqn{pen_{shape}} is the penalty shape.}
}
The capushe function calls the functions \code{\link[=DDSE]{DDSE}} and 
\code{\link[=Djump]{Djump}} to calibrate \eqn{\kappa}, see the description of these functions
 for more details. 
In the case of equality between two penalty shape values, only the model with the 
smallest contrast is considered.


}
\value{
\item{@DDSE}{A list returned by the \code{\link[=DDSE]{DDSE}} function.}
\item{@DDSE@model}{The \code{model} selected by the \code{\link[=DDSE]{DDSE}} function.}
\item{@DDSE@kappa}{The vector of the successive slope values.}
\item{@DDSE@ModelHat}{A list providing details about the model selected by the \code{\link[=DDSE]{DDSE}} function.}
\item{@DDSE@interval}{A list about the "slope interval" corresponding to the
plateau selected in \code{DDSE}. See \code{\link[=DDSE]{DDSE}} for more details.}
\item{@DDSE@graph}{A list computed for the \code{\link[=plot.capushe]{plot}} function.}
\item{@Djump}{A list returned by the \code{\link[=Djump]{Djump}} function.}
\item{@Djump@model}{The \code{model} selected by the \code{\link[=Djump]{Djump}} function.} 
\item{@Djump@ModelHat}{A list providing details about the model selected by the \code{\link[=Djump]{Djump}} function.}
\item{@Djump@graph}{A list computed for the \code{\link[=plot.capushe]{plot}} function.}
\item{@AIC_capushe}{A list returned by the \code{\link[=AICcapushe]{AICcapushe}} function.} 
\item{@BIC_capushe}{A list returned by the \code{\link[=BICcapushe]{BICcapushe}} function.}
\item{@n}{Sample size.}
}
\references{
\url{http://www.math.univ-toulouse.fr/~maugis/CAPUSHE.html}

\url{http://www.math.u-psud.fr/~brault/capushe.html}

Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/
s11222-011-9236-1
}
\author{
Vincent Brault
}




\seealso{
\code{\link[=Djump]{Djump}}, \code{\link[=DDSE]{DDSE}}, \code{\link[=AICcapushe]{AIC}} 
or \code{\link[=BICcapushe]{BIC}} to use only one of these model selection functions. 
\code{\link[=plot.capushe]{plot}} for graphical displays of DDSE
 and Djump.
}
\examples{
data(datacapushe)
capushe(datacapushe) 
capushe(datacapushe,1000)
}

\keyword{models}

