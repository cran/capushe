\name{Djump}
\alias{Djump}
\alias{Djump-class}
\alias{plot,Djump-method}
\alias{print,Djump-method}
\alias{show,Djump-method}
\alias{summary,Djump-method}
\alias{print.Djump}
\alias{show.Djump}
\alias{summary.Djump}
\alias{djump}
\alias{DJUMP}
\alias{Dimensionjump}
\alias{DimensionJump}
\alias{Dimension_Jump}
\alias{Dimension_jump}

\title{
Model selection by dimension jump
}
\description{
\code{Djump} is a model selection function based on the slope heuristics.
}
\usage{
Djump(data,scoef=2)
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
\item{scoef}{
  Ratio parameter. Default value is 2.
  
}
}

\details{The Djump algorithm proceeds in three steps:
\enumerate{
\item{For all \eqn{\kappa>0}, compute

\eqn{m(\kappa)\in argmin_{m\in M} \{\gamma_n(\hat{s}_m)+\kappa\times pen_{shape}(m)\}}

This gives a decreasing step function \eqn{\kappa \mapsto C_{m(\kappa)}}.}
\item{Find \eqn{\hat{\kappa}} such that \eqn{C_{m(\hat{\kappa})}} corresponds to the
greatest jump of complexity.}
\item{Select \eqn{\hat{m}=m(scoef\times\hat{\kappa})} (output \code{@model}).}
}
}

\value{
\item{@model}{The \code{model} selected by the dimension jump method.}
\item{@ModelHat}{A list describing the algorithm.}
\item{@ModelHat$jump}{The vector of jump heights.}
\item{@ModelHat$kappa}{The vector of the values of \eqn{\kappa} at each jump.}
\item{@ModelHat$model_hat}{The vector of the selected models \eqn{m(\kappa)} by the jump.}
\item{@ModelHat$JumpMax}{The location of the greatest jump.}
\item{@ModelHat$Kopt}{ \eqn{\kappa_{opt}=scoef\hat{\kappa}}.}
\item{@graph}{A list computed for the \code{\link[=plot.Djump]{plot}} method.}
}
\references{
\url{http://www.math.univ-toulouse.fr/~maugis/CAPUSHE.html}

Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/
s11222-011-9236-1
}
\author{
Vincent Brault
}
                                                          
\seealso{
\code{\link[=capushe]{capushe}} for a model selection function including \code{\link[=AICcapushe]{AIC}},
\code{\link[=BICcapushe]{BIC}}, the \code{\link[=DDSE]{DDSE}} algorithm and the \code{Djump}
algorithm. \code{\link[=plot.Djump]{plot}} for a graphical display of the \code{DDSE}
algorithm and the \code{Djump} algorithm.
}
\examples{
data(datacapushe)
Djump(datacapushe)
plot(Djump(datacapushe))
}

\keyword{models}

