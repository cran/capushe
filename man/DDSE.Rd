\name{DDSE}
\alias{DDSE}
\alias{DDSE-class}
\alias{plot,DDSE-method}
\alias{print,DDSE-method}
\alias{show,DDSE-method}
\alias{summary,DDSE-method}
\alias{validation,DDSE-method}
\alias{print.DDSE}
\alias{show.DDSE}
\alias{summary.DDSE}
\alias{ddse}
\alias{Ddse}
\alias{Data-Driven}
\alias{data-driven}
\alias{Data-driven}

\title{
Model selection by Data-Driven Slope Estimation
}
\description{
\code{DDSE} is a model selection function based on the slope heuristics.
}
\usage{
DDSE(data, pct = 0.15, point = 0, psi.rlm = psi.bisquare, scoef = 2)
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
  \item{pct}{
  Minimum percentage of points for the plateau selection.
   It must be between 0 and 1. Default value is 0.15.
}
  \item{point}{
  Minimum number of point for the plateau selection. 
  If \code{point} is different from 0, \code{pct} is obsolete.
}
  \item{psi.rlm}{
  Weight function used by \code{\link[MASS:rlm]{rlm}}. \code{psi.rlm}="lm" for non robust
  linear regression. 
}
  \item{scoef}{
  Ratio parameter. Default value is 2.
  
}
}
\details{Let \eqn{M} be the model collection and \eqn{P=\{pen_{shape}(m),m\in M\}}.
The DDSE algorithm proceeds in four steps:
\enumerate{
\item{If several models in the collection have the same penalty shape value (column 2),
only the model having the smallest contrast value \eqn{\gamma_n(\hat{s}_m)} (column 4)
is considered.}
\item{For any \eqn{p\in P}, the slope \eqn{\hat{\kappa}(p)} (argument \code{@kappa}) of the linear regression
(argument \code{psi.rlm}) on the couples of points \eqn{\{(pen_{shape}(m),-\gamma_n (\hat{s}_m)); pen_{shape}(m)\geq p\}}
is computed.}
\item{For any \eqn{p\in P}, the model fulfilling the following condition is selected:

\eqn{\hat{m}(p)=} argmin \eqn{\gamma_n (\hat{s}_m)+scoef\times \hat{\kappa}(p)\times pen_{shape}(m)}.

This gives an increasing sequence of change-points \eqn{(p_i)_{1\leq i\leq I+1}} (output 
\code{@ModelHat$point_breaking}). Let \eqn{(N_i)_{1\leq i\leq I}} (output \code{@ModelHat$number_plateau})
be the lengths of each "plateau".}
\item{If \code{point} is different from 0, let \eqn{\hat{i}=} max 
\eqn{\{1\leq i\leq I; N_i\geq point\}} else let \eqn{\hat{i}=} max 
\eqn{\{1\leq i\leq I; N_i\geq pct\sum_{l=1}^IN_l\}} (output \code{@ModelHat$imax}).
The model \eqn{\hat{m}(p_{\hat{i}})} (output \code{@model})} is finally returned.
}
The "slope interval" is the interval \eqn{[a,b]} where \eqn{a=inf\{\hat{\kappa}(p),p\in[p_{\hat{i}},p_{\hat{i}+1}[\cap P\}}
and \eqn{b=sup\{\hat{\kappa}(p),p\in[p_{\hat{i}},p_{\hat{i}+1}[\cap P\}}.
}
\value{
\item{@model}{The \code{model} selected by the DDSE algorithm.}
\item{@kappa}{The vector of the successive slope values.}
\item{@ModelHat}{A list describing the algorithm.}
\item{@ModelHat$model_hat}{The vector of preselected models \eqn{\hat{m}(p)}.}
\item{@ModelHat$point_breaking}{The vector of the breaking points \eqn{(p_i)_{1\leq i\leq I+1}}.}
\item{@ModelHat$number_plateau}{The vector of the lengths \eqn{(N_i)_{1\leq i\leq I}}.}
\item{@ModelHat$imax}{The rank \eqn{\hat{i}} of the selected plateau.}
\item{@interval}{A list about the "slope interval".}
\item{@interval$interval}{The slope interval.}
\item{@interval$percent_of_points}{The proportion \eqn{N_{\hat{i}}/\sum_{l=1}^IN_l}.}
\item{@graph}{A list computed for the \code{\link[=plot.DDSE]{plot}} method.}
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
\code{\link[=capushe]{capushe}} for a model selection function including \code{\link[=AICcapushe]{AIC}},
 \code{\link[=BICcapushe]{BIC}}, the \code{DDSE} algorithm and the \code{\link[=Djump]{Djump}} algorithm.
 \code{\link[=plot,DDSE-method]{plot}} for graphical dsiplays of the \code{DDSE} algorithm
 and the \code{Djump} algorithm.
}
\examples{
data(datacapushe)
DDSE(datacapushe)
plot(DDSE(datacapushe))
## DDSE with "lm" for the regression
DDSE(datacapushe,psi.rlm="lm")
}
\keyword{models}
