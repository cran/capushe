\name{BICAICcapushe}
\alias{AICcapushe}
\alias{BICcapushe}
\alias{AICapushe}
\alias{BICapushe}
\alias{Aiccapushe}
\alias{Biccapushe}
\alias{Aicapushe}
\alias{Bicapushe}
\alias{aiccapushe}
\alias{biccapushe}
\alias{aicapushe}
\alias{bicapushe}

\title{
AICcapushe and BICcapushe
}
\description{
These functions return the model selected by the Akaike Information Criterion (AIC)
and the Bayesian Information Criterion (BIC).
}
\usage{
AICcapushe(data, n)
BICcapushe(data, n)
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
}
\details{
 The penalty shape value should be increasing with respect to the complexity value (column 3).
 The complexity values have to be positive.
 \code{n} is necessary to compute AIC and BIC criteria. \code{n} is the size of 
 sample used to compute the contrast values given in the \code{data} matrix.
  Do not confuse \code{n} with the size of the model collection which is the number
   of rows of the \code{data} matrix. 
}
\value{
  \item{model}{The model selected by AIC or BIC.}
  \item{AIC}{The corresponding value of AIC (for AICcapushe only).}
  \item{BIC}{The corresponding value of BIC (for BICcapushe only).}
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
\code{\link[=capushe]{capushe}} for a model selection function including AIC, BIC,
 the \code{\link[=DDSE]{DDSE}} algorithm and the \code{\link[=Djump]{Djump}} algorithm.
}
\examples{
data(datacapushe)
AICcapushe(datacapushe,n=1000)
BICcapushe(datacapushe,n=1000)


}

\keyword{models}
