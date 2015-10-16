\name{validation}
\alias{validation}
\alias{validation-methods}
\alias{validation.capushe}
\alias{validation.DDSE}
\alias{validationcapushe}
\alias{validationDDSE}
\title{
validation
}
\description{
\code{validation} checks that the slope heuristics can be applied confidently.
}
\usage{
validation(x,data2,...)
}

\arguments{
  \item{x}{
    \code{x} must be an object of \code{\link[=Capushe-class]{class capushe}} 
    or \code{\link[=DDSE-class]{DDSE}}, in practice an 
    output of the \code{\link[=capushe]{capushe}} function or the \code{\link[=DDSE]{DDSE}} function. 
}
\item{data2}{
\code{data2} is a matrix or a data.frame with four columns of the same length
 and each line corresponds to a model:
  \enumerate{
    \item The first column contains the model names. 
    \item The second column contains the penalty shape values.   
    \item The third column contains the model complexity values. 
    \item The fourth column contains the minimum contrast value for each model. 
    }
}
  \item{\dots}{
\itemize{
      \item{
      If \code{newwindow==TRUE}, a new window is created for the plot.
    }
}
}
}
\details{
The \code{validation} function plots the additional and more complex models \code{data2}
 to check that the linear relation between the penalty shape values and the contrast
 values (which is recorded in \code{x}) is valid for the more complex models.
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
\code{\link[=capushe]{capushe}} for a more general model selection function including
\code{\link[=AICcapushe]{AIC}}, \code{\link[=BICcapushe]{BIC}}, the \code{\link[=DDSE]{DDSE}}
algorithm and the \code{\link[=Djump]{Djump}} algorithm.
}
\examples{
data(datapartialcapushe)
capushepartial=capushe(datapartialcapushe)
data(datavalidcapushe)
validation(capushepartial,datavalidcapushe) ## The slope heuristics should not 
## be applied for datapartialcapushe.
data(datacapushe)
plot(capushe(datacapushe))

}


\keyword{models}
