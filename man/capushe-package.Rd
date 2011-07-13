\name{capushe-package}
\alias{capushe-package}
\alias{capushepack}
\alias{capushe.package}
\alias{slope}
\alias{heuristic}
\docType{package}
\title{
Capushe
}
\description{
This package includes functions for model selection via penalization. The model 
selection criterion has the following form: \eqn{\gamma_n (\hat{s}_m)+scoef\times\kappa\times pen_{shape}(m)}.
Two algorithms based on the slope heuristics are proposed to calibrate the
parameter \eqn{\kappa} in the penalty: the \link[=DDSE]{data-driven slope estimation algorithm (DDSE)}
and the \link[=Djump]{dimension jump algorithm (Djump)}.
}
\details{
The data-driven slope estimation algorithm and the dimension jump algorithm are 
respectively implemented into the \code{\link[=DDSE]{DDSE}} function and the \code{\link[=Djump]{Djump}} function. Somes
classes are defined for the outputs of \code{\link[=DDSE]{DDSE}} and \code{\link[=Djump]{Djump}} and a \link[=plot.capushe]{graphical} display is
available for each one of these two classes. \code{\link[=DDSE]{DDSE}} and \code{\link[=Djump]{Djump}} are both included in
the \code{\link[=capushe]{capushe}} function which is the main function of the package.
}
\author{
Vincent Brault, Jean-Patrick Baudry, Cathy Maugis and Bertrand Michel.

Maintainer: Vincent Brault <vincent.brault@math.u-psud.fr>
}
\references{
\url{http://www.math.univ-toulouse.fr/~maugis/CAPUSHE.html}

Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/
s11222-011-9236-1
}

\keyword{package}
\seealso{
\code{\link[=Djump]{Djump}} and \code{\link[=DDSE]{DDSE}} for model selection
algorithms based on the slope heuristics. \code{\link[=plot.capushe]{plot}} for a 
 graphical display of the two algorithms. \code{\link[=validation]{validation}} 
 to check that the slope heuristics can be applied confidently.
}
\examples{
data(datacapushe)
## capushe returns the same model with DDSE and Djump:
capushe(datacapushe)
## capushe also returns the model selected by AIC and BIC
capushe(datacapushe,n=1000)
## Djump only
Djump(datacapushe)
## DDSE only
DDSE(datacapushe)
## Graphical representations
plot(Djump(datacapushe))
plot(DDSE(datacapushe))
plot(capushe(datacapushe))
## Validation procedure
data(datapartialcapushe)
capushepartial=capushe(datapartialcapushe)
plot(capushepartial)
## Additional data
data(datavalidcapushe)
validation(capushepartial,datavalidcapushe) ## The slope heuristics should not 
## be applied for datapartialcapushe.

}
