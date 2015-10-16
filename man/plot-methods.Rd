\name{plot-methods}
\docType{methods}
\alias{plot.capushe}
\alias{plot.DDSE}
\alias{plot.Djump}
\alias{plotcapushe}
\alias{plotDDSE}
\alias{plotDjump}

\title{Plot for capushe}
\description{
The plot methods allow the user to check that the slope heuristics can be applied
 confidently.

\code{Usage}

plot(x,newwindow=TRUE,ask=TRUE) for \code{\link[=capushe]{capushe}}.

plot(x,newwindow=TRUE) for \code{\link[=DDSE]{DDSE}} and \code{\link[=Djump]{Djump}}.
}

\section{Methods}{
\describe{

\item{\code{signature(x = "Capushe")}}{ This graphical function displays the \code{DDSE} plot and the \code{Djump} plot.}

\item{\code{signature(x = "DDSE")}}{ This graphical function displays the \code{\link[=DDSE]{DDSE}} plot.}

\item{\code{signature(x = "Djump")}}{ This graphical function displays the \code{\link[=Djump]{Djump}} plot.}
}}
\details{
The graphical window of \code{DDSE} is composed of three graphics (see \code{\link[=DDSE]{DDSE}} for more details):
\describe{
\item{left}{The left plot shows \eqn{-\gamma_n(\hat{s}_m)} with respect to the 
penalty shape values.}
\item{topright}{Successive slope values \eqn{\hat{\kappa}(p)}.}
\item{bottomright}{The bottomright plot shows the selected models \eqn{\hat{m}(p)} with respect 
to the successive slope values. The plateau in blue is selected.} 
}
The graphical window of \code{Djump} shows the complexity  \eqn{C_{m(\kappa)}} of
 the selected model with respect to \eqn{\kappa}. \eqn{\hat{\kappa}^{dj}} corresponds
 to the greatest jump. \eqn{\kappa_{opt}} is defined by \eqn{\kappa_{opt}=scoef\times \hat{\kappa}^{dj}}.
 The red line represents the slope interval computed by the \code{DDSE} algorithm
 (only for \code{capushe}). See \code{\link[=Djump]{Djump}} for more details.
}
\arguments{
\item{x}{Output of \code{\link[=DDSE]{DDSE}}, \code{\link[=Djump]{Djump}} or \code{\link[=capushe]{capushe}}.}
\item{newwindow}{If \code{newwindow}=\code{TRUE}, a new window is created for each 
plot.}
\item{ask}{If \code{ask}=\code{TRUE}, \code{plot} waits for the user to press
a key to display the next plot (only for the class \code{capushe}).}
}
\note{
Use \code{newwindow}=\code{FALSE} to produce a PDF files (for an object of class
\code{capushe}, use moreover \code{ask}=\code{FALSE}).
}
\keyword{methods}

