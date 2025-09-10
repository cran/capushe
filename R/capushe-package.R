#' @docType package
#' @name capushe-package
#' @aliases capushe-package
#' @aliases capushepack
#' @aliases capushe.package
#' @aliases slope
#' @aliases heuristic
#'
#' @title capushe
#'
#' @description
#' This package includes functions for model selection via penalization. The model
#' selection criterion has the following form: \eqn{\gamma_n (\hat{s}_m)+scoef\times\kappa\times pen_{shape}(m)}.
#' Two algorithms based on the slope heuristics are proposed to calibrate the
#' parameter \eqn{\kappa} in the penalty: the \link[=DDSE]{data-driven slope estimation algorithm (DDSE)}
#' and the \link[=Djump]{dimension jump algorithm (Djump)}.
#'
#' @details
#' The data-driven slope estimation algorithm and the dimension jump algorithm are
#' respectively implemented into the \code{\link[=DDSE]{DDSE}} function and the \code{\link[=Djump]{Djump}} function. Somes
#' classes are defined for the outputs of \code{\link[=DDSE]{DDSE}} and \code{\link[=Djump]{Djump}} and a \link[=plot.capushe]{graphical} display is
#' available for each one of these two classes. \code{\link[=DDSE]{DDSE}} and \code{\link[=Djump]{Djump}} are both included in
#' the \code{\link[=capushe]{capushe}} function which is the main function of the package.
#'
#' @author Vincent Brault with the participation of Jean-Patrick Baudry, Cathy Maugis, Bertrand Michel and Sylvain Arlot
#' Maintainer: Vincent Brault <vincent.brault@univ-grenoble-alpes.fr>
#'
#' @references
#' Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
#' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
#'
#' @keywords package
#'
#' @keywords internal
#'
#' @seealso \code{\link[=Djump]{Djump}} and \code{\link[=DDSE]{DDSE}} for model selection
#' algorithms based on the slope heuristics. \code{\link[=plot.capushe]{plot}} for a
#' graphical display of the two algorithms. \code{\link[=validation]{validation}}
#' to check that the slope heuristics can be applied confidently.
#'
#' @examples
#' data(datacapushe)
#' ## capushe returns the same model with DDSE and Djump:
#' capushe(datacapushe)
#' ## capushe also returns the model selected by AIC and BIC
#' capushe(datacapushe,n=1000)
#' ## Djump only
#' Djump(datacapushe)
#' ## DDSE only
#' DDSE(datacapushe)
#' ## Graphical representations
#' plot(Djump(datacapushe),newwindow=FALSE)
#' plot(DDSE(datacapushe),newwindow=FALSE)
#' plot(capushe(datacapushe),newwindow=FALSE)
#' ## Validation procedure
#' data(datapartialcapushe)
#' capushepartial=capushe(datapartialcapushe)
#' plot(capushepartial,newwindow=FALSE)
#' ## Additional data
#' data(datavalidcapushe)
#' validation(capushepartial,datavalidcapushe,newwindow=FALSE) ## The slope heuristics should not
#' ## be applied for datapartialcapushe.
"_PACKAGE"
