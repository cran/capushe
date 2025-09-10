#' @title datacapushe
#'
#' @description
#'
#' A dataframe example for the \code{\link[=capushe-package]{capushe package}} based on a simulated Gaussian
#' mixture dataset in \eqn{\R^3}.
#'
#' @usage data(datacapushe)
#'
#' @format A data frame with 50 rows (models) and the following 4 variables:
#' \describe{
#'   \item{\code{model}}{a character vector: model names.}
#'   \item{\code{pen}}{a numeric vector: model penalty shape values.}
#'   \item{\code{complexity}}{a numeric vector: model complexity values.}
#'   \item{\code{contrast}}{a numeric vector: model contrast values.}
#' }
#'
#' @details
#' The simulated dataset is composed of \eqn{n=1000} observations in \eqn{\R^3}. It
#' consists of an equiprobable mixture of three large "bubble" groups centered at
#' \eqn{\nu_1=(0,0,0)}, \eqn{\nu_2=(6,0,0)} and \eqn{\nu_3=(0,6,0)} respectively. Each
#' bubble group \eqn{j} is simulated from a mixture of seven components according
#' to the following density distribution:
#'
#' \eqn{x\in\R^3\rightarrow 0.4\Phi(x|\mu_1+\nu_j,I_3)+\sum_{k=2}^70.1\Phi(x|\mu_k+\nu_j,0.1I_3)}
#'
#' with \eqn{\mu_1=(0,0,0)}, \eqn{\mu_2=(0,0,1.5)}, \eqn{\mu_3=(0,1.5,0)}, \eqn{\mu_4=(1.5,0,0,)},
#' \eqn{\mu_5=(0,0,-1.5)}, \eqn{\mu_6=(0,-1.5,0)} and \eqn{\mu_7=(-1.5,0,0,)}. Thus the
#' distribution of the dataset is actually a \eqn{21}-component Gaussian mixture.
#'
#' A model collection of spherical Gaussian mixtures is considered and the dataframe
#' \code{datacapushe} contains the maximum likelihood estimations for each of these models.
#' The number of free parameters of each model is used for the complexity values and \eqn{pen_{shape}}
#' is defined by this complexity divided by \eqn{n}.
#'
#' \code{datapartialcapushe} and \code{datavalidcapushe} can be used to run the
#' \code{\link[=validation]{validation}} function. \code{datapartialcapushe} only
#' contains the models with less than \eqn{21} components. \code{datavalidcapushe}
#' contains three models with \eqn{30}, \eqn{40} and \eqn{50} components respectively.
#'
#'
#' @references Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
#' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
#'
#' @examples
#' data(datacapushe)
#' capushe(datacapushe,n=1000)
#' ## BIC, DDSE and Djump all three select the true model
#' plot(capushe(datacapushe),newwindow=FALSE)
#' ## Validation:
#' data(datapartialcapushe)
#' capushepartial=capushe(datapartialcapushe)
#' data(datavalidcapushe)
#' validation(capushepartial,datavalidcapushe,newwindow=FALSE) ## The slope heuristics should not
#' ## be applied for datapartialcapushe.
#'
#' @keywords datasets
#' @aliases datacapushe
"datacapushe"

#' @title datapartialcapushe
#'
#' @description
#'
#' A dataframe example for the \code{\link[=capushe-package]{capushe package}} based on a simulated Gaussian
#' mixture dataset in \eqn{\R^3}.
#'
#' @usage data(datapartialcapushe)
#'
#' @format A data frame with 21 rows (models) and the following 4 variables:
#' \describe{
#'   \item{\code{model}}{a character vector: model names.}
#'   \item{\code{pen}}{a numeric vector: model penalty shape values.}
#'   \item{\code{complexity}}{a numeric vector: model complexity values.}
#'   \item{\code{contrast}}{a numeric vector: model contrast values.}
#' }
#'
#' @details
#' The simulated dataset is composed of \eqn{n=1000} observations in \eqn{\R^3}. It
#' consists of an equiprobable mixture of three large "bubble" groups centered at
#' \eqn{\nu_1=(0,0,0)}, \eqn{\nu_2=(6,0,0)} and \eqn{\nu_3=(0,6,0)} respectively. Each
#' bubble group \eqn{j} is simulated from a mixture of seven components according
#' to the following density distribution:
#'
#' \eqn{x\in\R^3\rightarrow 0.4\Phi(x|\mu_1+\nu_j,I_3)+\sum_{k=2}^70.1\Phi(x|\mu_k+\nu_j,0.1I_3)}
#'
#' with \eqn{\mu_1=(0,0,0)}, \eqn{\mu_2=(0,0,1.5)}, \eqn{\mu_3=(0,1.5,0)}, \eqn{\mu_4=(1.5,0,0,)},
#' \eqn{\mu_5=(0,0,-1.5)}, \eqn{\mu_6=(0,-1.5,0)} and \eqn{\mu_7=(-1.5,0,0,)}. Thus the
#' distribution of the dataset is actually a \eqn{21}-component Gaussian mixture.
#'
#' A model collection of spherical Gaussian mixtures is considered and the dataframe
#' \code{datacapushe} contains the maximum likelihood estimations for each of these models.
#' The number of free parameters of each model is used for the complexity values and \eqn{pen_{shape}}
#' is defined by this complexity divided by \eqn{n}.
#'
#' \code{datapartialcapushe} and \code{datavalidcapushe} can be used to run the
#' \code{\link[=validation]{validation}} function. \code{datapartialcapushe} only
#' contains the models with less than \eqn{21} components. \code{datavalidcapushe}
#' contains three models with \eqn{30}, \eqn{40} and \eqn{50} components respectively.
#'
#'
#' @references Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
#' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
#'
#' @examples
#' data(datacapushe)
#' capushe(datacapushe,n=1000)
#' ## BIC, DDSE and Djump all three select the true model
#' plot(capushe(datacapushe),newwindow=FALSE)
#' ## Validation:
#' data(datapartialcapushe)
#' capushepartial=capushe(datapartialcapushe)
#' data(datavalidcapushe)
#' validation(capushepartial,datavalidcapushe,newwindow=FALSE) ## The slope heuristics should not
#' ## be applied for datapartialcapushe.
#'
#' @keywords datasets
#' @aliases datapartialcapushe
"datapartialcapushe"

#' @title datavalidcapushe
#'
#' @description
#'
#' A dataframe example for the \code{\link[=capushe-package]{capushe package}} based on a simulated Gaussian
#' mixture dataset in \eqn{\R^3}.
#'
#' @usage data(datavalidcapushe)
#'
#' @format A data frame with 3 rows (models) and the following 4 variables:
#' \describe{
#'   \item{\code{model}}{a character vector: model names.}
#'   \item{\code{pen}}{a numeric vector: model penalty shape values.}
#'   \item{\code{complexity}}{a numeric vector: model complexity values.}
#'   \item{\code{contrast}}{a numeric vector: model contrast values.}
#' }
#'
#' @details
#' The simulated dataset is composed of \eqn{n=1000} observations in \eqn{\R^3}. It
#' consists of an equiprobable mixture of three large "bubble" groups centered at
#' \eqn{\nu_1=(0,0,0)}, \eqn{\nu_2=(6,0,0)} and \eqn{\nu_3=(0,6,0)} respectively. Each
#' bubble group \eqn{j} is simulated from a mixture of seven components according
#' to the following density distribution:
#'
#' \eqn{x\in\R^3\rightarrow 0.4\Phi(x|\mu_1+\nu_j,I_3)+\sum_{k=2}^70.1\Phi(x|\mu_k+\nu_j,0.1I_3)}
#'
#' with \eqn{\mu_1=(0,0,0)}, \eqn{\mu_2=(0,0,1.5)}, \eqn{\mu_3=(0,1.5,0)}, \eqn{\mu_4=(1.5,0,0,)},
#' \eqn{\mu_5=(0,0,-1.5)}, \eqn{\mu_6=(0,-1.5,0)} and \eqn{\mu_7=(-1.5,0,0,)}. Thus the
#' distribution of the dataset is actually a \eqn{21}-component Gaussian mixture.
#'
#' A model collection of spherical Gaussian mixtures is considered and the dataframe
#' \code{datacapushe} contains the maximum likelihood estimations for each of these models.
#' The number of free parameters of each model is used for the complexity values and \eqn{pen_{shape}}
#' is defined by this complexity divided by \eqn{n}.
#'
#' \code{datapartialcapushe} and \code{datavalidcapushe} can be used to run the
#' \code{\link[=validation]{validation}} function. \code{datapartialcapushe} only
#' contains the models with less than \eqn{21} components. \code{datavalidcapushe}
#' contains three models with \eqn{30}, \eqn{40} and \eqn{50} components respectively.
#'
#'
#' @references Article: Baudry, J.-P., Maugis, C. and Michel, B. (2011) Slope heuristics:
#' overview and implementation. \var{Statistics and Computing}, to appear. doi: 10.1007/s11222-011-9236-1
#'
#' @examples
#' data(datacapushe)
#' capushe(datacapushe,n=1000)
#' ## BIC, DDSE and Djump all three select the true model
#' plot(capushe(datacapushe),newwindow=FALSE)
#' ## Validation:
#' data(datapartialcapushe)
#' capushepartial=capushe(datapartialcapushe)
#' data(datavalidcapushe)
#' validation(capushepartial,datavalidcapushe,newwindow=FALSE) ## The slope heuristics should not
#' ## be applied for datapartialcapushe.
#'
#' @keywords datasets
#' @aliases datavalidcapushe
"datavalidcapushe"

