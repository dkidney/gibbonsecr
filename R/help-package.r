
#' @name gibbonsecr-package
#' @aliases gibbonsecr
#' @docType package
#' @title SECR for acoustic gibbon surveys
#' @description An implementation of Spatially Explicit Capture-Recapture (SECR) for
#' estimating the density of gibbon groups from acoustic surveys.
#'
#' This package also contains a user interface which can be accessed using the
#' \code{\link[gibbonsecr]{gui}} function. See the online manual for more details
#' \url{http://dkidney.github.io/gibbonsecr}.
#'
#' \pkg{gibbonsecr} relies on several functions from the \pkg{\link[secr]{secr}} package
#' and uses an analagous system of objects. Model fitting is performed in a similar way
#' using a fine grid of points called a mask to approximate the spatial integration
#' component of the SECR likelihood. The defintion of 'trap' and 'session' are also
#' consistent with \pkg{secr}. However \pkg{gibbonsecr} differs from \pkg{secr} in the
#' following respects:
#' \enumerate{
#'  \item Supplementary information on detected animals in the form of estimated
#'  bearings and estimated distances can be incorporated in addition to the recapture
#'  data to improve precision in the density estimate.
#'  \item Stochastic availability of animals can be modelled using the parameter
#'  \code{pcall}. In general, this represents the proportion of animals being availablity
#'  for detection on each survey occasion. For gibbons, it can be interpreted as the
#'  proportion of groups that call on a given survey day.
#'  \item Trap data objects associated with capture history objects are always
#'  of \code{type} \code{\link[secr]{proximity}}.
#'  \item \strong{no group cov}.
#'  \item \strong{session cov and timecov attributes}.
#' }
#' The main functions in \pkg{gibbonsecr} are:
#' \tabular{ll}{
#'  \code{\link[gibbonsecr]{import_data}} \tab importing survey data, listening post
#'  locations and non-spatial covariates \cr
#'  \code{\link[gibbonsecr]{import_shp}}  \tab importing GIS shapefiles (\code{.shp}) \cr
#'  \code{\link[gibbonsecr]{make_mask}}   \tab constructing mask objects \cr
#'  \code{\link[gibbonsecr]{gfit}}        \tab fitting models \cr
#' }
#' See \code{\link[gibbonsecr]{N.annamensis}} for an example data set from a survey of
#' \emph{Nomascus annamensis} in northwestern Cambodia.
#' @seealso \link[gibbonsecr]{N.annamensis}, \link[gibbonsecr]{import_data},
#'   \link[gibbonsecr]{gfit}
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @references
#'   Kidney D., Rawson B.M., Borchers D.L., Stevenson B.C., Marques T.A., & Thomas L.
#'   2016. An Efficient Acoustic Density Estimation Method with Human Detectors Applied
#'   to Gibbons in Cambodia. \emph{PLoS ONE} 11(5): e0155066.
#'
#'   Borchers, D. L., Stevenson, B. C., Kidney, D., Thomas, L., & Marques, T. A. 2015. A
#'   Unifying Model for Capture-Recapture and Distance Sampling Surveys of Wildlife
#'   Populations. \emph{Journal of the American Statistical Association}, 110(509),
#'   195-204.
# @section Acknowledgements: sdfasdfafd
# @section Issues to resolve:
# \strong{PRIORITY}
#  \itemize{
#   \item \strong{predict - how to deal with centred covariates}
#   \item \strong{predict - check bounds of newdata}
#   \item \strong{example-N.annamensis}
#   \item \strong{documentation - gcapthist list attributes}
#   \item \strong{gui - tk2tips - use stringr::str_wrap}
#   \item \strong{gui - update screenshots}
#   \item \strong{gui - add region to plots}
#   \item \strong{testing - susan cheynes data}
#   \item \strong{print.summary.gcapthist}
#   \item \strong{print.summary.gfit}
#  }
# \strong{general}
#  \itemize{
#   \item gfit - calculate ESA so it isn't done every time in summary()
#   \item summary should invisible return a list and have gmask_summary class
#   and print.gmask_summary
#   \item make MS a generic function?
#   \item print.mask to print using tbl_df
#   \item clean up delta method function(s) - see r bloggers article: "Delta
#   Method Confidence Bands for Gaussian Density"
#   \item try using packrat
#  }
# \strong{plotting}
#  \itemize{
#   \item if density model covariates are all character then consider allowing
#   ggplot method to use density = factor. Alternatively, have a 'factor =
#   TRUE' argument to force density to be a factor.
#  }
# \strong{documentation}
#  \itemize{
#   \item add more detail to classes help page
#   \item update plosone reference if/when it's accepted
# }
# \strong{Future developments}
#  \itemize{
#   \item simulate functions
#   \item interval estimates for bearings and distances
#   \item function to convert multi-occasion survey data to independent occasions
#   \item plots for GAM models
#   \item option to use known locations
# }
#' @importFrom Rcpp sourceCpp
# @import dplyr
#' @import ggplot2
#' @import maptools
#' @import methods
#' @import secr
#' @useDynLib gibbonsecr

NULL
