
#' @rdname classes
#' @name gibbonsecr-classes
#' @aliases gcapthist
#' @title gibbonsecr object classes
#' @description Object classes returned from \pkg{gibbonsecr} functions.
#' @section gcapthist:
#'   Returned from \link[gibbonsecr]{import_data} and based on the
#'   \link[secr]{capthist} class from the \pkg{\link[secr]{secr}} package.
#'   \code{gcapthist} objects are always multi-session and each capthist element
#'   has \code{bearings} and/or \code{distances} attributes which are equal in
#'   dimension to the recapture data and contain the estimated
#'   bearings/distances (if these data are not present then these attributes are
#'   set to \code{NULL}). Covariates derived from the input survey data are
#'   stored within the \code{gcapthist} objects as additional attributes.
# @seealso \link[gibbonsecr]{import_data}

NULL

#' @rdname classes
#' @name gmask
#' @section gmask:
#'   Returned from \link[gibbonsecr]{make_mask} and based on the
#'   \link[secr]{mask} class from the \pkg{\link[secr]{secr}} package.
#'   \code{gmask} objects are always multi-session \code{"trapbuffer"} masks and
#'   have \code{buffer} and \code{spacing} attributes.
# @seealso \link[gibbonsecr]{make_mask}

NULL

#' @rdname classes
#' @name gshp
#' @section gshp:
#'   Returned from \link[gibbonsecr]{import_shp}. \code{gshp} objects are lists
#'   with two elements: \code{sp} which contains an object of class
#'   \link[sp]{sp}, \code{fortify} which contains an object returned from the
#'   \link[ggplot2]{fortify.sp} function from the \pkg{ggplot2} package, applied
#'   to the sp object (this is used by the \link[gibbonsecr]{geom_shp}
#'   function).
# @seealso \link[gibbonsecr]{import_shp}

NULL

#' @rdname classes
#' @name gsecr
#' @section gsecr:
#'   Returned from \link[gibbonsecr]{gfit}. \code{gsecr} objectscontain
#'   information on the model fit such as paramter estimates.
# @seealso \link[gibbonsecr]{gfit}

NULL

