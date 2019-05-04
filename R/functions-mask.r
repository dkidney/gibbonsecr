
# mask construction ============================================================

#' @title Make a habitat mask
#' @description A wrapper function for \link[secr]{make.mask} (from the
#'   \pkg{\link[secr]{secr}} package) which returns a
#'   \link[gibbonsecr]{gmask} object.
#' @param traps a \link[secr]{traps}, \link[secr]{capthist} or
#'   \link[gibbonsecr]{gcapthist} object (traps must be \link[secr]{proximity}
#'   type)
#' @param buffer width of buffer (in metres)
#' @param spacing spacing between grid points (metres)
#' @param poly bounding polygon to which mask should be clipped (see
#'   documentation for \link[secr]{make.mask})
#' @param regular if \code{TRUE}, then all mask points will be taken from a
#'   single regular grid
#' @seealso \link[gibbonsecr]{add_covariates}, \link[gibbonsecr]{plot.gmask}
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export

make_mask = function(traps, buffer = 6000, spacing = 250, regular = TRUE, poly = NULL){
    if(inherits(traps, c("gcapthist", "capthist")))
        traps = traps(traps)
    if(!inherits(traps, "traps"))
        stop("'traps' object required")
    if(!all(unique(unlist(detector(traps))) %in% "proximity"))
        stop("'traps' must be 'proximity'")
    if(regular){
        # make a regionmask from regiontraps
        regiontraps = make_regiontraps(traps, ms = FALSE)
        regionmask = make.mask(traps = regiontraps, buffer = buffer,
                               spacing = spacing, type = "trapbuffer", poly = poly)
        # extract mask points for each session traps and save as multi-session
        mask = MS(lapply(traps, function(x){
            subset(regionmask,
                   apply(calc_distances(x, regionmask) <= buffer, 1, any))
        }), session.names = session(traps))
    }else{
        mask = make.mask(traps = traps, buffer = buffer, spacing = spacing,
                         type = "trapbuffer", poly = poly)
    }
    # add buffer and spacing as attributes
    attr(mask, "buffer")  = buffer
    attr(mask, "spacing") = spacing
    attr(mask, "area")    = attr(mask[[1]], "area") / 100
    # add gibbonsecr class
    class(mask) = c("gmask", "list", "mask")
    return(mask)
}


# Collapse a multi-session mask into a single session
make_regionmask = function(mask, ms = TRUE){
    if(!inherits(mask, "gmask"))
        stop("expecting a gmask object")
    # collapse mask points into a single data.frame
    regionmask = do.call(rbind, lapply(mask, as.data.frame))
    # set attributes
    class(regionmask) = c("mask", "data.frame")
    # re-attach covariates
    if(!is.null(covariates(mask))){
        covariates(regionmask) = do.call(rbind, covariates(mask))
    }
    # remove duplicate points
    regionmask = subset(regionmask, !duplicated(regionmask))
    # convert to multi-session
    if(ms){
        regionmask = MS(regionmask)
        class(regionmask) = class(mask)
    }
    # reset gmask attributes
    attr(regionmask, "buffer")  = attr(mask, "buffer")
    attr(regionmask, "spacing") = attr(mask, "spacing")
    attr(regionmask, "area")    = attr(mask, "area")
    # attr(regionmask, "meanSD")      = mask_meanSD(regionmask)
    # attr(regionmask, "boundingbox") = mask_bbox(regionmask)
    # attr(regionmask, "polygon")     = attr(mask[[1]], "polygon")
    return(regionmask)
}

#' @method na.omit gmask
#' @importFrom stats na.omit
na.omit.gmask = function(object, ...){
    submask = sapply(object, function(x){
        # check for missing coordinates
        missing.coords = is.na(x$x) | is.na(x$y)
        # check for missing coovariates
        missing.covs = if(is.null(covariates(x))){
            rep(FALSE, nrow(x))
        }else{
            apply(covariates(x), 1, anyNA)
        }
        delete = missing.coords | missing.covs
        if(any(delete))
            x = subset(x, !delete)
    }, simplify = FALSE)
    # re-set attributes
    class(submask) = class(object)
    attr(submask, "buffer")  = buffer(object)
    attr(submask, "spacing") = spacing(object)
    attr(submask, "area")    = area(object)
    return(submask)
}


# # Make a single-session mask to cover the area enclosed by a multi-session mask
# # Assumes input is a regular gmask
# # Impute any covariates from the multi-session mask
#
# make_regular_regionmask = function(mask, capthist = NULL){
#     if(!inherits(mask, "gmask"))
#         stop("expecting a gmask object")
#     # if(!inherits(capthist, "gcapthist"))
#         # stop("expecting a gcapthist object")
#     # if(!all(session(mask) == session(capthist)))
#         # stop("session(mask) not equal to session(capthist)")
#     regionmask = make_regionmask(mask)
#     # regiontraps = make_regiontraps(capthist)
#     # regular_regionmask = make_mask(
#     #     regiontraps,
#     #     buffer  = attr(regionmask, "buffer"),
#     #     spacing = attr(regionmask, "spacing")
#     # )
#     attr(regionmask, "class") = c("traps", "data.frame")
#     attr(regionmask, "detector") = "proximity"
#     regular_regionmask = make_mask(traps   = regionmask,
#                                    buffer  = spacing(mask),
#                                    spacing = spacing(mask))[[1]]
#     # str(regular_regionmask)
#     # plot(regular_regionmask, pch = 3, col = 2, cex = 0.1)
#     # spacing(regular_regionmask)
#     # gibbonsecr:::buffer(regular_regionmask)
#
#     # poly    = attr(regionmask, "polygon"))
#     if(!is.null(covariates(regionmask)))
#         regular_regionmask = add_covariates(regular_regionmask, regionmask)
#     return(regular_regionmask)
# }

# mask attributes ==============================================================

# @rdname mask_attributes
# @name buffer
# @title Extract mask attributes
# @description Extract attribute data from a gibbonsecr mask.
# @param object a \link[gibbonsecr]{gmask} object.
# @details \tabular{ll}{
#  \code{buffer}  \tab buffer distance around traps (in metres) \cr
#  \code{spacing} \tab spacing betweeen mask points (in metres) \cr
#  \code{size}    \tab number of mask points \cr
#  \code{area}    \tab area of each mask cell (in square kilometres) \cr
#  \code{Area}    \tab total mask area (in square kilometres) \cr
# }
# @param object a \link[gibbonsecr]{gmask} object
# @examples
# data(N.annamensis)
# buffer(N.annamensis.mask)
# spacing(N.annamensis.mask)
# size(N.annamensis.mask)
# area(N.annamensis.mask)
# Area(N.annamensis.mask)
# @export
buffer = function(object, ...){
    if(!inherits(object, "gmask"))
        stop("gmask object required")
    attr(object, "buffer")
}

# @rdname mask_attributes
# @name spacing
#' @method spacing gmask
#' @importFrom secr spacing
# @export
spacing.gmask = function(object, ...){
    if(!inherits(object, "gmask"))
        stop("gmask object required")
    attr(object, "spacing")
}

# @method covariates gmask
# @importFrom secr covariates
# @export
# covariates.gmask = function(object, ...){
#     lapply(object, function(x){
#         dplyr::tbl_df(covariates(x))
#     })
# }

# @rdname mask_attributes
# @name Area
# @export
Area = function(object){
    if(!inherits(object, "gmask"))
        stop("gmask object required")
    area(object) * size(object)
    # sapply(mask, function(x){
        # nrow(x) * attr(x, "area") / 100
    # })
}

# @rdname mask_attributes
# @name area
# @export
area = function(object, ...){
    if(!inherits(object, "gmask"))
        stop("gmask object required")
    area = sapply(object, function(x) attr(x, "area") / 100)
    if(!all(area == area[1]))
        stop("session masks have different cell areas")
    return(unname(area[1]))
}

# @rdname mask_attributes
# @name size
# @export
size = function(object){
    if(!inherits(object, "gmask"))
        stop("gmask object required")
    sapply(object, nrow)
}

# @method covariates gmask
# covariates.gmask = function(object, ...){
#     lapply(object, function(x){
#         out = as.data.frame(x)
#         cov = covariates(x)
#         if(!is.null(cov)) out = cbind(out, cov)
#         return(out)
#     })
# }


## ========================================================================== ##

## ========================================================================== ##

# mask_bbox = function(mask){
#     if(!inherits(mask, "mask")) stop("mask object required")
#     if(ms(mask)){
#         bbox = sapply(mask, mask_bbox, simplify = FALSE)
#     }else{
#         spacing = attr(mask, "spacing")
#         if(is.null(spacing)) stop("no spacing attribute")
#         x = range(mask$x) + c(-1, 1) * spacing / 2
#         y = range(mask$y) + c(-1, 1) * spacing / 2
#         bbox = data.frame(x = x[c(1,2,2,1)], y = y[c(1,1,2,2)])
#     }
#     return(bbox)
# }

# mask_clip = function(mask, poly){
#     if(!inherits(mask, "mask")) stop("mask object required")
#     for(i in 1:length(mask)){
#         keep = pointsInPolygon(mask[[i]], poly)
#         mask[[i]] = subset(mask[[i]], keep)
#     }
#     return(mask)
# }

# mask_meanSD = function(mask){
#     as.data.frame(apply(mask, 2, function(x) c(mean(x), sd(x))))
# }

# mask_na_rm = function(x){
#     if(!inherits(x, "mask")) stop("mask object required")
#     if(ms(x)){
#         x = sapply(x, mask_na_rm, simplify = FALSE)
#         class(x) = c("list","mask")
#     }else{
#         missing.coords = is.na(x$x) | is.na(x$y)
#         if(!is.null(covariates(x))){
#             missing.covs = apply(covariates(x), 1, anyNA)
#             delete = missing.coords | missing.covs
#         }else{
#             delete = missing.coords
#         }
#         if(any(delete))
#             x = subset(x, !delete)
#     }
#     return(x)
# }

# mask_spacing = function(mask){
#     if(!inherits(mask, "mask")) stop("mask object required")
#     if(!ms(mask)) mask = list(mask)
#
#     spacing = sapply(mask, function(x) attr(x, "spacing"))
#     #
#     #     spacing = sapply(mask, function(x){
#     #         xs = sort(unique(x$x))
#     #         ys = sort(unique(x$y))
#     #         min(min(xs[-1] - xs[-length(xs)]), min(ys[-1] - ys[-length(ys)]))
#     #     })
#     if(all(spacing == spacing[1])) spacing = unname(spacing[1])
#     return(spacing)
# }

# convert mask and mask covariates to tbl format (nicer printing)
# mask_tbl = function(mask){
#     if(ms(mask)){
#         for(i in session(mask)){
#             mask[[i]] = mask_tbl(mask[[i]])
#         }
#     }else{
#         head(attr(mask, "row.names"))
#         row.names = rownames(mask)
#         mask = dplyr::tbl_df(mask)
#         class(mask) = c(class(mask), "mask")
#         head(attr(mask, "row.names"))
#         attr(mask, "row.names") = row.names
#         head(attr(mask, "row.names"))
#         if(is.null(covariates(mask))){
#             temp = dplyr::tbl_df(data.frame())
#             attr(temp, "dim") = c(nrow(mask), 0)
#             attr(temp, "row.names") = row.names
#             attr(mask, "covariates") = temp
#         }else{
#             attr(mask, "covariates") = dplyr::tbl_df(covariates(mask))
#         }
#     }
#     return(mask)
# }

# mask_type = function(mask){
#     if(!inherits(mask, "mask")) stop("mask object required")
#     if(!ms(mask)) mask = list(mask)
#     type = sapply(mask, function(x){
#         attr(x, "type")
#     })
#     if(all(type == type[1])) type = unname(type[1])
#     return(type)
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# extract mask covarates including x and y coordinates

# maskcov = function(mask){
#     if(!inherits(mask, "mask")) stop("mask object required")
#     if(ms(mask)){
#         lapply(mask, maskcov)
#     }else{
#         covariates(mask)
#         if(is.null(covariates(mask))){
#             as.data.frame(mask)
#         }else{
#             cbind(as.data.frame(mask), covariates(mask))
#         }
#     }
# }
