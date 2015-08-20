## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## gibbonsecr methods functions

# add_bearings
# add_distances

# AIC.gibbonsecr_fit

# calc_bearings
# calc_bearings.default
# calc_bearings.traps
# calc_distances
# calc_distances.default
# calc_distances.traps

# coef.gibbonsecr_fit
# confint.gibbonsecr_fit

# get_bearings
# get_captures
# get_distances

# logLik.gibbonsecr_fit

# mask_Area
# mask_area
# mask_buffer
# mask_npoints
# mask_spacing

# MS
# MS

# n_arrays
# n_groups
# n_occasions
# n_traps

# plot.gibbonsecr_fit
# predict.gibbonsecr_fit
# print.gibbonsecr_fit
# simulate.gibbonsecr_fit
# summary.gibbonsecr_fit
# vcov.gibbonsecr_fit

## not all of these functions are strictly S3 methods, but they all act on specific object classes

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# add bearings to a capthist object

#' @rdname add_bearings_distances
#' @name add_bearings
#' @aliases add_bearings
#' @aliases add_bearings<-
#' @aliases add_distances
#' @aliases add_distances<-
#' @title Add estimated bearings or distances data to a capthist object
#' @description TODO
#' @param capthist a \code{\link{capthist}} object
#' @param value estimated bearings or distances data
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export
add_bearings = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    # check bearings argument
    attr(capthist, "bearings") = value
    return(capthist)
}

#' @rdname add_bearings_distances
#' @name add_distances
#' @export
add_distances = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    # check distances argument
    attr(capthist, "distances") = value
    return(capthist)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @inheritParams coef.gibbonsecr_fit
#' @param k the penalty per parameter to be used (defaults to k = 2)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method AIC gibbonsecr_fit
#' @export
AIC.gibbonsecr_fit = function(object, ..., k = 2){
    - 2 * logLik(object) + k * length(coef(object)) 
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @rdname calc_bearings_distances
#' @name calc_bearings 
#' @aliases calc_bearings.default
#' @aliases calc_bearings.traps
#' @aliases calc_distances
#' @aliases calc_distances.default
#' @aliases calc_distances.traps
#' @title Calculate bearings or distances between two sets of points
#' @description Calculate bearings or distances from points in \code{x} to
#'   points in \code{y}.
#' @param x a matrix, data.frame or \code{\link{traps}} object
#' @param y a matrix, data.frame or \code{\link{mask}} object
#' @param ... additional arguments (not used)
#' @details These functions are wrappers to non-exported functions written using
#'   \code{\link{Rcpp}}. (The underlying Rcpp functions, which take matrix 
#'   inputs and have no error checks, can be accessed with 
#'   \code{gibbonsecr:::calc_bearings_rcpp} and
#'   \code{gibbonsecr:::calc_distances_rcpp}.)
#'   
#' \code{.default} 
#' \itemize{ 
#'   \item \code{x} and \code{y} must be two-column matrices or data.frames.
#' }
#' \code{.traps}
#' \itemize{ 
#'   \item \code{x} must be a \code{\link{traps}} object and \code{y} must be a
#'   \code{\link{mask}} object.
#' }
#' @return A matrix, with columns representing points in \code{x} and rows 
#'   representing points in \code{y}. \code{calc_bearings} returns a matrix of 
#'   radians and \code{calc_distances} returns a matrix of distances in the same
#'   units as \code{x} and \code{y} (\code{x} and \code{y} must use the same 
#'   units). In the case of \code{.traps} functions, if \code{x} or \code{y} are
#'   multi-session, then a list of matrices is returned with one element per
#'   session.
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link{add_bearings}, \link{add_distances}, \link{get_bearings},
#'   \link{get_captures}, \link{get_distances}
#' @examples
#' ## Example 1: from a single point to all points in a matrix
#' A = matrix(0,1,2)
#' Z = matrix(c(0,1,1,1,1,0), 3, 2)
#' plot(Z, pch = 19, col = 2, asp = 1)
#' points(A, pch = 19, col = 4)
#' calc_distances(A, Z)
#' calc_bearings(A, Z)
#' 
#' ## Example 2: from a trap to all points in a mask
#' data(N.annamensis)
#' traps = secr::traps(N.annamensis)[[1]]
#' mask = secr::make.mask(traps, buffer = 3000, spacing = 100)
#' 
#' distances = calc_distances(traps, mask)
#' bearings = calc_bearings(traps, mask)
#' 
#' # plot results for trap 1
#' x = unique(mask$x)
#' y = unique(mask$y)
#' 
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,2))
#' 
#' z = matrix(distances[,1], length(x), length(y))
#' image(x, y, z, col = heat.colors(100), asp = 1, main = "Distances")
#' points(traps, pch = 19)
#' points(traps[1,], pch = 19, col = 4)
#' 
#' z = matrix(bearings[,1], length(x), length(y))
#' image(x, y, z, col = heat.colors(100), asp = 1, main = "Distances")
#' points(traps, pch = 19)
#' points(traps[1,], pch = 19, col = 4)
#' 
#' par(op)
#' @export
calc_bearings = function(x, y, ...){
    UseMethod("calc_bearings")
}

#' @rdname calc_bearings_distances
#' @name calc_bearings.default 
#' @export
calc_bearings.default = function(x, y, ...){
    warning.text = "x and y must be matrices or data.frames"
    if(!inherits(x, c("matrix", "data.frame")) || !inherits(y, c("matrix", "data.frame")))
        stop(warning.text)
    if(ncol(x) != 2 || ncol(y) != 2)
        stop(warning.text)
    calc_bearings_rcpp(as.matrix(x), as.matrix(y))
}

#' @rdname calc_bearings_distances
#' @name calc_bearings.traps 
#' @method calc_bearings traps
#' @export
calc_bearings.traps = function(x, y, ...){
    if(!inherits(y, "mask")) 
        stop("y must be a 'mask' object")
    if(ms(x)){
        session.names = session(x)
        if(ms(y)){
            if(any(session(y) != session.names) || ms(x) != ms(y))
                stop("unequal number of sessions in traps and mask")
        }
    }else{
        if(ms(y)){
            session.names = session(y)
        }else{
            session.names = session(x)
        }
    }
    nsessions = length(session.names)
    bearings = setNames(lapply(1:nsessions, function(i){
        calc_bearings_rcpp(
            as.matrix(if(ms(x)) x[[i]] else x),
            as.matrix(if(ms(y)) y[[i]] else y)
        )
    }), session.names)
    if(!ms(x) && !ms(y)) bearings = bearings[[1]]
    return(bearings)
}

#' @rdname calc_bearings_distances
#' @name calc_distances
#' @export
calc_distances = function(x, y, ...){
    UseMethod("calc_distances")
}

#' @rdname calc_bearings_distances
#' @name calc_distances.default
#' @export
calc_distances.default = function(x, y, ...){
    warning.text = "x and y must be matrices or data.frames"
    if(!inherits(x, c("matrix", "data.frame")) || !inherits(y, c("matrix", "data.frame")))
        stop(warning.text)
    if(ncol(x) != 2 || ncol(y) != 2)
        stop(warning.text)
    calc_distances_rcpp(x, y)
}

#' @rdname calc_bearings_distances
#' @name calc_distances.traps 
#' @method calc_distances traps
#' @export
calc_distances.traps = function(x, y, ...){
    if(!inherits(y, "mask")) 
        stop("y must be a 'mask' object")
    if(ms(x)){
        session.names = session(x)
        if(ms(y)){
            if(any(session(y) != session.names) || ms(x) != ms(y))
                stop("unequal number of sessions in traps and mask")
        }
    }else{
        if(ms(y)){
            session.names = session(y)
        }else{
            session.names = session(x)
        }
    }
    nsessions = length(session.names)
    distances = setNames(lapply(1:nsessions, function(i){
        calc_distances_rcpp(
            as.matrix(if(ms(x)) x[[i]] else x),
            as.matrix(if(ms(y)) y[[i]] else y)
        )
    }), session.names)
    if(!ms(x) && !ms(y)) distances = distances[[1]]
    return(distances)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @param object a \code{gibbonsecr_fit} model object (i.e. an object returned from the \code{\link{gibbonsecr_fit}} function)
#' @param ... additional arguments (not used)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method coef gibbonsecr_fit
#' @export
coef.gibbonsecr_fit = function(object, ...){
    object$nlm$estimate
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @inheritParams coef.gibbonsecr_fit
#' @param parm parameters (see documentation for the \code{\link{confint}} generic)
#' @param level the confidence level
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method confint gibbonsecr_fit
#' @export
confint.gibbonsecr_fit = function(object, parm, level = 0.95, ...){
    confint.default(object, parm, level, ...)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

fitted_detectfn_auxiliary_values = function(beta, fit, session, x, which = "detectfn"){
    
    if(!which %in% c("detectfn","bearings","distances"))
        stop("which must be one of: 'detectfn', 'bearings', 'distances'")
    
    ##################################################
    ## submodels
    
    submodels = if(which == "detectfn"){
        switch(fit$model.options$detectfn + 1, c("g0","sigma"), c("g0","sigma","z"))
    }else which
    # check model exists
    for(submodel in submodels){
        if(fit$model[[submodel]] == 0) stop("no ", submodel, " model to plot")
    }
    
    ##################################################
    ## function to generate values
    
    FUN = switch(
        which,
        "detectfn"  = list(hn, hr)[[fit$model.options$detectfn + 1]],
        "bearings"  = list(dvm, dwrpcauchy)[[fit$model.options$bearings]],
        "distances" = list(dgamma, dlnorm)[[fit$model.options$distances]]
    )
    EX = switch(which, "bearings" = 0, "distances" = 1000, NULL)
    
    ##################################################
    ## design matrices
    
    design.matrices = sapply(submodels, function(submodel){ # submodel = "sigma"
        covs = all.vars(fit$model[[submodel]]) # covs
        if(length(covs) == 0){
            model.frame = data.frame(matrix(0, nrow = 1, ncol = 0))
        }else{
            # all covariates (from all sessions)
            model.frame = do.call(rbind, lapply(fit$model.frames, function(x) x[[submodel]])) # head(model.frame)
            # extract reference level (factors) or mean (numeric) for each cov
            model.frame = do.call(cbind, sapply(covs, function(cov){ # cov = covs[1] ; cov
                out = list()
                out[[cov]] = if(inherits(model.frame[[cov]], "factor")){
                    levs = levels(model.frame[[cov]])
                    factor(levs[1], levels = levs)
                }else{
                    mean(model.frame[,cov])
                }
                as.data.frame(out)
            }, simplify = FALSE)) # str(model.frame)
        }
        make_model_matrix(fit$model[[submodel]], model.frame, fit$smooth.setup[[submodel]]) 
    }, simplify = FALSE)
    # get submodel arrays - each array will be a 1 by 1 matrix
    design.matrices = list("1" = design.matrices)
    
    ##################################################
    ## convert to submodel arrays and return function values
    
    submodel.arrays = make_submodel_arrays2(beta = beta, parindx = fit$parindx, fixed = fit$fixed, design.matrices, fit$inv.link, S = c("1" = 1), K = c("1" = 1))[[1]]
    par = as.numeric(submodel.arrays)
    FUN(x, par, EX)
    
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

fitted_surface_values = function(beta, fit, session, mask, traps, which = "pdot"){ # beta=coef(fit); session="2"; which = "density"
    
    if(!which %in% c("pdot","density"))
        stop("which must be one of: 'pdot', 'density")

    ##################################################
    ## submodels
    
    submodels = switch(
        which,
        "density" = "D",
        "pdot"    = switch(
            fit$model.options$detectfn + 1, 
            c("g0","sigma","pcall"), 
            c("g0","sigma","z","pcall"))
    )
    
    ##################################################
    ## design matrices
    
    if(which == "density"){    
        model.frame = make_model_frames(
            model       = fit$model,
            traps       = traps,
            mask        = mask,
            n_occasions = n_occasions(fit$capthist),
            sessioncov  = sessioncov(fit$capthist),
            sessions    = session,
            submodels   = submodels
        )
        design.matrices = make_design_matrices(
            model        = fit$model, 
            model.frame  = model.frame, 
            smooth.setup = fit$smooth.setup,
            sessions     = session,
            submodels    = submodels
        )
    }
    if(which == "pdot"){
        design.matrices = fit$design.matrices[session]
    }
    
    ##################################################
    ## return values
    
    M = mask_npoints(fit$mask)
    S = n_occasions(fit$capthist)
    K = n_traps(fit$capthist)
    submodel.arrays = make_submodel_arrays(
        beta            = beta, 
        par.labels      = fit$par.labels, 
        fixed           = fit$fixed, 
        design.matrices = design.matrices, 
        inv.link        = fit$inv.link, 
        S               = S, 
        K               = K,
        sessions        = session,
        submodels       = submodels
    )
    if(which == "pdot"){
        values = calc_pdot(
            detectfn  = fit$model.options$detectfn,
            g0        = submodel.arrays[[session]][["g0"]], 
            sigma     = submodel.arrays[[session]][["sigma"]], 
            z         = submodel.arrays[[session]][["z"]], 
            pcall     = submodel.arrays[[session]][["pcall"]], 
            distances = fit$mask.info[[session]][["distances"]], 
            usage     = usage(traps[[session]]), 
            M         = M[session], 
            S         = S[session], 
            K         = K[session]
        )
    }
    if(which == "density"){
        values = as.numeric(submodel.arrays[[session]][["D"]])
    }
    return(values)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

fitted_pdot_values = function(beta, fit, session = 1){ # beta = coef(fit)
    
    submodels = switch(fit$model.options$detectfn + 1, 
                       c("g0","sigma","pcall"), 
                       c("g0","sigma","z","pcall"))
    
    S = n_occasions(fit$capthist)[session]
    K = n_traps(fit$capthist)[session]
    M = mask_npoints(fit$mask)[session]
    
    submodel.arrays = make_submodel_arrays(
        beta            = beta, 
        par.labels      = fit$par.labels, 
        fixed           = fit$fixed, 
        design.matrices = fit$design.matrices[session], 
        inv.link        = fit$inv.link, 
        S               = S, 
        K               = K,
        submodels       = submodels
    )[[session]]
    
    calc_pdot(
        detectfn  = fit$model.options$detectfn,
        g0        = submodel.arrays[["g0"]], 
        sigma     = submodel.arrays[["sigma"]], 
        z         = submodel.arrays[["z"]], 
        pcall     = submodel.arrays[["pcall"]], 
        distances = fit$mask.info[[session]][["distances"]], 
        usage     = usage(traps(fit$capthist[[session]])), 
        M         = M, 
        S         = S, 
        K         = K
    )
    
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

covlevels = function(x){
    covlevels = list()
    if(inherits(x, "capthist")){
        SS = if(ms(x)) x[[1]] else x
        covlevels$groupcov   = colnames(covariates(SS))
        covlevels$sessioncov = colnames(attr(x, "sessioncov")) 
        covlevels$timecov    = colnames(attr(SS, "timecov"))
        covlevels$trapcov    = c("x", "y", unique(colnames(covariates(traps(SS)))))
        if(!is.null(covlevels$trapcov)){
            covlevels$timevaryingcov = names(timevaryingcov(traps(SS)))
            if(!is.null(covlevels$timevaryingcov)){
                keep = !covlevels$trapcov %in% covlevels$timevaryingcov
                covlevels$trapcov = covlevels$trapcov[keep]
            }
        }
    }
    if(inherits(x, "mask")){
        if(ms(x)) x = x[[1]]
        covlevels$maskcov = sort(c("x", "y", colnames(covariates(x))))
    }
    covlevels = if(length(covlevels) == 0) NULL else{
        lapply(covlevels, sort)
    }
    return(covlevels)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @rdname get_bearings_captures_distances
#' @name get_bearings
#' @title Extract data from capthist
#' @description TODO
#' @details TODO
#' @param capthist a \code{\link{capthist}} object
#' @param summarise choose whether to collapse capture data across traps or occasions (see \bold{Details})
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export
get_bearings = function(capthist){
    if(ms(capthist)){
        bearings = lapply(capthist, function(x){
            attr(x, "bearings")
        })
        if(all(sapply(bearings, is.null)))
            bearings = NULL
    }else{
        bearings = attr(capthist, "bearings")
    }
    return(bearings)
}

#' @rdname get_bearings_captures_distances
#' @name get_distances
#' @export
get_distances = function(capthist){
    if(ms(capthist)){
        distances = lapply(capthist, function(x){
            distances = attr(x, "distances")
        })
        if(all(sapply(distances, is.null)))
            distances = NULL
    }else{
        distances = attr(capthist, "distances")
    }
    return(distances)
}

#' @rdname get_bearings_captures_distances
#' @name get_captures
#' @export
get_captures = function(capthist, summarise = NULL){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    # convert to list so lapply can be used regardless of ms
    captures = if(ms(capthist)) capthist else list(capthist)
    # extract capture history data 
    captures = lapply(captures, function(x){
        array(as.numeric(x), dim(x), dimnames(x))
    })
    if(!is.null(summarise)){
        # summarise across occasions - makes an [n,S] matrix
        if(summarise == "occasions"){
            captures = lapply(captures, function(x){
                apply(x, c(1,2), function(x) 1 * any(x == 1))
            })
        }
        # summarise across traps - makes an [n,K] matrix
        if(summarise == "traps"){
            captures = lapply(captures, function(x){
                apply(x, c(1,3), function(x) 1 * any(x == 1))
            })
        }
        if(!summarise %in% c("traps","occasions"))
            stop("'summarise' must be NULL, 'occasions' or 'traps'")
    }
    if(!ms(capthist)) captures = captures[[1]]
    return(captures)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

get_esa = function(fit){
    if(!inherits(fit, "gibbonsecr_fit"))
        stop("requires a gibbonsecr_fit object")
    esa = calc_esa(
        beta            = coef(fit),
        detectfn        = fit$model.options$detectfn, 
        par.labels      = fit$par.labels, 
        fixed           = fit$fixed, 
        design.matrices = fit$design.matrices, 
        distances       = calc_distances(traps(fit$capthist), fit$mask), 
        usage           = usage(traps(fit$capthist)),
        inv.link        = fit$inv.link, 
        S               = n_occasions(fit$capthist), 
        K               = n_traps(fit$capthist), 
        M               = mask_npoints(fit$mask), 
        a               = mask_area(fit$mask)
    )
    return(esa)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @inheritParams coef.gibbonsecr_fit
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method logLik gibbonsecr_fit
#' @export
logLik.gibbonsecr_fit = function(object, ...){
    -object$nlm$minimum
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# functions to extract mask attributes
# - mask_Area - returns the total area in square km
# - mask_area - returns the area of single mask grid cell in square km
# - mask_npoints - returns the number of mask points
# - mask_buffer - returns the buffer size in m used to generate the mask
# if mask is single session, they return a scalar
# if mask is multi-session, they returns a vector (with length equal to the number of sessions)

mask_Area = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(!ms(mask)) mask = list(mask)
    sapply(mask, function(x){
        nrow(x) * attr(x, "area") / 100
    })
}

mask_area = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(!ms(mask)) mask = list(mask)
    sapply(mask, function(x){
        attr(x, "area") / 100
    })
}

mask_bbox = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(!ms(mask)) mask = list(mask)
    bbox = apply(do.call(rbind, lapply(mask, function(x){
        apply(attr(x, "boundingbox"), 2, range)
    })), 2, range)
    bbox = as.matrix(expand.grid(x = bbox[,1], y = bbox[,2]))
    return(bbox)
}

mask_buffer = function(mask, traps){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(inherits(traps, "capthist")) traps = traps(traps)
    if(!inherits(traps, "traps")) stop("traps object required")
    mask = MS(mask)
    traps = MS(traps)
    if(length(mask) != length(traps))
        stop("mask and traps have different number of sessions")
    buffer = setNames(sapply(1:length(mask), function(i){
        min(traps[[i]]$x) - min(attr(mask[[i]], "boundingbox")$x)
    }), names(mask))
    if(all(buffer == buffer[1])) buffer = unname(buffer[1])
    return(buffer)
}

mask_clip = function(mask, poly){
    if(!inherits(mask, "mask")) stop("mask object required")
    for(i in 1:length(mask)){
        keep = pointsInPolygon(mask[[i]], poly)
        mask[[i]] = subset_mask(mask[[i]], keep)
    }
    return(mask)
}

mask_image = function(mask, values, plot = TRUE){
    l = mask_spacing(mask)
    x = unique(mask$x)
    x = seq(min(x), max(x), l)
    y = unique(mask$y)
    y = seq(min(y), max(y), l)
    z = matrix(NA, nrow = length(x), ncol = length(y))
    i = cbind(match(mask$x, x), match(mask$y, y))
    z[i] = values
    if(plot) image(x, y, z, asp = 1)
    invisible(list(x = x, y = y, z = z))
}

mask_npoints = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(!ms(mask)) mask = list(mask)
    sapply(mask, function(x){
        nrow(x)
    })
}

mask_rbind = function(mask){
    if(!inherits(mask, "mask")) stop("expecting a mask object")
    if(!ms(mask)){
        message("can't rbind single-session masks")    
        return(mask)
    }
    # check types
    types = sapply(mask, mask_type)
    if(!all(types == types[1]))
        stop("can't rbind masks of different types")
    # check spacing
    spacings = sapply(mask, mask_spacing)
    if(!all(spacings == spacings[1]))
        stop("can't rbind masks with different spacings")
    # combine coordinates        
    regionmask = do.call(rbind, lapply(mask, as.data.frame))
    # add attributes
    class(regionmask) = c("mask","data.frame")
    covariates(regionmask) = do.call(rbind, lapply(mask, covariates))
    attr(regionmask, "meanSD") = as.data.frame(rbind(
        apply(regionmask, 2, mean),       
        apply(regionmask, 2, sd)        
    ))
    lims = apply(do.call(rbind, lapply(mask, mask_bbox)), 2, range)
    attr(regionmask, "boundingbox") = data.frame(
        x = lims[c(1,2,2,1),1],
        y = lims[c(1,1,2,2),2]
    )
    return(regionmask)
}

mask_spacing = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(!ms(mask)) mask = list(mask)
    
    spacing = sapply(mask, function(x) attr(x, "spacing"))
    #     
    #     spacing = sapply(mask, function(x){
    #         xs = sort(unique(x$x))
    #         ys = sort(unique(x$y))
    #         min(min(xs[-1] - xs[-length(xs)]), min(ys[-1] - ys[-length(ys)]))
    #     })
    if(all(spacing == spacing[1])) spacing = unname(spacing[1])
    return(spacing)
}

mask_type = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(!ms(mask)) mask = list(mask)
    type = sapply(mask, function(x){
        attr(x, "type")
    })
    if(all(type == type[1])) type = unname(type[1])
    return(type)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

MS = function(...){
    MS = list(...)
    classes = c("capthist", "mask", "traps")
    # if dots is a ms capthist/traps/mask then return unchanged
    if(length(MS) == 1 && inherits(MS[[1]], "list") && inherits(MS[[1]], classes)){
        return(MS[[1]])
    }
    # if dots is a list of single session objects then remove outer list from MS
    if(length(MS) == 1 && inherits(MS[[1]], "list") && !inherits(MS[[1]], classes)){
        MS = MS[[1]]
    }
    # if dots is a series of single session objects then check all have same class
    classesOK = sapply(classes, function(i){
        all(sapply(MS, function(x) inherits(x, i)))
    })
    if(any(classesOK)){
        # if MS has more than one element then check session(MS)
        class(MS) = c("list", classes[classesOK])
        #         sessions = lapply(MS, function(x) attr(x, "session"))
        #         if(any(sapply(sessions, is.null))){
        #             stop("missing session attributes")
        #         }else{
        #             session(MS) = unlist(sessions)
        #         }
    }else{
        stop("objects not recognised")
    }
    return(MS)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# get number of arrays / groups / occasions / traps in capthist
# for groups, occasions and traps:
# - if capthist is not ms, returns a scalar
# - if capthist is ms, returns a vector

n_arrays = function(capthist){
    if(!inherits(capthist, "capthist")) stop("capthist object required")
    capthist = MS(capthist)
    return(length(capthist))
}

n_detections = function(capthist){
    if(!inherits(capthist, "capthist")) stop("capthist object required")
    if(!ms(capthist)) capthist = MS(capthist)
    sapply(capthist, function(x){
        if(is.null(dim(x))) 0 else sum(x)
    })
}

n_groups = function(capthist){
    if(!inherits(capthist, "capthist")) stop("capthist object required")
    if(!ms(capthist)) capthist = MS(capthist)
    sapply(capthist, function(x){
        if(is.null(dim(x))) 0 else dim(x)[1]
    })
}

n_occasions = function(capthist){
    if(!inherits(capthist, "capthist")) stop("capthist object required")
    if(!ms(capthist)) capthist = MS(capthist)
    sapply(capthist, function(x){ # x = capthist[["15"]]
        if(is.null(dim(x))) dim(usage(traps(x)))[2] else dim(x)[2]
    })
}

n_traps = function(capthist){
    if(!inherits(capthist, "capthist")) stop("capthist object required")
    if(!ms(capthist)) capthist = MS(capthist)
    sapply(capthist, function(x){
        if(is.null(dim(x))) dim(traps(x))[1] else dim(x)[3]
    })
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title TODO
# @description TODO
# @inheritParams coef.gibbonsecr_fit
# @param submodel TODO 
# @param se.fit Logical scalar indicating if standard errors are required.
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @seealso \link{gibbonsecr_fit}
# @method predict gibbonsecr_fit
# @export

# predict.gibbonsecr_fit = function(object, submodel = c("D","g0","sigma","z","pcall","bearings","distances","detectfn"), se.fit = FALSE, ...){
#     
#     preds = list()
#     
#     return(preds)
#     
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @param x a \code{gibbonsecr} model object (i.e. an object returned from the \code{\link{gibbonsecr_fit}} function)
#' @param ... additional arguments (not used)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method print gibbonsecr_fit
#' @export
print.gibbonsecr_fit = function(x, ...){
    summary(x)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# x = c(1000.12345678, 990.12345, 1000)
# width = 7

pretty_numbers = function(x, width){
    n = length(x)
    xchar = as.character(x)
    # add a decimal place if one doesn't already exist
    for(i in 1:n){ # i=3
        if(!grepl("\\.", xchar[i]))
            xchar[i] = paste0(xchar[i], ".0")
    }
    # count how many numbers before and after the decimal
    xsplit = strsplit(xchar, "\\.")
    nbefore = sapply(xsplit, function(x) nchar(x[1]))
    # after = sapply(xsplit, function(x) length(x) - match(".", x))
    # if width is less than max before, then increase width
    if(width < max(nbefore)) width = max(nbefore)
    # check number of decimals
    nafter = width - max(nbefore) - 1
    xchar = sapply(xsplit, function(x){ # x = xsplit[[1]]
        if(nchar(x[2]) < nafter){
            x[2] = paste(x[2], rep(" ", nafter - nchar(x[2])), sep = "")
        }
        if(nchar(x[2]) > nafter){
            x[2] = substr(x[2], 1, nafter)
        }
        paste(x, collapse = ".")
    })
    return(xchar)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @inheritParams coef.gibbonsecr_fit
#' @param nsim TODO
#' @param seed TODO
#' @param buffer TODO
#' @param spacing TODO
#' @param beta TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link{gibbonsecr_fit}
#' @method simulate gibbonsecr_fit
#' @importFrom stats simulate
#' @export

simulate.gibbonsecr_fit = function(object, nsim = 1, seed = NULL, buffer = 6000, spacing = 10, beta = NULL, ...){ 
    if(!is.null(seed)) set.seed(seed)
    if(is.null(beta)){
        beta = coef(object)  
    }else{
        if(is.null(names(beta))) names(beta) = names(coef(object))
    }
    simcapt = sim_capthist(
        beta          = beta, 
        traps         = traps(object$capthist), 
        model         = object$model, 
        model.options = object$model.options, 
        fixed         = object$fixed,
        n_occasions   = n_occasions(object$capthist), 
        n_sessions    = n_arrays(object$capthist), 
        session.names = session(object$capthist),
        sessioncov    = attr(object, "sessioncov"), 
        timecov       = attr(object, "timecov"), 
        smooth.setup  = object$smooth.setup, 
        buffer        = buffer, 
        spacing       = spacing,
        usage.prob    = NULL,
        seed          = seed
    )
    # traps = traps(object$capthist); model = object$model ; model.options = object$model.options ; n_occasions = n_occasions(object$capthist) ; n_sessions = n_arrays(object$capthist) ; session.names = session(object$capthist) ; sessioncov = attr(object, "sessioncov"); timecov = attr(object, "timecov"); smooth.setup  = object$smooth.setup; usage.prob = NULL
    return(simcapt)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# traps = traps(fit$capthist) ; model = fit$model; model.options = fit$model.options; smooth.setup = fit$smooth.setup; n_occasions = 2; seed = 1; spacing = 10 ; beta = c(c(log(2), 0, 0) + coef(fit), "g0.(Intercept)" = logit(0.75), "pcall.(Intercept)" = logit(0.5)) ; beta

sim_capthist = function(beta, traps, model, model.options, fixed, n_occasions = 1, n_sessions = 1, session.names = NULL, sessioncov = NULL, timecov = NULL, smooth.setup = NULL, buffer = 6000, spacing = 10, usage.prob = NULL, seed = NULL){
    
    ##################################################
    ## check inputs
    
    if(is.null(names(beta))) stop("beta needs to be a named vector of parameter values", call. = FALSE)
    if(is.null(session.names)){
        session.names = as.character(1:n_sessions)
    }else{
        if(length(session.names) != n_sessions) stop("length(session.names) must equal n_sessions", call. = FALSE)
    }
    if(!ms(traps)) stop("expecting ms traps", call. = FALSE)
    if(length(traps) != n_sessions) stop("length(traps) must equal n_sessions", call. = FALSE)
    n_traps = sapply(traps, nrow)
    if(!is.null(sessioncov)) if(nrow(sessioncov) != n_sessions) stop("nrow(sessioncov) should equal n_sessions", call. = FALSE)
    if(!is.null(timecov)){
        if(length(timecov) != n_sessions) stop("length(timecov) should equal n_sessions", call. = FALSE)
        if(!all(names(timecov) == session.names)) stop("!all(names(timecov) == session.names)")
    }
    if(length(n_occasions) == 1){
        n_occasions = setNames(rep(n_occasions, n_sessions), session.names)
    }else{
        if(length(n_occasions) != n_sessions)
            stop("length of n_occasions should equal 1 or n_sessions", call. = FALSE)
        names(n_occasions) = session.names
    }
    
    ##################################################
    ## make submodel arrays
    
    par.labels = cbind(
        submodel = sapply(strsplit(names(beta), "\\."), function(x) x[1]),
        term     = sapply(strsplit(names(beta), "\\."), function(x) x[2]),
        unique   = names(beta)
    )
    
    bigmask = make.mask(traps, buffer = buffer, spacing = spacing) # mask_npoints(bigmask)
    
    inv.link = get_inv_link(model, fixed = fixed, model.options)
    
    design.matrices = make_design_matrices(
        model        = model, 
        traps        = traps, 
        mask         = bigmask, 
        n_occasions  = n_occasions,
        session.names= session.names, 
        sessioncov   = sessioncov, 
        timecov      = timecov, 
        smooth.setup = smooth.setup
    )
    
    # names(design.matrices)
    
    submodel.arrays = make_submodel_arrays(
        beta            = beta,
        par.labels      = par.labels, 
        fixed           = fixed, 
        design.matrices = design.matrices, 
        inv.link        = inv.link, 
        S               = n_occasions, 
        K               = n_traps
    )
    # fixed = list(); S = n_occasions; K = sapply(traps, nrow)
    
    ##################################################
    ## make new capthist
    
    detectfn = list(hn, hr)[[model.options$detectfn + 1]]
    
    newcapt = sapply(1:n_sessions, function(session){ # session = 1
        
        temptraps = if(ms(traps)) traps[[session]] else traps
        tempmask  = if(ms(bigmask)) bigmask[[session]] else bigmask
        S         = n_occasions[session]
        K         = n_traps[session]
        M         = mask_npoints(tempmask)
        a         = mask_area(tempmask)
        
        ##################################################        
        ## trap usage
        
        # use usage probability if supplied
        # else use existing usage (if no existing usage then assume full usage)
        if(!is.null(usage.prob)){
            usage(temptraps) = matrix(rbinom(K * S, 1, usage.prob), nrow = K, ncol = S)
        }else{
            if(is.null(usage(temptraps)))
                usage(temptraps) = matrix(1, nrow = K, ncol = S)
        }
        
        ##################################################        
        ## population
        
        # get density values for all points on mask
        D.values = submodel.arrays[[session]][["D"]]
        # assume poisson and draw random population size
        N = rpois(1, sum(D.values) * a)
        # take sample of N mask points
        i = sample(x = 1:M, size = N, replace = TRUE, prob = D.values / sum(D.values))
        population = tempmask[i, , drop = FALSE] # head(population) ; dim(population)
        rownames(population) = 1:N
        
        ##################################################        
        ## capthist
        
        # calulate distances
        distances = calc_distances(temptraps, population) ; head(distances) ; dim(distances)
        # calulate detection probabilities
        usage = usage(temptraps)
        g0    = submodel.arrays[[session]][["g0"]] ; dim(g0)
        sigma = submodel.arrays[[session]][["sigma"]] ; dim(sigma)
        z     = submodel.arrays[[session]][["z"]] ; dim(z)
        detprob = calc_detprob(model.options$detectfn, g0, sigma, z, distances, usage, N, S, K)
        # generate capthist
        newcapt = array(rbinom(n = N * S * K, size = 1, prob = as.numeric(detprob)), dim = dim(detprob), 
                        dimnames = list(rownames(population), 1:S, rownames(temptraps)))
        # delete undetected groups
        class(newcapt) = "capthist"
        i = apply(get_captures(newcapt, summarise = "occasions"), 1, function(x) any(x == 1))
        newcapt   = newcapt[i, , , drop = FALSE]
        locations = population[i, , drop = FALSE]
        distances = distances [i, , drop = FALSE]
        n = sum(i)
        # simulate availability
        pcall = as.numeric(submodel.arrays[[session]][["pcall"]]) 
        for(i in 1:n){    
            available = rbinom(S, 1, pcall)
            # newcapt[i,,]
            newcapt[i, available == 0, ] = 0
            # newcapt[i,,]
        }
        # delete undetected groups one more time to take account of availability
        class(newcapt) = "capthist"
        i = apply(get_captures(newcapt, summarise = "occasions"), 1, function(x) any(x == 1))
        newcapt   = newcapt[i, , , drop = FALSE]
        locations = population[i, , drop = FALSE]
        distances = distances [i, , drop = FALSE]
        n = sum(i)
        
        ##################################################        
        ## bearings
        
        if(model.options$bearings != 0 && n > 0){
            b_par  = submodel.arrays[[session]][["bearings"]]
            b_mean = calc_bearings_rcpp(as.matrix(temptraps), as.matrix(locations))  
            b_est  = array(NA, dim = dim(newcapt))
            b_func = switch(model.options$bearings, 
                            CircStats::rvm, 
                            CircStats::rwrpcauchy
            )
            for(k in 1:K){
                for(s in 1:S){
                    for(i in 1:n){
                        if(newcapt[i,s,k] == 1){
                            b_est[i,s,k] = b_func(1, b_mean[i,k], b_par[s,k]) 
                        }
                    }
                }
            }
            attr(newcapt, "bearings") = b_est
            attr(attr(newcapt, "bearings"), "details") = list(units = "radians", type = "continuous")
        }
        
        ##################################################        
        ## distances
        
        if(model.options$distances != 0 && n > 0){
            d_par  = submodel.arrays[[session]][["distances"]]
            d_est  = array(NA, dim = dim(newcapt))
            d_mean = distances
            d_func = switch(model.options$distances, 
                            function(EX, par) rgamma(1, shape = par, scale = EX / par), 
                            function(EX, par) rlnorm(1, meanlog = log(EX) - par^2 / 2, sdlog = par))
            for(k in 1:K){
                for(s in 1:S){
                    for(i in 1:n){
                        if(newcapt[i,s,k] == 1){
                            d_est[i,s,k] = d_func(d_mean[i,k], d_par[s,k]) 
                        }
                    }
                }
            }
            attr(newcapt, "distances") = d_est ; d_est[,1,]
            attr(attr(newcapt, "distances"), "details") = list(units = "m", type = "continuous")
        }
        
        ##################################################        
        ## capthist attributes 
        
        attr(newcapt, "session") = session.names[session]
        class(newcapt) = "capthist"
        traps(newcapt) = temptraps
        
        return(newcapt)
        
    }, simplify = FALSE)
    
    ##################################################        
    ## convert to single/multi session and add attributes
    
    if(length(newcapt) == 1){
        newcapt = newcapt[[1]]
    }else{
        newcapt = MS(newcapt)
        session(newcapt) = sapply(newcapth, session)
    }
    attr(newcapt, "sessioncov") = sessioncov
    attr(newcapt, "timecov") = timecov
    
    if(!is.null(seed)) set.seed(NULL)
    return(newcapt)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title SUbset a multi-session capthist
#' @description TODO
#' @param capthist a \code{\link{capthist}} object
#' @param index logical, integer or character vector indicating which capthist sessions (multi-session) or rows (single-session) to keep
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export

subset_capthist = function(capthist, index){
    if(!inherits(capthist, "capthist"))
        stop("capthist object required", call. = FALSE)
    attrs = attributes(capthist)
    if(ms(capthist)){
        # subset sessions
        capthist = capthist[index] # ms(capthist) ; class(capthist)
        # sessioncov
        if(!is.null(attrs$sessioncov)){
            attrs$sessioncov = attrs$sessioncov[index, , drop = FALSE]
        }
    }else{
        # subset groups
        capthist = capthist[index, , , drop = FALSE]
        if(!is.null(attrs$covariates))
            attrs$covariates = attrs$covariates[index, , drop = FALSE]
    }
    attributes(capthist) = attrs
    return(capthist)
}


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Subset a mask
#' @description TODO
#' @param mask a \code{\link{mask}} object
#' @param index logical, integer or character vector indicating which mask sessions (multi-session) or points (single-session) to keep
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export

subset_mask = function(mask, index){
    if(!inherits(mask, "mask"))
        stop("mask object required", call. = FALSE)
    attrs = attributes(mask)
    if(ms(mask)){
        # subset sessions
        mask = mask[index]
    }else{
        # subset points
        mask = mask[index, , drop = FALSE]
        if(!is.null(attrs$covariates))
            attrs$covariates = attrs$covariates[index, , drop = FALSE]
    }
    attributes(mask) = attrs
    return(mask)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# prints a summary of capthist data
# an alternative to secr::summary.capthist which is quite verbose

#' @title TODO
#' @description TODO
#' @inheritParams add_bearings
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export

summary_capthist = function(capthist){
    
    ##################################################
    ## check inputs
    
    if(!inherits(capthist, "capthist")) 
        stop("requires 'capthist' object", call. = FALSE)
    if(detector(traps(capthist)) != "proximity") 
        stop("requires proximity detectors", call. = FALSE)
    if(!ms(capthist)) capthist = MS(capthist)
    
    ##################################################
    ## detections
    
    cat("Detections:\n")
    detections.table = rbind(
        ngroups     = n_groups(capthist),
        ndetections = n_detections(capthist),
        ndays       = n_occasions(capthist),
        nposts      = n_traps(capthist)
    )
    detections.table = cbind(detections.table, total = apply(detections.table, 1, sum)) 
    print(detections.table)
    cat("\nRecaptures =", round(100 * mean(do.call(c, lapply(capthist, as.numeric)), na.rm = TRUE), 1), "%\n\n")
    
    ##################################################
    ## bearings and distances
    
    aux.data = sapply(c("bearings", "distances"), function(aux){
        any(sapply(capthist, function(x) !is.null(attr(x, aux))))
    })
    if(any(aux.data)){
        cat("Auxilliary data:")
        temp = do.call(rbind, sapply(c("bearings", "distances"), function(aux){ # aux = "bearings"
            if(aux.data[aux]){
                temp = c(
                    units = unique(do.call(c, lapply(capthist, function(x){ # x = capthist[[1]]
                        if(is.null(attr(x, aux))) NULL else attr(attr(x, aux), "details")$units
                    }))),
                    type = unique(do.call(c, lapply(capthist, function(x){
                        if(is.null(attr(x, aux))) NULL else attr(attr(x, aux), "details")$type
                    })))
                )
                range = range(sapply(capthist, function(x) as.numeric(attr(x, aux))), na.rm = TRUE)
                temp["range"] = paste0("(", paste(round(range, 1), collapse = ", "), ")")
            }else temp = NULL
            return(temp)
        }, simplify = FALSE))
        cat("\n")
        print(temp, quote = FALSE)
    }else{
        cat("\nNo auxilliary data\n")
    }
    
    ##################################################
    ## covariates
    
    covlevels = covlevels(capthist)
    if(is.null(covlevels)){
        cat("\nNo covariate data\n")
    }else{
        cat("\nCovariates:\n")
        covtable = do.call(rbind, 
                           sapply(covlevels, function(x){ # i = names(covlevels)[1] ; i
                               c(covariates = paste(x, collapse = ", "))
                           }, simplify = FALSE)
        )
        for(i in 1:nrow(covtable)){ # i=1
            rownames(covtable)[i] = switch(
                rownames(covtable)[i],
                "sessioncov"     = "array-level",
                "trapcov"        = "post-level",
                "timecov"        = "occasion-level",
                "timevaryingcov" = "post-occasion-level"
            )
        }
        print(covtable, quote = FALSE)
    }
    
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @inheritParams coef.gibbonsecr_fit
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method summary gibbonsecr_fit
#' @export

summary.gibbonsecr_fit = function(object, ...){
    
    if(object$nlm$code < 3){
        
        ##################################################
        ## model
        
        cat("Model:\n")
        cat(" detection func.", switch(object$model.options$detectfn + 1, "half normal", "hazard rate"), "\n")
        cat(" bearings dist. ", switch(object$model.options$bearings  + 1, "none", "von mises", "wrapped cauchy"), "\n")
        cat(" distances dist.", switch(object$model.options$distances + 1, "none", "gamma", "log-normal"), "\n")
        cat("\n")
        
        ##################################################
        ## mask
        
        cat("Mask:\n")
        summary_mask(object$mask, object$capthist)
        cat("\n")
        
        ##################################################
        ## parameter estimates
        
        cat("Parameter estimates (sub-model intercepts on inverse link scale):\n")
        est = coef(object)
        par.table = list(estimate = est) 
        # confidence intervals
        if(!is.null(object$nlm$hessian)){
            # don't need to use -H if negloglik has been minimised
            # inv.neg.hessian = try(solve(object$nlm$hessian), silent = TRUE)
            inv.neg.hessian = vcov(object)
            if(is.null(inv.neg.hessian)){
                par.table$lower = NA
                par.table$upper = NA
            }else{
                std.err = sqrt(diag(inv.neg.hessian))
                par.table$lower = est - 1.96 * std.err
                par.table$upper = est + 1.96 * std.err
            }
        }
        par.table = do.call(cbind, par.table)
        col.names = colnames(par.table)
        # extract intercept parameters
        par.table = par.table[grepl("Intercept", rownames(par.table)), , drop = FALSE] 
        # update rownames to give submodel name
        submodels = rownames(par.table) = gsub("\\.\\(Intercept\\)", "", rownames(par.table))
        # convert to response scale
        for(i in submodels)
            par.table[i,] = object$inv.link[[i]](par.table[i,])
        # pretty numbers
        par.table = as.data.frame(do.call(rbind, sapply(rownames(par.table), function(i){
            pretty_numbers(par.table[i,], 6)
        }, simplify = FALSE)))
        colnames(par.table) = col.names
        # add units
        par.table$units = sapply(rownames(par.table), function(i){ 
            switch(i,
                   "D" = "grps / sq km",
                   "sigma" = "metres",
                   "")
        }) 
        # print(par.table, digits = 5)
        print(par.table)
        cat("\n")
        
        ##################################################
        ## fixed
        
        if(length(object$fixed) == 0){
            cat("No fixed parameters\n")
        }else{
            cat("Fixed parameters:\n")
            temp = cbind(value = unlist(object$fixed)) ; temp
            rownames(temp) = paste0(" ", names(object$fixed))
            print(temp)
        }
        
        ##################################################
        ## esa, aic and run time
        
        esa = get_esa(object)
        cat("\nEffective sampling area:", round(mean(esa), 1), "sq km (average per array)\n")
        cat("\nAIC =", AIC(object), "\n")
        cat("\nTime taken: ", round(object$run.time,1), " ", attr(object$run.time, "units"), " (", object$nlm$iterations, " iterations)\n", sep = "")
        cat("\n")
        
    }else{
        warning("Fitting algorithm did not converge")
    }
    
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

summary_mask = function(mask, traps = NULL){
    if(!is.null(traps)){
        if(inherits(traps, "capthist"))
            traps = traps(traps)
        cat(" buffer    ", mask_buffer(mask, traps), "m\n")
    }
    cat(" spacing   ", mask_spacing(mask), "m\n")
    cat(" npoints   ", round(mean(mask_npoints(mask))), "(average per array)\n")
    cat(" area      ", round(mean(mask_Area(mask))), "sq km (average per array)\n")
    maskcov = covlevels(mask)$maskcov
    cat(" covariates", paste(maskcov, collapse = ", "), "\n")
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

sessioncov = function(capthist){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    if(ms(capthist)){
        attr(capthist, "sessioncov")
    }else{
        NULL
    }
}

'sessioncov<-' = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    if(ms(capthist)){
        if(!is.null(value)){
            if(!inherits(value, "data.frame"))
                stop("expecting a data.frame")
            if(nrow(value) != length(capthist))
                stop("nrow(sessioncov) should be equal to nsessions")
            if(!all(rownames(value) == session(capthist))){
                message("changing rownames(sessioncov) to equal session names")
                rownames(value) = session(capthist)
            }
        }
        structure(capthist, sessioncov = value)
    }else{
        stop("can't add sessioncov to single session capthist")
    }
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

timecov = function(capthist){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    if(ms(capthist)){
        sapply(capthist, function(x) attr(x, "timecov"), simplify = FALSE)
    }else{
        attr(capthist, "timecov")
    }
}

'timecov<-' = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    if(ms(capthist)){
        if(!inherits(value, "list") || inherits(value, "data.frame")){
            stop("expecting a list")
        }else{
            if(length(value) != length(capthist)){
                stop("length(timecov) should equal nsessions")
            }
            for(i in 1:length(value)){
                timecov(capthist[[i]]) <- value[[i]]
            }
            capthist
        }
    }else{
        if(!is.null(value)){
            if(!inherits(value, "data.frame")){
                stop("expecting a data.frame")
            }else{
                if(nrow(value) != n_occasions(capthist))
                    stop("nrow(timecov) should be equal to number of occasions")
            }
        }
        structure(capthist, timecov = value)
    }
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

traps_rbind = function(traps){
    if(!inherits(traps, "traps")) stop("expecting a traps object")
    if(!ms(traps)){
        message("can't rbind single-session traps")    
        return(traps)
    }
    # trap attributes
    attrs = lapply(traps, attributes)
    # check detector types
    types = sapply(attrs, function(x) x$detector)
    if(!all(types == types[1]))
        stop("can't rbind traps with different detector types")
    # combine coordinates        
    regiontraps = do.call(rbind, lapply(traps, as.data.frame))
    # add attributes
    class(regiontraps) = c("traps","data.frame")
    covariates(regiontraps) = do.call(rbind, lapply(traps, covariates))
    usage(regiontraps) = do.call(rbind, lapply(traps, usage))
    attr(regiontraps, "spacex") = NA
    attr(regiontraps, "spacey") = NA
    attr(regiontraps, "spacing") = NA
    return(regiontraps)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @inheritParams coef.gibbonsecr_fit
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method vcov gibbonsecr_fit
#' @export
vcov.gibbonsecr_fit = function(object, ...){
    if(is.null(object$nlm$hessian)){
        stop("no hessian") 
    }else{
        vcov = try(MASS::ginv(object$nlm$hessian), silent = TRUE)
        if(inherits(vcov, "try-error")){
            return(NULL)
        }else{
            dimnames(vcov) = dimnames(object$nlm$hessian)
            return(vcov)
        }
    }
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
