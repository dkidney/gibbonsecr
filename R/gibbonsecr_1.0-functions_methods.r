## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @rdname add_bearings_distances
# @name add_bearings
# @aliases add_bearings
# @aliases add_bearings<-
# @aliases add_distances
# @aliases add_distances<-
# @title Add estimated bearings or distances data to a capthist object
# @description TODO
# @param capthist a \code{\link{capthist}} object
# @param value estimated bearings or distances data
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export
add_bearings = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    # check bearings argument
    attr(capthist, "bearings") = value
    return(capthist)
}

# @rdname add_bearings_distances
# @name add_distances
# @export
add_distances = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("requires a 'capthist' object")
    # check distances argument
    attr(capthist, "distances") = value
    return(capthist)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# update to addCovariates which allows for SpatialPointsDataFrame class
add_covariates = function(x, shp){
    if(!inherits(x, c("mask","traps")))
        stop("expecting a mask or traps object")
    if(!inherits(shp, c("SpatialPolygonsDataFrame","SpatialPointsDataFrame")))
        stop("expecting a SpatialPolygonsDataFrame or SpatialPointsDataFrame object")
    class  = if(inherits(x, "mask")) "mask" else "traps"
    covnames = colnames(shp@data)
    if(ms(x)){
        x = lapply(x, function(x) add_covariates(x, shp))
        class(x) = c("list", class)
    }else{
        # check mask covs for covnames
        if(!is.null(covariates(x))){
            for(j in covnames){
                if(j %in% colnames(covariates(x))){
                    warning(paste0("overwriting existsing covariate '", j,
                                   "' in", class))
                    covariates(x)[[j]] = NULL
                }
            }
        }
        if(inherits(shp, "SpatialPointsDataFrame")){
            # secr::addCovariates doen't work for spatial points
            # use nearest point method for assigning covariates to mask
            distances = calc_distances_rcpp(as.matrix(x), as.matrix(shp@coords))
            i = apply(distances, 2, which.min)
            if(is.null(covariates(x))){
                covariates(x) = shp@data[i,]
                rownames(covariates(x)) = rownames(x)
            }else{
                covariates(x) = cbind(covariates(x), shp@data[i,])
            }
        }else{
            x = addCovariates(x, shp)
        }
    }
    return(x)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Compute the AIC score
#' @description Compute the AIC score for a model fitted with
#'   \code{\link{gibbonsecr_fit}}.
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

# @rdname calc_bearings_distances
# @name calc_bearings
# @aliases calc_bearings.default
# @aliases calc_bearings.traps
# @aliases calc_distances
# @aliases calc_distances.default
# @aliases calc_distances.traps
# @title Calculate bearings or distances between two sets of points
# @description Calculate bearings or distances from points in \code{x} to
#   points in \code{y}.
# @param x a matrix, data.frame or \code{\link{traps}} object
# @param y a matrix, data.frame or \code{\link{mask}} object
# @param ... additional arguments (not used)
# @details These functions are wrappers to non-exported functions written using
#   \code{\link{Rcpp}}. (The underlying Rcpp functions, which take matrix
#   inputs and have no error checks, can be accessed with
#   \code{gibbonsecr:::calc_bearings_rcpp} and
#   \code{gibbonsecr:::calc_distances_rcpp}.)
#
# \code{.default}
# \itemize{
#   \item \code{x} and \code{y} must be two-column matrices or data.frames.
# }
# \code{.traps}
# \itemize{
#   \item \code{x} must be a \code{\link{traps}} object and \code{y} must be a
#   \code{\link{mask}} object.
# }
# @return A matrix, with columns representing points in \code{x} and rows
#   representing points in \code{y}. \code{calc_bearings} returns a matrix of
#   radians and \code{calc_distances} returns a matrix of distances in the same
#   units as \code{x} and \code{y} (\code{x} and \code{y} must use the same
#   units). In the case of \code{.traps} functions, if \code{x} or \code{y} are
#   multi-session, then a list of matrices is returned with one element per
#   session.
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @seealso \link{add_bearings}, \link{add_distances}, \link{get_bearings},
#   \link{get_captures}, \link{get_distances}
# @examples
# ## Example 1: from a single point to all points in a matrix
# A = matrix(0,1,2)
# Z = matrix(c(0,1,1,1,1,0), 3, 2)
# plot(Z, pch = 19, col = 2, asp = 1)
# points(A, pch = 19, col = 4)
# calc_distances(A, Z)
# calc_bearings(A, Z)
#
# ## Example 2: from a trap to all points in a mask
# data(N.annamensis)
# traps = secr::traps(N.annamensis)[[1]]
# mask = secr::make.mask(traps, buffer = 3000, spacing = 100)
#
# distances = calc_distances(traps, mask)
# bearings = calc_bearings(traps, mask)
#
# # plot results for trap 1
# x = unique(mask$x)
# y = unique(mask$y)
#
# op = par(no.readonly = TRUE)
# par(mfrow = c(1,2))
#
# z = matrix(distances[,1], length(x), length(y))
# image(x, y, z, col = heat.colors(100), asp = 1, main = "Distances")
# points(traps, pch = 19)
# points(traps[1,], pch = 19, col = 4)
#
# z = matrix(bearings[,1], length(x), length(y))
# image(x, y, z, col = heat.colors(100), asp = 1, main = "Distances")
# points(traps, pch = 19)
# points(traps[1,], pch = 19, col = 4)
#
# par(op)
# @export
calc_bearings = function(x, y, ...){
    UseMethod("calc_bearings")
}

# @rdname calc_bearings_distances
# @name calc_bearings.default
# @export
calc_bearings.default = function(x, y, ...){
    warning.text = "x and y must be matrices or data.frames"
    if(!inherits(x, c("matrix", "data.frame")) ||
       !inherits(y, c("matrix", "data.frame")))
        stop(warning.text)
    if(ncol(x) != 2 || ncol(y) != 2)
        stop(warning.text)
    calc_bearings_rcpp(as.matrix(x), as.matrix(y))
}

# @rdname calc_bearings_distances
# @name calc_bearings.traps
# @method calc_bearings traps
# @export
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

# @rdname calc_bearings_distances
# @name calc_distances
# @export
calc_distances = function(x, y, ...){
    UseMethod("calc_distances")
}

# @rdname calc_bearings_distances
# @name calc_distances.default
# @export
calc_distances.default = function(x, y, ...){
    warning.text = "x and y must be matrices or data.frames"
    if(!inherits(x, c("matrix", "data.frame")) ||
       !inherits(y, c("matrix", "data.frame")))
        stop(warning.text)
    if(ncol(x) != 2 || ncol(y) != 2)
        stop(warning.text)
    calc_distances_rcpp(x, y)
}

# @rdname calc_bearings_distances
# @name calc_distances.traps
# @method calc_distances traps
# @export
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
        x = if(ms(x)) x[[i]] else x
        y = if(ms(y)) y[[i]] else y
        dists = calc_distances_rcpp(
            as.matrix(if(ms(x)) x[[i]] else x),
            as.matrix(if(ms(y)) y[[i]] else y)
        )
        dimnames(dists) = list(rownames(y), rownames(x))
        return(dists)
        #         x = calc_distances_rcpp(
        #             as.matrix(if(ms(x)) x[[i]] else x),
        #             as.matrix(if(ms(y)) y[[i]] else y)
        #         )
    }), session.names)
    if(!ms(x) && !ms(y)) distances = distances[[1]]
    return(distances)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Extract model parameters
#' @description Extract estimated parameters from a model fitted with
#'   \code{\link{gibbonsecr_fit}}.
#' @param object a \code{gibbonsecr_fit} model object (i.e. an object returned
#'   from the \code{\link{gibbonsecr_fit}} function)
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

#' @title Compute confidence intervals for model parameters
#' @description Compute confidence intervals stimated parameters from a model
#'   fitted with \code{\link{gibbonsecr_fit}}.
#' @details Uses the delta method to obtain interval estimates on the link
#'   scale.
#' @inheritParams coef.gibbonsecr_fit
#' @param parm parameters (see documentation for the \code{\link{confint}}
#'   generic)
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

covtable = function(data, w = 50){
    covtable = do.call(rbind, lapply(colnames(data), function(i){
        factor = inherits(data[[i]], "factor")
        info = c(Name = paste0("'", i, "'"),
                 Type = if(factor) "category" else "number",
                 Details = if(factor){
                     paste0(length(levels(data[[i]])), " levels: ",
                            paste0(levels(data[[i]]),
                                   collapse = ","))
                 }else{
                     paste0("range: ",
                            paste(prettyNum(range(data[[i]])),
                                  collapse = " to "))
                 })
        if(nchar(info["Details"]) > w){
            commas = gregexpr(",", info["Details"])[[1]]
            j = which(commas <= w)
            j = j[length(j)]
            info["Details"] = paste0(substr(info["Details"], 1, commas[j]), "...")
        }
        return(info)
    }))
    return(covtable)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

fitted_detectfn_auxiliary_values_old = function(beta, fit, session, x, which = "detectfn", true.distance = 500){

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
    EX = switch(which, "bearings" = 0, "distances" = true.distance, NULL)

    ##################################################
    ## design matrices

    design.matrices = sapply(submodels, function(submodel){ # submodel = "sigma"
        covs = all.vars(fit$model[[submodel]]) # covs
        if(length(covs) == 0){
            model.frame = data.frame(matrix(0, nrow = 1, ncol = 0))
        }else{
            # all covariates (from all sessions)
            model.frame = do.call(rbind, lapply(fit$model.frames, function(x){
                x[[submodel]]
            })) # head(model.frame)
            # extract reference level (factors) or mean (numeric) for each cov
            model.frame = do.call(cbind, sapply(covs, function(cov){
                # cov = covs[1] ; cov
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
        make_model_matrix(fit$model[[submodel]],
                          model.frame,
                          fit$smooth.setup[[submodel]])
    }, simplify = FALSE)
    # get submodel arrays - each array will be a 1 by 1 matrix
    design.matrices = list("1" = design.matrices)

    ##################################################
    ## convert to submodel arrays and return function values

    submodel.arrays = make_submodel_arrays(
        beta            = beta,
        parindx         = fit$parindx,
        fixed           = fit$fixed,
        design.matrices = design.matrices,
        inv.link        = fit$inv.link,
        S               = c("1" = 1),
        K               = c("1" = 1),
        submodels       = submodels,
        sessions        = "1")[["1"]]
    par = as.numeric(submodel.arrays)
    FUN(x, par, EX)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

fitted_detectfn_auxiliary_values = function(beta, fit, newdata, x, which = "detectfn", true.distance = 500){

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
    EX = switch(which, "bearings" = 0, "distances" = true.distance, NULL)

    ##################################################
    ## design matrices

    if(is.null(newdata)){
        newdata = make_newdata(fit, submodels)
    }else{
        if(nrow(newdata) != 1)
            error("newdata should only have one row")
    }

    ##################################################
    ## model matrix

    # use list("1" = ... to make dummy session
    design.matrices = list("1" = sapply(submodels, function(submodel){
        make_model_matrix(formula      = fit$model[[submodel]],
                          data         = newdata,
                          smooth.setup = fit$smooth.setup[[submodel]])
    }, simplify = FALSE))

    ##################################################
    ## convert to submodel arrays and return function values

    submodel.arrays = make_submodel_arrays(
        beta            = beta,
        parindx         = fit$parindx,
        fixed           = fit$fixed,
        design.matrices = design.matrices,
        inv.link        = fit$inv.link,
        S               = c("1" = 1),
        K               = c("1" = 1),
        submodels       = submodels,
        sessions        = "1")[["1"]]
    par = as.numeric(submodel.arrays)
    FUN(x, par, EX)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

fitted_submodel_values = function(beta, fit, newdata = NULL, submodel = "D"){

    ##################################################
    ## checks

    if(!submodel %in% names(fit$parindx))
        stop("no model for ", submodel)
    if(is.null(newdata))
        newdata = make_newdata(fit, submodel)

    ##################################################
    ## model matrix

    # use list("1" = ... to make dummy session
    design.matrices = list("1" = sapply(submodel, function(submodel){
        make_model_matrix(formula      = fit$model[[submodel]],
                          data         = newdata,
                          smooth.setup = fit$smooth.setup[[submodel]])
    }, simplify = FALSE))

    ##################################################
    ## submodel arrays

    as.numeric(make_submodel_arrays(
        beta            = beta,
        parindx         = fit$parindx,
        fixed           = fit$fixed,
        design.matrices = design.matrices,
        inv.link        = setNames(list(function(x) x), submodel),
        S               = c("1" = nrow(newdata)),
        K               = c("1" = 1),
        sessions        = "1",
        submodels       = submodel
    )[["1"]][[submodel]])

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
            model.frames = model.frame,
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
        parindx         = fit$parindx,
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
        parindx         = fit$parindx,
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

# @rdname get_bearings_captures_distances
# @name get_bearings
# @title Extract data from capthist
# @description TODO
# @details TODO
# @param capthist a \code{\link{capthist}} object
# @param summarise choose whether to collapse capture data across traps or occasions (see \bold{Details})
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export
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

# @rdname get_bearings_captures_distances
# @name get_distances
# @export
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

# @rdname get_bearings_captures_distances
# @name get_captures
# @export
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

    # shortcut if D model is intercept only
    # otherwise need to calculate
    esa = if(fit$model$D == ~ 1){

        n_groups(fit$capthist) / fit$inv.link$D(coef(fit)[fit$parindx$D])

    }else{

        calc_esa(
            beta            = coef(fit),
            detectfn        = fit$model.options$detectfn,
            parindx         = fit$parindx,
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

    }

    return(esa)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Extract the log-likelihood
#' @description Extract the log-likelihood from a model fitted with
#'   \code{\link{gibbonsecr_fit}}.
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

make_newdata = function(fit, submodels = NULL){
    if(!inherits(fit, "gibbonsecr_fit"))
        stop("gibbonsecr_fit object required")
    if(is.null(submodels))
        submodels = names(fit$model)
    newdata = do.call(cbind, sapply(submodels, function(submodel){
        # if no covariates, use model frame with no columns
        # otherwise make big model frame by collapsing sessions
        # and get average / reference level for each covariate
        covnames = all.vars(fit$model[[submodel]])
        if(length(covnames) == 0){
            data.frame(matrix(0, nrow = 1, ncol = 0))
        }else{
            big.mf = do.call(rbind, lapply(fit$model.frames, function(x){
                x[[submodel]]
            }))
            do.call(cbind, sapply(covnames, function(cov){
                out = list()
                out[[cov]] = if(inherits(big.mf[[cov]], "factor")){
                    levs = levels(big.mf[[cov]])
                    factor(levs[1], levels = levs)
                }else{
                    mean(big.mf[,cov])
                }
                as.data.frame(out)
            }, simplify = FALSE))
        }
    }, simplify = FALSE))
    return(newdata)
}

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
    if(ms(mask)){
        bbox = sapply(mask, mask_bbox, simplify = FALSE)
    }else{
        spacing = attr(mask, "spacing")
        if(is.null(spacing)) stop("no spacing attribute")
        x = range(mask$x) + c(-1, 1) * spacing / 2
        y = range(mask$y) + c(-1, 1) * spacing / 2
        bbox = data.frame(x = x[c(1,2,2,1)], y = y[c(1,1,2,2)])
    }
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

mask_meanSD = function(mask){
    as.data.frame(apply(mask, 2, function(x) c(mean(x), sd(x))))
}

mask_na_rm = function(x){
    if(!inherits(x, "mask")) stop("mask object required")
    if(ms(x)){
        x = sapply(x, mask_na_rm, simplify = FALSE)
        class(x) = c("list","mask")
    }else{
        delete = is.na(x$x) | is.na(x$y)
        if(!is.null(covariates(x)))
            delete = delete | apply(covariates(x), 1, function(x) any(is.na(x)))
        if(any(delete))
            x = subset_mask(x, !delete)
    }
    return(x)
}

mask_npoints = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(!ms(mask)) mask = list(mask)
    sapply(mask, function(x){
        nrow(x)
    })
}

make_regionmask = function(mask){
    if(!inherits(mask, "mask")) stop("expecting a mask object")
    if(!ms(mask)) return(mask)
    # combine coordinates
    regionmask = do.call(rbind, lapply(mask, as.data.frame))
    # combine covariates
    if(!is.null(covariates(mask))){
        covariates(regionmask) = do.call(rbind, covariates(mask))
    }
    # mask attributes
    class(regionmask) = c("mask", "data.frame")
    attr(regionmask, "meanSD") = mask_meanSD(regionmask)
    attr(regionmask, "boundingbox") = mask_bbox(regionmask)
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

# extract mask covarates including x and y coordinates

maskcov = function(mask){
    if(!inherits(mask, "mask")) stop("mask object required")
    if(ms(mask)){
        lapply(mask, maskcov)
    }else{
        if(is.null(covariates(mask))){
            as.data.frame(mask)
        }else{
            cbind(as.data.frame(mask), covariates(mask))
        }
    }
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# convert a list of capthists, traps or mask to a multi-session object

MS = function(x, session.names = NULL){

    ##################################################
    ## checks

    if(!inherits(x, "list"))
        x = list(x)
    # check all elements have the same class
    classes = c("capthist", "mask", "traps")
    classesOK = sapply(classes, function(i){
        all(sapply(x, function(x) inherits(x, i)))
    })
    if(!any(classesOK))
        stop("expecting 'capthist', 'traps' or 'mask' objects")

    ##################################################
    ## convert to multi-session

    class = classes[classesOK]
    if(!(inherits(x, "list") && inherits(x, class)))
        class(x) = c("list", class)

    ##################################################
    ## check session names

    if(is.null(session.names))
        session.names = session(x)
    if(length(session.names) == 0)
        session.names = sapply(x, function(x){
            y = attr(x, "session")
            if(is.null(y)) NA else y
        })
    if(length(unique(session.names)) != length(session.names) ||
       any(is.na(session.names)))
        session.names = names(x)
    if(is.null(session.names)){
        # message("using default session.names")
        session.names = as.character(1:length(x))
    }
    # check session names are unique and have correct length
    if(length(session.names) != length(unique(session.names)))
        stop("session.names not unique")
    if(length(session.names) != length(x))
        stop("session.names of incorrect length")
    # update session names
    for(i in 1:length(x))
        attr(x[[i]], "session") = session.names[i]
    session(x) = session.names

    return(x)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# get number of arrays / groups / occasions / traps in capthist
# for groups, occasions and traps:
# - if capthist is not ms, returns a scalar
# - if capthist is ms, returns a vector

n_arrays = function(x){
    if(!inherits(x, "capthist")) stop("capthist object required")
    x = MS(x)
    return(length(x))
}

n_detections = function(x){
    if(!inherits(x, "capthist")) stop("capthist object required")
    x = MS(x)
    sapply(x, function(x){
        if(is.null(dim(x))) 0 else sum(x)
    })
}

n_groups = function(x){
    if(!inherits(x, "capthist")) stop("capthist object required")
    x = MS(x)
    sapply(x, function(x){
        if(is.null(dim(x))) 0 else dim(x)[1]
    })
}

n_occasions = function(x){
    if(!inherits(x, "capthist")) stop("capthist object required")
    if(!ms(x)) x = MS(x)
    sapply(x, function(x){ # x = x[["15"]]
        if(is.null(dim(x))) dim(usage(traps(x)))[2] else dim(x)[2]
    })
}

n_traps = function(x){
    if(!inherits(x, c("capthist","traps")))
        stop("capthist or traps object required")
    x = MS(x)
    sapply(x, function(x){
        if(inherits(x, "capthist")){
            nrow(traps(x))
        }else{
            nrow(x)
        }
    })
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

predict.gibbonsecr_fit = function(object, newdata = NULL, submodels = NULL, se.fit = TRUE, alpha = 0.05, ...){

    if(is.null(submodels))
        submodels = names(object$parindx)
    # if newdata not supplied, make default dataframe of covariate values
    # using averages / factor reference levels
    if(is.null(newdata))
        newdata = make_newdata(object, submodels)

    ##################################################
    ## check bounds
    # warn if outside observed range
    # check_newdata(newdata, object)

    ##################################################
    ## point and interval estimates

    # link scale point and interval estimates
    delta = sapply(submodels, function(submodel){ # submodel = "D"
        deltaargs = list(beta     = coef(object),
                         fit      = object,
                         newdata  = newdata,
                         submodel = submodel)
        if(se.fit){
            deltaargs$f    = fitted_submodel_values
            deltaargs$vcov = vcov(object)
            delta = do.call(delta_method, deltaargs)
            zvalue = qnorm(1 - alpha / 2)
            delta$lower = delta$est - zvalue * delta$se
            delta$upper = delta$est + zvalue * delta$se
            delta$se = NULL
        }else{
            delta = list(est = do.call(fitted_submodel_values, deltaargs))
        }
        return(delta)
    }, simplify = FALSE)

    # apply inverse link
    delta = sapply(submodels, function(submodel){ # submodel = "D"
        lapply(delta[[submodel]], object$inv.link[[submodel]])
    }, simplify = FALSE)

    # reshape
    index = if(se.fit) c("lower","upper","est") else "est"
    preds = sapply(index, function(i){ # i="est"
        x = do.call(cbind, sapply(delta, function(x) x[[i]], simplify = FALSE))
        rownames(x) = rownames(newdata)
        return(x)
    }, simplify = FALSE)

    return(preds)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Print a summary of a fitted model
#' @description Print a summary of the results from a model fitted with
#'   \code{\link{gibbonsecr_fit}}.
#' @param x a \code{gibbonsecr} model object (i.e. an object returned from the
#'   \code{\link{gibbonsecr_fit}} function)
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

#' @title Simulate data from a fitted model
#' @description Simulate capture history data from a model fitted with
#'   \code{\link{gibbonsecr_fit}}
#' @inheritParams coef.gibbonsecr_fit
#' @param nsim number of data sets to simulate (currently fixed at 1)
#' @param seed an integer to initialize the random number generator
#' @param buffer buffer distance for use in generating the underlying population
#'   (see Details)
#' @param spacing TODO
#' @param beta TODO
#' @param debug TODO
#' @details TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link{gibbonsecr_fit}
#' @method simulate gibbonsecr_fit
#' @importFrom stats simulate
#' @export

simulate.gibbonsecr_fit = function(object, nsim = 1, seed = NULL, buffer = 6000, spacing = 10, beta = NULL, debug = FALSE, ...){
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
        parindx       = object$parindx,
        n_occasions   = n_occasions(object$capthist),
        sessioncov    = sessioncov(object$capthist),
        timecov       = timecov(object$capthist),
        smooth.setup  = object$smooth.setup,
        buffer        = buffer,
        spacing       = spacing,
        seed          = seed,
        debug         = debug
    )
    # object = fit ; traps = traps(object$capthist) ; model = object$model ; model.options = object$model.options ; fixed = object$fixed ; n_occasions = n_occasions(object$capthist) ; sessioncov = sessioncov(object$capthist) ; timecov = timecov(object$capthist) ; smooth.setup  = object$smooth.setup ; usage.prob = NULL
    return(simcapt)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

sim_capthist = function(beta, traps, model, model.options, fixed, parindx, n_occasions, sessioncov = NULL, timecov = NULL, smooth.setup = NULL, buffer = 6000, spacing = 10, usage.prob = NULL, seed = NULL, debug = FALSE){

    ##################################################
    ## check inputs

    if(is.null(names(beta)))
        stop("beta needs to be a named vector of parameter values")
    traps = MS(traps)
    sessions = session(traps)
    # check n_occasions
    # - check n_occasions length again n_sessions
    if(length(n_occasions) != length(session(traps)))
        stop("length(n_occasions) != length(session(traps))")
    # - check n_occasions names against session names
    if(!all(names(n_occasions) == sessions))
        stop("!all(names(n_occasions) == session(traps))")
    # check timecov
    if(!is.null(timecov) && !all(sapply(timecov, is.null))){
        # - check timecov names against session names
        if(!all(names(timecov) == sessions))
            stop("!all(names(timecov) == session(traps))")
        # - check timecov nrows against n_occasions values
        if(!all(sapply(timecov, nrow) == n_occasions))
            stop("!all(sapply(timecov, nrow) == n_occasions)")
    }
    # check sessioncov
    if(!is.null(sessioncov)){
        # - check sessioncov rownames against session names
        if(!all(rownames(sessioncov) == sessions))
            stop("!all(rownames(sessioncov) == session(traps))")
    }
    # check timevaryingcov
    if(!is.null(timevaryingcov(traps))){
        for(session in sessions){
            tvc = timevaryingcov(traps)[[session]]
            for(cov in names(tvc))
                if(length(tvc[[cov]]) != n_occasions[session])
                    stop("length(timevaryingcov(traps)[['", session, "']][['",
                         cov, "']]) != n_occasions['", session, "']")
        }
    }

    n_traps = n_traps(traps)

    ##################################################
    ## make submodel arrays

    par.labels = cbind(
        submodel = sapply(strsplit(names(beta), "\\."), function(x) x[1]),
        term     = sapply(strsplit(names(beta), "\\."), function(x) x[2]),
        unique   = names(beta)
    )

    bigmask = make.mask(traps, buffer = buffer, spacing = spacing) # mask_npoints(bigmask)

    inv.link = get_inv_link(model, fixed = fixed, model.options)

    model.frames = make_model_frames(
        model        = model,
        traps        = traps,
        mask         = bigmask,
        n_occasions  = n_occasions,
        sessioncov   = sessioncov,
        timecov      = timecov,
        debug        = debug
    ) # model.frames[[1]]

    design.matrices = make_design_matrices(
        model        = model,
        model.frames = model.frames,
        smooth.setup = smooth.setup,
        debug        = debug
    ) # design.matrices[[1]]

    submodel.arrays = make_submodel_arrays(
        beta            = beta,
        parindx         = parindx,
        fixed           = fixed,
        design.matrices = design.matrices,
        inv.link        = inv.link,
        S               = n_occasions,
        K               = n_traps
    ) # submodel.arrays[[1]]

    ##################################################
    ## make new capthist

    newcapt = sapply(sessions, function(session){ # session = sessions[1] ; session

        if(debug) message("\nsession ", session)

        temptraps = if(ms(traps)) traps[[session]] else traps
        tempmask  = if(ms(bigmask)) bigmask[[session]] else bigmask
        S         = n_occasions[session]
        K         = n_traps[session]
        M         = mask_npoints(tempmask)
        a         = mask_area(tempmask)

        ##################################################
        ## trap usage

        if(debug) message("trap usage")

        # use usage probability if supplied, otherwise use existing usage
        # if no existing usage then assume full usage
        if(!is.null(usage.prob)){
            usage(temptraps) = matrix(rbinom(K * S, 1, usage.prob),
                                      nrow = K, ncol = S)
        }else{
            if(is.null(usage(temptraps)))
                usage(temptraps) = matrix(1, nrow = K, ncol = S)
        }

        ##################################################
        ## population

        if(debug) message("population")

        # get density values for all points on mask
        D.values = submodel.arrays[[session]][["D"]]
        # assume poisson and draw random population size
        N = rpois(1, sum(D.values) * a)
        # take sample of N mask points
        i = sample(x = 1:M, size = N, replace = TRUE,
                   prob = D.values / sum(D.values))
        population = tempmask[i, , drop = FALSE]
        rownames(population) = paste(session, 1:N, sep = "_")
        # head(population) ; dim(population) ; plot(population, asp = 1)

        if(debug) message("N = ", N)

        ##################################################
        ## capthist

        if(debug) message("capthist")

        # calulate distances
        distances = calc_distances(temptraps, population)
        # head(distances) ; dim(distances)
        # calulate detection probabilities
        usage = usage(temptraps)
        dimnames(usage) = list(rownames(temptraps), 1:S)
        g0    = submodel.arrays[[session]][["g0"]]    # dim(g0)
        sigma = submodel.arrays[[session]][["sigma"]] # dim(sigma)
        z     = submodel.arrays[[session]][["z"]]     # dim(z)
        detprob = calc_detprob(model.options$detectfn,
                               g0, sigma, z, distances, usage, N, S, K)
        # head(detprob[,1,]) ; dim(detprob)
        # generate capthist
        newcapt = array(
            rbinom(n = N * S * K, size = 1, prob = as.numeric(detprob)),
            dim = dim(detprob),
            dimnames = dimnames(detprob)
        )
        # head(newcapt[,1,]) ; dim(newcapt)
        # delete undetected groups
        class(newcapt) = "capthist"
        i = apply(get_captures(newcapt, summarise = "occasions"), 1, function(x){
            any(x == 1)
        })
        newcapt   = newcapt[i, , , drop = FALSE]  # head(newcapt[,1,]) ; dim(newcapt) ; sum(newcapt)
        locations = population[i, , drop = FALSE] # head(locations) ; dim(locations)
        distances = distances [i, , drop = FALSE] # head(distances) ; dim(distances)
        n = sum(i)

        if(debug) message("n = ", n)

        ##################################################
        ## availability

        if(debug) message("availability")

        if(n > 0){
            # availability probabilities - pcall should be length S
            pcall = as.numeric(submodel.arrays[[session]][["pcall"]])
            # for each group, simulate availability for each occasion
            # and delete capthist for all k in occasions with no availability
            for(i in 1:n){
                available = rbinom(S, 1, pcall) # newcapt[i,,]
                newcapt[i, available == 0, ] = 0 # newcapt[i,,]
            }
            # delete undetected groups to account for availability
            class(newcapt) = "capthist"
            i = apply(get_captures(newcapt, summarise = "occasions"), 1, function(x){
                any(x == 1)
            })
            newcapt   = newcapt[i, , , drop = FALSE]  # head(newcapt[,1,]) ; dim(newcapt) ; sum(newcapt)
            locations = locations[i, , drop = FALSE]  # head(locations) ; dim(locations)
            distances = distances [i, , drop = FALSE] # head(distances) ; dim(distances)
            n = sum(i)
        }

        if(debug) message("n = ", n)

        ##################################################
        ## bearings

        if(debug) message("bearings")

        if(model.options$bearings != 0 && n > 0){
            b_par  = submodel.arrays[[session]][["bearings"]]
            b_mean = calc_bearings_rcpp(as.matrix(temptraps),
                                        as.matrix(locations))
            b_est  = array(NA, dim = dim(newcapt))
            b_func = switch(
                model.options$bearings,
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
            attr(attr(newcapt, "bearings"), "details") = list(
                units = "radians",
                type = "continuous"
            )
        }

        ##################################################
        ## distances

        if(debug) message("distances")

        if(model.options$distances != 0 && n > 0){
            d_par  = submodel.arrays[[session]][["distances"]]
            d_est  = array(NA, dim = dim(newcapt))
            d_mean = distances
            d_func = switch(
                model.options$distances,
                function(EX, par){
                    rgamma(1, shape = par, scale = EX / par)
                },
                function(EX, par){
                    rlnorm(1, meanlog = log(EX) - par^2 / 2, sdlog = par)
                })
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
            attr(attr(newcapt, "distances"), "details") = list(
                units = "m",
                type = "continuous"
            )
        }

        ##################################################
        ## capthist attributes

        if(debug) message("attributes")

        class(newcapt) = "capthist"
        traps(newcapt) = temptraps
        attr(newcapt, "population") = population
        attr(newcapt, "locations") = locations

        return(newcapt)
    }, simplify = FALSE)

    ##################################################
    ## convert to multi-session and add attributes

    newcapt = MS(newcapt, sessions)
    sessioncov(newcapt) = sessioncov
    timecov(newcapt) = timecov

    if(!is.null(seed)){
        attr(newcapt, "seed") = seed
        set.seed(NULL)
    }
    return(newcapt)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title SUbset a multi-session capthist
# @description TODO
# @param capthist a \code{\link{capthist}} object
# @param keep logical, integer or character vector indicating which capthist sessions (multi-session) or rows (single-session) to keep
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export

subset_capthist = function(capthist, keep){
    if(!inherits(capthist, "capthist"))
        stop("capthist object required")
    attrs = attributes(capthist)
    if(ms(capthist)){
        # subset sessions
        capthist = capthist[keep] # ms(capthist) ; class(capthist)
        # sessioncov
        if(!is.null(attrs$sessioncov)){
            attrs$sessioncov = attrs$sessioncov[keep, , drop = FALSE]
        }
    }else{
        # subset groups
        capthist = capthist[keep, , , drop = FALSE]
        if(!is.null(attrs$covariates))
            attrs$covariates = attrs$covariates[keep, , drop = FALSE]
    }
    attributes(capthist) = attrs
    return(capthist)
}


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title Subset a mask
# @description TODO
# @param mask a \code{\link{mask}} object
# @param keep logical, integer or character vector indicating which mask sessions (multi-session) or points (single-session) to keep
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export

subset_mask = function(mask, keep){
    if(!inherits(mask, "mask"))
        stop("mask object required")
    if(ms(mask)){
        # delete sessions
        mask = mask[keep]
        # return lost attribute
        attr(mask, "class") = c("list", "mask")
    }else{
        # delete mask points
        mask = mask[keep, , drop = FALSE]
        if(!is.null(covariates(mask))){
            # delete rows in covariates
            covariates(mask) = covariates(mask)[keep, , drop = FALSE]
        }
        # update attributes
        attr(mask, "boundingbox") = data.frame(
            x = (range(mask$x) + c(-1,1) * attr(mask, "spacing") / 2)[c(1,2,2,1)],
            y = (range(mask$y) + c(-1,1) * attr(mask, "spacing") / 2)[c(1,1,2,2)]
        )
        attr(mask, "meanSD") = as.data.frame(apply(mask, 2, function(x) c(mean(x), sd(x))))
    }
    return(mask)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# prints a summary of capthist data
# an alternative to secr::summary.capthist which is quite verbose

# @title TODO
# @description TODO
# @inheritParams add_bearings
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export

summary_capthist = function(capthist){

    ##################################################
    ## check inputs

    if(inherits(capthist, "gibbonsecr_fit"))
        capthist = capthist$capthist
    if(!inherits(capthist, "capthist"))
        stop("requires 'capthist' object")
    if(detector(traps(capthist)) != "proximity")
        stop("requires proximity detectors")
    capthist = MS(capthist)

    ##################################################
    ## detections

    cat("Detections:\n")
    detections.table = rbind(
        "ngroups"     = n_groups(capthist),
        "ndetections" = n_detections(capthist),
        "noccasions"  = n_occasions(capthist),
        "nposts"      = n_traps(capthist)
    )
    detections.table = cbind(detections.table,
                             total = apply(detections.table, 1, sum))
    detections.table = cbind(array = c("ngroups","ndetections","ndays","nposts"),
                             detections.table)
    print(as.data.frame(detections.table), quote = FALSE, row.names = FALSE)
    cat("\nRecaptures =", round(100 * mean(do.call(c, lapply(capthist, as.numeric)),
                                           na.rm = TRUE), 1), "%\n\n")

    ##################################################
    ## bearings and distances

    aux.data = sapply(c("bearings", "distances"), function(aux){
        any(sapply(capthist, function(x) !is.null(attr(x, aux))))
    })
    if(any(aux.data)){
        cat("Auxilliary data:\n")
        auxtable = do.call(rbind, sapply(names(aux.data)[aux.data], function(i){
            # i="bearings"
            units = unique(do.call(c, lapply(capthist, function(x){
                if(is.null(attr(x, i))) NULL else{
                    attr(attr(x, i), "details")$units
                }
            })))
            type = unique(do.call(c, lapply(capthist, function(x){
                if(is.null(attr(x, i))) NULL else{
                    attr(attr(x, i), "details")$type
                }
            })))
            details = if(type == "continuous"){
                paste("range:", paste(prettyNum(range(
                    sapply(capthist, function(x){
                        as.numeric(attr(x, i))
                    }), na.rm = TRUE)), collapse = " to "))
            }else{
                stop("not implemented for interval data")
            }
            c(" " = i, Units = units, Type = type, Details = details)
        }, simplify = FALSE))
        print(as.data.frame(auxtable), row.names = FALSE, right = FALSE)
    }else{
        cat("\nNo auxilliary data\n")
    }

    ##################################################
    ## covariates

    if(is.null(covlevels)){
        cat("\nNo covariate data\n")
    }else{
        cat("\nCovariates:\n")
        covlevels = covlevels(capthist)
        covtable = do.call(rbind, sapply(names(covlevels), function(covlevel){
            # covlevel = "sessioncov"
            # covlevel = "trapcov"
        # covlevels = c("sessioncov", "trapcov", "timecov", "timevaryingcov")
#         covtable = do.call(rbind, sapply(covlevels, function(covlevel){
            # covariate level
            level = switch(covlevel,
                           "sessioncov"     = "array",
                           "trapcov"        = "post",
                           "timecov"        = "day",
                           "timevaryingcov" = "post-day")

            # extract covariates from capthist
            data = switch(covlevel,
                          "sessioncov" = sessioncov(capthist),
                          "timecov"    = timecov(capthist),
                          trapcov(capthist)
            )
            # if data is a list, collapse it into a single dataframe
            if(!inherits(data, "data.frame"))
                data = do.call(rbind, data)
            # trapcov and timevaryingcov need to be handled differently
            if(!is.null(data)){
                if(covlevel %in% c("trapcov", "timevaryingcov")){
                    tvc = timevaryingcov(traps(capthist))
                    if(!is.null(tvc)){
                        # tvc columns
                        to.delete = colnames(data) %in% unique(sapply(tvc, names))
                        # if timevaryingcov, need to delete the non-tvc columns
                        if(covlevel == "timevaryingcov") to.delete = !to.delete
                        # delete columns
                        # - not using subsetting here to preserve column names
                        for(i in sort(which(to.delete), decreasing = TRUE))
                            data[[i]] = NULL
                    }
                }
            }
            if(is.null(data) || ncol(data) == 0) return(NULL)
            # summarise covariate
            covtable = covtable(data[,covlevels[[covlevel]], drop = FALSE])
            covtable = cbind(Level = c(level, rep("", nrow(covtable) - 1)), covtable)
            return(covtable)
        }))
        print(as.data.frame(covtable), quote = FALSE, row.names = FALSE,
              right = FALSE)
        # rownames(covtable) = ""
        # print(covtable, quote = FALSE, row.names = FALSE, right = FALSE)
    }

    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Summarise a fitted model
#' @description Summarise the results of a model fitted with
#'   \code{\link{gibbonsecr_fit}}.
#' @inheritParams coef.gibbonsecr_fit
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @seealso \link{gibbonsecr_fit}
#' @method summary gibbonsecr_fit
#' @export

summary.gibbonsecr_fit = function(object, ...){

    if(object$nlm$code >= 3){
        warning(paste0("Fitting algorithm did not converge (nlm code: ", object$nlm$code, ")"))
    }else if(object$nlm$iterations == 0){
        warning(paste0("Fitting algorithm did not converge (n iterations: ", object$nlm$iterations, ")"))
    }else{

        ##################################################
        ## model

        cat("Model options:\n")
        cat(" detection func.", switch(object$model.options$detectfn + 1,
                                       "half normal",
                                       "hazard rate"), "\n")
        cat(" bearings dist. ", switch(object$model.options$bearings  + 1,
                                       "none",
                                       "von mises",
                                       "wrapped cauchy"), "\n")
        cat(" distances dist.", switch(object$model.options$distances + 1,
                                       "none",
                                       "gamma",
                                       "log-normal"), "\n")
        cat("\n")

        ##################################################
        ## mask

        # cat("Mask:\n")
        # summary_mask(object$mask, object$capthist)
        # cat("\n")

        ##################################################
        ## formulas

        cat("Formulas:\n")
        nspace = max(nchar(names(object$parindx)))
        for(i in names(object$parindx)){
            cat(rep(" ", 1 + nspace - nchar(i)),
                paste(i, paste(object$model[[i]], collapse = " ")), "\n", sep = "")
        }
        cat("\n")

        ##################################################
        ## fixed

        if(length(object$fixed) == 0){
            cat("No fixed parameters\n")
        }else{
            cat("Fixed parameters:\n")
            nspace = max(nchar(names(object$fixed)))
            for(i in names(object$fixed)){
                cat(rep(" ", 1 + nspace - nchar(i)),
                    paste(i, "=", object$fixed[[i]]), "\n", sep = "")
            }
            # temp = cbind(value = unlist(object$fixed)) ; temp
            # rownames(temp) = paste0(" ", names(object$fixed))
            # print(temp)
        }

        cat("\n")

        ##################################################
        ## parameter estimates

        cat("Estimates (model intercepts on inverse link scale):\n")
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
        submodels = rownames(par.table) = gsub("\\.\\(Intercept\\)", "",
                                               rownames(par.table))
        # convert to response scale
        for(i in submodels)
            par.table[i,] = object$inv.link[[i]](par.table[i,])
        # pretty numbers
        par.table = as.data.frame(do.call(rbind, {
            sapply(rownames(par.table), function(i){
                pretty_numbers(par.table[i,], 6)
            }, simplify = FALSE)
        }))
        colnames(par.table) = col.names
        # add units
        par.table$units = sapply(rownames(par.table), function(i){
            switch(i,
                   "D" = "groups / sq km",
                   "sigma" = "metres",
                   "")
        })
        # print(par.table, digits = 5)
        print(par.table)
        cat("\n")

        ##################################################
        ## esa, aic and run time

        esa = get_esa(object)
        cat("Effective sampling area:", round(mean(esa), 3),
            "sq km (average per array)\n")
        cat("\nAIC =", AIC(object), "\n")
        cat("\nTime taken: ", round(object$run.time,1), " ",
            attr(object$run.time, "units"), " (", object$nlm$iterations,
            " iterations)\n", sep = "")
        # cat("\n")

    }

    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

summary_mask = function(mask, traps = NULL){

    ##################################################
    ## checks

    if(inherits(mask, "gibbonsecr_fit")){
        traps = traps(mask$capthist)
        mask = mask$mask
    }
    if(!inherits(mask, "mask"))
        stop("mask object required")
    if(!is.null(traps))
    if(!inherits(mask, "mask"))
        stop("mask object required")

    ##################################################
    ## summary stats

    cat("Summary stats:\n")
    if(!is.null(traps)){
        cat(" buffer    ", max(mask_buffer(mask, traps)), "m\n")
    }
    cat(" spacing   ", mask_spacing(mask), "m\n")
    cat(" npoints   ", round(mean(mask_npoints(mask))), "(average per array)\n")
    cat(" area      ", round(mean(mask_Area(mask))), "sq km (average per array)\n")

    ##################################################
    ## covariates

    cat("\nCovariates:\n")
    data = do.call(rbind, maskcov(MS(mask)))
    covtable = covtable(data)
    print(as.data.frame(covtable), quote = FALSE, row.names = FALSE, right = FALSE)
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
        # if value is NULL then set value to list of NULLs
        if(is.null(value))
            value = sapply(session(capthist), function(i) NULL, simplify = FALSE)
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

trapcov = function(traps){
    if(inherits(traps, "capthist"))
        traps = traps(traps)
    if(!inherits(traps, "traps"))
        stop("requires a 'traps' object")
    if(ms(traps)){
        sapply(traps, trapcov, simplify = FALSE)
    }else{
        if(is.null(covariates(traps))){
            as.data.frame(traps)
        }else{
            cbind(as.data.frame(traps), covariates(traps))
        }
    }
}

'trapcov<-' = function(traps, value){
    capt = inherits(traps, "capthist")
    if(capt){
        capthist = traps
        traps = traps(capthist)
    }
    if(!inherits(traps, "traps"))
        stop("requires a 'traps' object")
    if(ms(traps)){
        # if value is NULL then set value to list of NULLs
        if(is.null(value))
            value = sapply(session(traps), function(i) NULL, simplify = FALSE)
        if(!inherits(value, "list") || inherits(value, "data.frame")){
            stop("expecting a list")
        }else{
            if(length(value) != length(traps)){
                stop("length(trapcov) should equal nsessions")
            }
            for(i in 1:length(value)){
                trapcov(traps[[i]]) <- value[[i]]
            }
            traps
        }
    }else{
        if(!is.null(value)){
            if(!inherits(value, "data.frame")){
                stop("expecting a data.frame")
            }else{
                if(nrow(value) != n_occasions(traps))
                    stop("nrow(trapcov) should be equal to number of occasions")
            }
        }
        if(capt){
            structure(traps(capthist), covariates = value)
        }else{
            structure(traps, covariates = value)
        }
    }
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

make_regiontraps = function(traps){
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
    # usage(regiontraps) = do.call(rbind, lapply(traps, usage))
    attr(regiontraps, "spacex") = NA
    attr(regiontraps, "spacey") = NA
    attr(regiontraps, "spacing") = NA
    return(regiontraps)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Compute the variance-covariance matrix
#' @description Compute the variance-covariance from a model fitted with
#'   \code{\link{gibbonsecr_fit}}.
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
