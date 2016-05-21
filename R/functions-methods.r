
add_scaled_xy = function(x, meanSD){
    if(!inherits(x, c("gcapthist", "gmask")))
        stop("expecting a gcapthist or gmask object")
    sessions = session(x)
    for(session in sessions){
        data = if(inherits(x, "gmask")){
            x[[session]]
        }else{
            traps(x[[session]])
        }
        coords = as.data.frame(mapply(function(x, y, z) ((x - y) / z),
                                      x = data,
                                      y = meanSD[1,],
                                      z = meanSD[2,]))
        rownames(coords) = rownames(data)
        if(is.null(covariates(data))){
            covariates(data) = coords
        }else{
            covariates(data) = cbind(covariates(data), coords)
        }
        attr(data, "meanSD") = meanSD
        data = if(inherits(x, "gmask")){
            x[[session]] = data
        }else{
            traps(x[[session]]) = data
        }
    }
    return(x)
}

# as methods ===================================================================

# @export
# as_secr = function(x, ...){
#     UseMethod("as_secr")
# }

# @export
# as_secr.default = function(x, ...){
#     return(x)
# }

# @export
# as_secr.gcapthist = function(x, ...){
#     message("not yet implemented")
#     invisible(x)
# }

# @export
# as_secr.gmask = function(x, ...){
#     message("not yet implemented")
#     invisible(x)
# }

# @title Convert a gibbonsecr capthist or mask to \pkg{secr} package format
# @description Convenience function to convert a
#   \link[gibbonsecr]{gcapthist} object to \link[secr]{capthist}
#   format, or a \link[gibbonsecr]{gmask} object to \link[secr]{mask}
#   format. The resulting objects can then be used for example by
#   \link[secr]{secr.fit}.
# @param x a \link[gibbonsecr]{gcapthist} or
#   \link[gibbonsecr]{gmask} object
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @example inst/examples/examples-N.annamensis-analysis.r
# @export

# as_secr_old = function(x){
#     if(inherits(x, c("gcapthist", "gmask"))){
#         if(inherits(x, "gcapthist")){
#             class(x) = c("list", "capthist")
#             for(i in 1:length(x)) class(x[[i]]) = "capthist"
#         }
#         if(inherits(x, "gmask")){
#             class(x) = c("list", "mask")
#             for(i in 1:length(x)) class(x[[i]]) = c("mask", "data.frame")
#         }
#     }else{
#         warning("input not recognised, returning it unchanged")
#     }
#     return(x)
# }

# general methods ==============================================================

#' @method AIC gsecr
#' @export
AIC.gsecr = function(object, ..., k = 2){
    - 2 * logLik(object) + k * length(coef(object))
}

#' @method coef gsecr
#' @export
coef.gsecr = function(object, ...){
    object$nlm$estimate
}

#' @method confint gsecr
#' @export
confint.gsecr = function(object, parm, level = 0.95, ...){
    confint.default(object, parm, level, ...)
}

#' @method logLik gsecr
#' @export
logLik.gsecr = function(object, ...){
    -object$nlm$minimum
}

#' @importFrom MASS ginv
#' @method vcov gsecr
#' @export
vcov.gsecr = function(object, ...){
    if(is.null(object$nlm$hessian)){
        stop("no hessian")
    }else{
        vcov = try(ginv(object$nlm$hessian), silent = TRUE)
        if(inherits(vcov, "try-error")){
            return(NULL)
        }else{
            dimnames(vcov) = dimnames(object$nlm$hessian)
            return(vcov)
        }
    }
}


# bearings =====================================================================

# add bearings data to capthist
# need to add check for dimensions
add_bearings = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("expecting a 'capthist' object")
    attr(capthist, "bearings") = value
    return(capthist)
}

calc_bearings = function(x, y, ...){
    UseMethod("calc_bearings")
}

calc_bearings.default = function(x, y, ...){
    warning.text = "x and y must be matrices or data.frames"
    if(!inherits(x, c("matrix", "data.frame")) ||
       !inherits(y, c("matrix", "data.frame")))
        stop(warning.text)
    if(ncol(x) != 2 || ncol(y) != 2)
        stop(warning.text)
    calc_bearings_rcpp(as.matrix(x), as.matrix(y))
}

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

# get bearings data from capthist
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


# distances ====================================================================

# add distances data to capthist
# need to add check for dimensions
add_distances = function(capthist, value){
    if(!inherits(capthist, "capthist"))
        stop("expecting a 'capthist' object")
    attr(capthist, "distances") = value
    return(capthist)
}

calc_distances = function(x, y, ...){
    UseMethod("calc_distances")
}

calc_distances.default = function(x, y, ...){
    warning.text = "x and y must be matrices or data.frames"
    if(!inherits(x, c("matrix", "data.frame")) ||
       !inherits(y, c("matrix", "data.frame")))
        stop(warning.text)
    if(ncol(x) != 2 || ncol(y) != 2)
        stop(warning.text)
    calc_distances_rcpp(x, y)
}

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
        dists = calc_distances_rcpp(as.matrix(x), as.matrix(y))
        dimnames(dists) = list(rownames(y), rownames(x))
        return(dists)
    }), session.names)
    if(!ms(x) && !ms(y)) distances = distances[[1]]
    return(distances)
}

# get distances data from capthist
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


# predict methods ==============================================================

predict_submodel = function(fit, beta = NULL, submodel = "D", newdata = NULL){
    # check inputs ----------------------------------------------------------- #
    if(is.null(beta)) beta = coef(fit)
    if(!submodel %in% names(fit$parindx)) stop("no model for ", submodel)
    if(is.null(newdata)) newdata = default_newdata(fit, submodel)
    ## model matrix ---------------------------------------------------------- #
    # use list("1" = ... to make dummy session
    design.matrices = list("1" = sapply(submodel, function(submodel){
        make_model_matrix(data         = newdata,
                          smooth.setup = fit$smooth.setup[[submodel]])
    }, simplify = FALSE))
    # submodel arrays -------------------------------------------------------- #
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

# get point estimates for 1D submodels
predict_1D_point = function(fit, x, which = "detfunc", beta = NULL, newdata = NULL, true.distance = 500){
    # check imputs ----------------------------------------------------------- #
    if(!which %in% c("detfunc","bearings","distances"))
        stop("which must be one of: 'detfunc', 'bearings', 'distances'")
    if(is.null(beta)) beta = coef(fit)
    # submodels -------------------------------------------------------------- #
    submodels = if(which == "detfunc"){
        switch(fit$model.options$detfunc + 1,
               c("g0","sigma"), # half normal
               c("g0","sigma","z")) # hazard rate
    }else which
    # check model exists
    for(submodel in submodels)
        if(fit$model[[submodel]] == 0) stop("no ", submodel, " model")
    # function to generate values -------------------------------------------- #
    FUN = switch(
        which,
        "detfunc"  = switch(fit$model.options$detfunc + 1, hn, hr),
        "bearings"  = switch(fit$model.options$bearings, dvm, dwrpcauchy),
        "distances" = switch(fit$model.options$distances, dgamma, dlnorm)
    )
    # mean of bearings / distances distribution
    EX = switch(which, "bearings" = 0, "distances" = true.distance, NULL)
    # design matrices -------------------------------------------------------- #
    if(is.null(newdata)) newdata = default_newdata(fit, submodels)
    design.matrices = sapply(submodels, function(submodel){
        make_model_matrix(data         = newdata,
                          smooth.setup = fit$smooth.setup[[submodel]])
    }, simplify = FALSE)
    # get sumbmodel values --------------------------------------------------- #
    # "1" represents a dummy session
    submodel.arrays = make_submodel_arrays(
        beta            = beta,
        parindx         = fit$parindx,
        fixed           = fit$fixed,
        design.matrices = list("1" = design.matrices),
        inv.link        = fit$inv.link,
        S               = c("1" = 1),
        K               = c("1" = 1),
        submodels       = submodels,
        sessions        = "1"
    )[["1"]]
    # return function values ------------------------------------------------- #
    par = as.numeric(submodel.arrays)
    FUN(x, par, EX)
}

# get interval estimates for 1D submodels
#' @importFrom MASS mvrnorm
predict_1D_interval = function(fit, x, which = "detfunc", level = 0.95, method = "delta", nboot = 999, newdata = NULL, true.distance = 500, ...){

    # check inputs ----------------------------------------------------------- #
    if(!inherits(fit, "gsecr")) stop("expecting a gsecr object")
    if(!which %in% c("detfunc","bearings","distances"))
        stop("which must be one of: 'detfunc', 'bearings', 'distances'")
    if(is.null(newdata)) newdata = default_newdata(fit, which)

    # predict_1D_point args -------------------------------------------------- #
    args = list(beta = coef(fit), fit = fit, x = x, newdata = newdata,
                which = which, true.distance = true.distance)

    # delta method ----------------------------------------------------------- #
    if(method == "delta"){
        args$f = predict_1D_point
        args$vcov = vcov(fit)
        values = do.call(delta_method, args)
        zvalue = -qnorm((1 - level) / 2)
        values$lower = values$estimate - zvalue * values$se
        values$upper = values$estimate + zvalue * values$se
        values$se = NULL
        values = as.data.frame(values)
    }

    # boot method ------------------------------------------------------------ #
    if(method == "boot"){
        # sample nboot values from sampling distribution of fitted coefs
        boot = mvrnorm(nboot, coef(fit), vcov(fit))
        # get predicted function for each set of bootstrap parameters
        values = sapply(1:nboot, function(i){
            args$beta = boot[i,]
            do.call(predict_1D_point, args)
        })
        # get quantiles for each point on x axis
        probs = c((1 - level) / 2, 1 - (1 - level) / 2)
        values = as.data.frame(
            do.call(rbind, lapply(1:nrow(values), function(i){
                quantile(values[i,], prob = probs)
            })))
        colnames(values) = c("lower", "upper")
        args$beta = NULL
        values$estimate = do.call(predict_1D_point, args)
    }
    return(values)
}

# get point estimates for 2D submodels
predict_2D_point = function(fit, mask, traps, which = "detsurf", session = 1, beta = NULL, newdata = NULL){
    # check inputs ----------------------------------------------------------- #
    if(is.null(newdata)){
        submodels = switch(which,
                           "densurf" = "D",
                           "detsurf" = c("g0", "sigma", "z", "pcall"))
        newdata = default_newdata(fit, submodels)
    }
    if(!inherits(fit, "gsecr")) stop("expecting a gsecr object")
    if(!inherits(mask, "gmask")) stop("expecting a gmask object")
    if(!which %in% c("detsurf", "densurf"))
        stop("which must be one of: 'pdot', 'density")
    if(is.null(beta)) beta = coef(fit)
    # submodels -------------------------------------------------------------- #
    submodels = switch(
        which,
        "densurf" = "D",
        "detsurf"    = switch(fit$model.options$detfunc + 1,
                           c("g0","sigma","pcall"),
                           c("g0","sigma","z","pcall"))
    )
    # design matrices -------------------------------------------------------- #
    if(which == "densurf"){
        # make a model frame and design matrix
        model.frame = make_model_frames(
            model       = fit$model,
            traps       = traps,
            mask        = mask,
            n_occasions = n_occasions(fit$capthist),
            sessioncov  = sessioncov(fit$capthist),
            timecov     = timecov(fit$capthist),
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
    if(which == "detsurf"){
        # can use the original design matrix for this session
        # since the sessionmask will have been
        design.matrices = fit$design.matrices[session]
    }
    # get sumbmodel values --------------------------------------------------- #
    M = size(fit$mask)
    # M = mask_npoints(fit$mask)
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
    # return function values ------------------------------------------------- #
    if(which == "detsurf"){
        values = calc_pdot(
            detfunc  = fit$model.options$detfunc,
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
    if(which == "densurf"){
        values = as.numeric(submodel.arrays[[session]][["D"]])
    }
    return(values)
}

# get interval estimates for 2D submodels
predict_2D_interval = function(fit, x, which = "detfunc", level = 0.95, method = "delta", nboot = 999, newdata = NULL, true.distance = 500, ...){
    # delta method ----------------------------------------------------------- #
    if(method == "delta"){
        delta_args = list(beta    = coef(fit),
                          fit     = fit,
                          mask    = fit$mask,
                          traps   = traps(fit$capthist),
                          which   = which,
                          session = session,
                          newdata = newdata)
        delta_args$f    = predict_2D_point
        delta_args$vcov = vcov(fit)
        values = do.call(delta_method, delta_args)
        zvalue = -qnorm((1 - level) / 2)
        values$lower = values$estimate - zvalue * values$se
        values$upper = values$estimate + zvalue * values$se
        values$se = NULL
    }
    return(values)
}

#' @method predict gsecr
#' @export
predict.gsecr = function(object, newdata = NULL, submodels = NULL, type = c("response", "link"), se.fit = TRUE, level = 0.95, method = "delta", nboot = 999, ...){
    type = match.arg(type)
    if(is.null(submodels))
        submodels = names(object$parindx)
    # if newdata not supplied, make default dataframe of covariate values
    # using averages / factor reference levels
    if(is.null(newdata))
        newdata = default_newdata(object, submodels)

    # check bounds ----------------------------------------------------------- #
    # warn if outside observed range
    # check_newdata(newdata, object)

    # predict_1D_point args -------------------------------------------------- #
    args = list(beta = coef(object), fit = object, newdata = newdata)

    # estimates only --------------------------------------------------------- #
    if(!se.fit){
        values = sapply(submodels, function(submodel){ # submodel = "D"
            args$submodel = submodel
            values = list(estimate = do.call(predict_submodel, args))
            values = as.data.frame(values)
            return(values)
        }, simplify = FALSE)
    }

    # delta method ----------------------------------------------------------- #
    if(se.fit && method == "delta"){
        values = sapply(submodels, function(submodel){ # submodel = "D"
            args$submodel = submodel
            args$f    = predict_submodel
            args$vcov = vcov(object)
            delta     = do.call(delta_method, args)
            zvalue    = -qnorm((1 - level) / 2)
            delta$lower = delta$estimate - zvalue * delta$se
            delta$upper = delta$estimate + zvalue * delta$se
            delta$se = NULL
            as.data.frame(delta)
        }, simplify = FALSE)
    }

    # boot method ------------------------------------------------------------ #
    if(se.fit && method == "boot"){
        boot = mvrnorm(nboot, coef(object), vcov(object))
        values = sapply(submodels, function(submodel){ # submodel = "D"
            args$submodel = submodel
            values = do.call(cbind, sapply(1:nboot, function(i){
                args$beta = boot[i,]
                do.call(predict_submodel, args)
            }, simplify = FALSE))
            probs = c((1 - level) / 2, 1 - (1 - level) / 2)
            values = as.data.frame(
                do.call(rbind, lapply(1:nrow(values), function(i){
                    quantile(values[i,], prob = probs)
                }))
            )
            colnames(values) = c("lower", "upper")
            args$beta = NULL
            cbind(estimate = do.call(predict_submodel, args), values)
        }, simplify = FALSE)
    }

    # inverse link ----------------------------------------------------------- #
    if(type == "response"){
        values = sapply(submodels, function(submodel){ # submodel = "D"
            object$inv.link[[submodel]](values[[submodel]])
        }, simplify = FALSE)
    }

    return(values)
}


# print methods ================================================================

#' @method print gcapthist
#' @export
print.gcapthist = function(x, ...){
    summary(x)
}

#' @method print gsecr
#' @export
print.gsecr = function(x, ...){
    summary(x)
}

#' @method print gmask
#' @export
print.gmask = function(x, ...){
    # summary(x)
    print(lapply(x, dplyr::tbl_df))
    invisible()
}

#' @method print summary_gmask
#' @export
print.summary_gmask = function(x, ...){
    # summary stats
    cat("Summary stats:\n")
    cat(" buffer    ", x$buffer, "m\n")
    cat(" spacing   ", x$spacing, "m\n")
    cat(" npoints   ", round(mean(x$size)), "(average per array)\n")
    cat(" area      ", round(mean(x$Area)), "sq km (average per array)\n")
    # covariates
    if(is.null(x$covtable)){
        cat("\nNo covariates\n")
    }else{
        cat("\nCovariates:\n")
        print(x$covtable, quote = FALSE, row.names = FALSE, right = FALSE)
    }
}

#' @method print summary_gcapthist
#' @export
print.summary_gcapthist = function(x, ...){
}

# subset methods ===============================================================

# @rdname subset_capthist_mask
# @name subset.gcapthist
# @title Subset a gibbonsecr capthist or mask
# @description Subset the sessions in a gibbonsecr capthist or mask object.
# @param x a \link[gibbonsecr]{gcapthist} or
#   \link[gibbonsecr]{gmask} object.
# @param subset a logical, integer or character vector indicating which
#   sessions to keep.
# @param ... additional arguments (not used)
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @method subset gcapthist
#' @export

subset.gcapthist = function(x, subset = NULL, ...){
    if(is.null(subset)) return(x)
    subcapt = x[subset]
    class(subcapt) = class(x)
    if(!is.null(sessioncov(x)))
        sessioncov(subcapt) = sessioncov(x)[subset, , drop = FALSE]
    return(subcapt)
}

# @rdname subset_capthist_mask
# @name subset.gmask
#' @method subset gmask
#' @export

subset.gmask = function(x, subset = NULL, ...){
    if(is.null(subset)) return(x)
    submask = x[subset]
    class(submask) = class(x)
    attr(submask, "buffer")  = buffer(x)
    attr(submask, "spacing") = spacing(x)
    attr(submask, "area")    = area(x)
    return(submask)
}

# summary methods ==============================================================

# helper function for summary methods
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

# prints a summary of capthist data
# an alternative to secr::summary.capthist which is quite verbose

# @title TODO
# @description TODO
# @inheritParams add_bearings
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @method summary gcapthist
#' @export

summary.gcapthist = function(object, ...){

    ## detections ----------------------------------------------------------- ##

    cat("Detections:\n")
    detections.table = rbind(
        "ngroups"     = n_groups(object),
        "ndetections" = n_detections(object),
        "noccasions"  = n_occasions(object),
        "nposts"      = n_traps(object)
    )
    detections.table = cbind(detections.table,
                             total = apply(detections.table, 1, sum))
    detections.table = cbind(array = c("ngroups","ndetections","ndays","nposts"),
                             detections.table)
    print(as.data.frame(detections.table), quote = FALSE, row.names = FALSE)
    cat("\nRecaptures =", round(100 * mean(do.call(c, lapply(object, as.numeric)),
                                           na.rm = TRUE), 1), "%\n\n")

    ## bearings and distances ----------------------------------------------- ##

    aux.data = sapply(c("bearings", "distances"), function(aux){
        any(sapply(object, function(x) !is.null(attr(x, aux))))
    })
    if(any(aux.data)){
        cat("Auxilliary data:\n")
        auxtable = do.call(rbind, sapply(names(aux.data)[aux.data], function(i){
            # i="bearings"
            units = unique(do.call(c, lapply(object, function(x){
                if(is.null(attr(x, i))) NULL else{
                    attr(attr(x, i), "details")$units
                }
            })))
            type = unique(do.call(c, lapply(object, function(x){
                if(is.null(attr(x, i))) NULL else{
                    attr(attr(x, i), "details")$type
                }
            })))
            details = if(type == "continuous"){
                paste("range:", paste(prettyNum(range(
                    lapply(object, function(x){
                        values = as.numeric(attr(x, i))
                        if(length(values) == 0) return(NA) else return(values)
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

    ## covariates ----------------------------------------------------------- ##

    if(is.null(covlevels)){
        cat("\nNo covariate data\n")
    }else{
        cat("\nCovariates:\n")
        covlevels = covlevels(object)
        covtable = do.call(rbind, sapply(names(covlevels), function(covlevel){
            # covariate level
            level = switch(covlevel,
                           "sessioncov"     = "array",
                           "trapcov"        = "post",
                           "timecov"        = "day",
                           "timevaryingcov" = "post-day")

            # extract covariates from capthist
            data = switch(covlevel,
                          "sessioncov" = sessioncov(object),
                          "timecov"    = timecov(object),
                          trapcov(object)
            )
            # if data is a list, collapse it into a single dataframe
            if(!inherits(data, "data.frame"))
                data = do.call(rbind, data)
            # trapcov and timevaryingcov need to be handled differently
            if(!is.null(data)){
                if(covlevel %in% c("trapcov", "timevaryingcov")){
                    tvc = timevaryingcov(traps(object))
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
            covtable = covtable(data[, covlevels[[covlevel]], drop = FALSE])
            covtable = cbind(Level = c(level, rep("", nrow(covtable) - 1)), covtable)
            return(covtable)
        }, simplify = FALSE))
        print(as.data.frame(covtable), quote = FALSE, row.names = FALSE, right = FALSE)
    }

    invisible()
}

## -------------------------------------------------------------------------- ##

# @title Summarise a fitted model
# @description Summarise the results of a model fitted with
#   \link[gibbonsecr]{gsecr}.
# @inheritParams coef.gsecr
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @example inst/examples/examples-N.annamensis-analysis.r
# @seealso \link[gibbonsecr]{gsecr}
#' @method summary gsecr
#' @export

summary.gsecr = function(object, ...){

    if(object$nlm$code >= 3){
        warning(paste0("Fitting algorithm did not converge (nlm code: ",
                       object$nlm$code, ")"))
    }else if(object$nlm$iterations == 0){
        warning(paste0("Fitting algorithm did not converge (n iterations: ",
                       object$nlm$iterations, ")"))
    }else{

        ## model ------------------------------------------------------------ ##

        cat("Model options:\n")
        cat(" detection func.", switch(object$model.options$detfunc + 1,
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

        ## mask ------------------------------------------------------------- ##

        # cat("Mask:\n")
        # summary(object$mask, object$capthist)
        # cat("\n")

        ## formulas --------------------------------------------------------- ##

        cat("Formulas:\n")
        nspace = max(nchar(names(object$parindx)))
        for(i in names(object$parindx)){
            cat(rep(" ", 1 + nspace - nchar(i)),
                paste(i, paste(object$model[[i]], collapse = " ")), "\n", sep = "")
        }
        cat("\n")

        ## fixed ------------------------------------------------------------ ##

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

        ## parameter estimates ---------------------------------------------- ##

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

        ## esa, aic and run time -------------------------------------------- ##

        esa = get_esa(object)
        cat("Effective sampling area:", round(mean(esa), 3),
            "sq km (average per array)\n")
        cat("\nAIC =", AIC(object), "\n")
        cat("\nTime taken: ", round(object$run.time,1), " ",
            attr(object$run.time, "units"), " (", object$nlm$iterations,
            " iterations)\n", sep = "")

    }

    invisible()
}

## -------------------------------------------------------------------------- ##

#' @method summary gmask
#' @export
summary.gmask = function(object, ...){
    out = list(
        buffer = buffer(object),
        spacing = spacing(object),
        size = size(object),
        area = area(object),
        Area = Area(object)
    )
    covs = do.call(rbind, covariates(object))
    if(!is.null(covs)) out$covtable = as.data.frame(covtable(covs))
    class(out) = "summary_gmask"
    return(out)
}


# capthist attributes ==========================================================

# @rdname capthist_attributes
# @name n_arrays
# @title Extract capthist attributes
# @description Extract attribute data from a gibbonsecr capthist
# @param x a \link[gibbonsecr]{gcapthist} object
# @examples
# data(N.annamensis)
# n_arrays(N.annamensis.capthist)
# n_traps(N.annamensis.capthist)
# n_occasions(N.annamensis.capthist)
# n_groups(N.annamensis.capthist)
# n_detections(N.annamensis.capthist)
# @export
n_arrays = function(x){
    if(!inherits(x, "gcapthist"))
        stop("expecting a 'gcapthist' object")
    length(x)
}

# @rdname capthist_attributes
# @name n_traps
# @export
n_traps = function(x){
    if(!inherits(x, "capthist"))
        stop("expecting a 'capthist' object")
    FUN = function(x) nrow(traps(x))
    if(ms(x)) sapply(x, FUN) else FUN(x)
}

# @rdname capthist_attributes
# @name n_occasions
# @export
n_occasions = function(x){
    if(!inherits(x, "capthist"))
        stop("expecting a 'capthist' object")
    FUN = function(x) dim(usage(traps(x)))[2]
    if(ms(x)) sapply(x, FUN) else FUN(x)
}

# @rdname capthist_attributes
# @name n_groups
# @export
n_groups = function(x){
    if(!inherits(x, "capthist"))
        stop("expecting a 'capthist' object")
    FUN = function(x) if(is.null(dim(x))) 0 else dim(x)[1]
    if(ms(x)) sapply(x, FUN) else FUN(x)
}

# @rdname capthist_attributes
# @name n_detections
# @export
n_detections = function(x){
    if(!inherits(x, "capthist"))
        stop("expecting a 'capthist' object")
    FUN = function(x) if(is.null(dim(x))) 0 else sum(x)
    if(ms(x)) sapply(x, FUN) else FUN(x)
}


# capthist covariates ==========================================================

sessioncov = function(x){
    if(!inherits(x, "gcapthist"))
        stop("expecting a 'gcapthist' object")
    attr(x, "sessioncov")
}

'sessioncov<-' = function(x, value){
    if(!inherits(x, "gcapthist"))
        stop("expecting a 'gcapthist' object")
    if(!is.null(value)){
        if(!inherits(value, "data.frame"))
            stop("expecting a data.frame")
        if(nrow(value) != length(x))
            stop("nrow(sessioncov) should be equal to nsessions")
        if(!all(rownames(value) == session(x))){
            message("changing rownames(sessioncov) to equal session names")
            rownames(value) = session(x)
        }
    }
    structure(x, sessioncov = value)
}

## -------------------------------------------------------------------------- ##

timecov = function(x){
    if(!inherits(x, "capthist"))
        stop("expecting a 'capthist' object")
    if(ms(x)){
        sapply(x, function(x) attr(x, "timecov"), simplify = FALSE)
    }else{
        attr(x, "timecov")
    }
}

'timecov<-' = function(x, value){
    if(!inherits(x, "capthist"))
        stop("expecting a 'capthist' object")
    if(inherits(x, "gcapthist")){
        # if value is NULL then set value to list of NULLs
        if(is.null(value))
            value = sapply(session(x), function(i) NULL, simplify = FALSE)
        if(length(value) != length(x)){
            stop("length(timecov) should equal nsessions")
        }
        for(i in 1:length(value)){
            timecov(x[[i]]) <- value[[i]]
        }
        x
    }else{
        if(!is.null(value)){
            if(!inherits(value, "data.frame")){
                stop("expecting a data.frame")
            }else{
                if(nrow(value) != n_occasions(x))
                    stop("nrow(timecov) should be equal to number of occasions")
            }
        }
        structure(x, timecov = value)
    }
}

## -------------------------------------------------------------------------- ##

trapcov = function(traps){
    if(inherits(traps, "capthist"))
        traps = traps(traps)
    if(!inherits(traps, "traps"))
        stop("expecting a 'traps' object")
    if(ms(traps)){
        sapply(traps, trapcov, simplify = FALSE)
    }else{
        # covariates(traps)
        if(is.null(covariates(traps))){
            as.data.frame(traps)
        }else{
            cbind(as.data.frame(traps), covariates(traps))
        }
    }
}

# 'trapcov<-' = function(traps, value){
#     capt = inherits(traps, "capthist")
#     if(capt){
#         capthist = traps
#         traps = traps(capthist)
#     }
#     if(!inherits(traps, "traps"))
#         stop("expecting a 'traps' object")
#     if(ms(traps)){
#         # if value is NULL then set value to list of NULLs
#         if(is.null(value))
#             value = sapply(session(traps), function(i) NULL, simplify = FALSE)
#         if(!inherits(value, "list") || inherits(value, "data.frame")){
#             stop("expecting a list")
#         }else{
#             if(length(value) != length(traps)){
#                 stop("length(trapcov) should equal nsessions")
#             }
#             for(i in 1:length(value)){
#                 trapcov(traps[[i]]) <- value[[i]]
#             }
#             traps
#         }
#     }else{
#         if(!is.null(value)){
#             if(!inherits(value, "data.frame")){
#                 stop("expecting a data.frame")
#             }else{
#                 if(nrow(value) != n_occasions(traps))
#                     stop("nrow(trapcov) should be equal to number of occasions")
#             }
#         }
#         if(capt){
#             structure(traps(capthist), covariates = value)
#         }else{
#             structure(traps, covariates = value)
#         }
#     }
# }

## -------------------------------------------------------------------------- ##

traps_bbox = function(x){
    if(inherits(x, "gsecr"))
        x = traps(x$capthist)
    if(inherits(x, "capthist"))
        x = traps(x)
    if(!inherits(x, "traps"))
        stop("expecting a 'traps' object")
    regiontraps = make_regiontraps(x)
    lim = apply(regiontraps, 2, range)
    bbox = cbind(x = lim[c(1,2,2,1), 1],
                 y = lim[c(1,1,2,2), 2])
    return(bbox)
}

traps_meanSD = function(capthist){
    if(!inherits(capthist, "gcapthist"))
        stop("expecting a 'gcapthist' object")
    traps = make_regiontraps(capthist, ms = FALSE)
    secr::getMeanSD(traps)
}

# bespoke methods ==============================================================

covlevels = function(x){
    covlevels = list()
    if(inherits(x, "capthist")){
        SS = if(ms(x)) x[[1]] else x
        covlevels$groupcov   = colnames(covariates(SS))
        covlevels$sessioncov = colnames(attr(x, "sessioncov"))
        covlevels$timecov    = colnames(attr(SS, "timecov"))
        covlevels$trapcov    = if(is.null(covariates(traps(SS)))){
            NULL
        }else{
            sort(unique(colnames(covariates(traps(SS)))))
        }
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
        covlevels$maskcov = if(is.null(covariates(x))){
            NULL
        }else{
            sort(colnames(covariates(x)))
        }
    }
    covlevels = if(length(covlevels) == 0) NULL else{
        lapply(covlevels, sort)
    }
    return(covlevels)
}

covtable = function(data, w = 50){
    covtable = do.call(rbind, lapply(colnames(data), function(i){
        factor = inherits(data[[i]], "factor")
        info = c(Name = paste0("'", i, "'"),
                 Type = if(factor) "category" else "number",
                 Details = if(factor){
                     paste0(length(levels(data[[i]])), " levels: ",
                            paste0(levels(data[[i]]),
                                   collapse = ", "))
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

# @rdname get_bearings_captures_distances
# @name get_captures
# @export
get_captures = function(capthist, summarise = NULL){
    if(!inherits(capthist, "gcapthist"))
        stop("expecting a 'gcapthist' object")
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

get_esa = function(fit){
    if(!inherits(fit, "gsecr"))
        stop("expecting a gsecr object")

    # shortcut if D model is intercept only
    # otherwise need to calculate
    esa = if(fit$model$D == ~ 1){

        n_groups(fit$capthist) / fit$inv.link$D(coef(fit)[fit$parindx$D])

    }else{

        calc_esa(
            beta            = coef(fit),
            detfunc        = fit$model.options$detfunc,
            parindx         = fit$parindx,
            fixed           = fit$fixed,
            design.matrices = fit$design.matrices,
            distances       = calc_distances(traps(fit$capthist), fit$mask),
            usage           = usage(traps(fit$capthist)),
            inv.link        = fit$inv.link,
            S               = n_occasions(fit$capthist),
            K               = n_traps(fit$capthist),
            M               = size(fit$mask),
            a               = area(fit$mask)
        )

    }

    return(esa)
}

# constructs default newdata
# extracts the design matrix (or matrices) for the submodels supplied
# returns newdata as a list of data frames with one element per submodel
default_newdata = function(fit, submodels = NULL){
    # if(!inherits(fit, "gsecr")) stop("gsecr object required")
    if(is.null(submodels)) submodels = names(fit$model)
    newdata = do.call(cbind, sapply(submodels, function(submodel){
        covnames = all.vars(fit$model[[submodel]])
        # if no covariates, use model frame with no columns
        # otherwise make big model frame by collapsing sessions
        # and get the average / reference level for each covariate
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
    newdata = newdata[,unique(colnames(newdata)), drop = FALSE]
    return(newdata)
}


# convert a list of capthists, traps or mask to a multi-session object
MS = function(x, session.names = NULL){

    # checks inputs ---------------------------------------------------------- #
    if(!inherits(x, "list"))
        x = list(x)
    # check all elements have the same class
    classes = c("capthist", "mask", "traps")
    classesOK = sapply(classes, function(i){
        all(sapply(x, function(x) inherits(x, i)))
    })
    if(!any(classesOK))
        stop("requires 'capthist', 'traps' or 'mask' objects")

    # convert to multi-session ----------------------------------------------- #
    class = classes[classesOK]
    if(!(inherits(x, "list") && inherits(x, class)))
        class(x) = c("list", class)

    # check session names ---------------------------------------------------- #
    if(is.null(session.names))
        session.names = session(x)
    if(length(session.names) == 0)
        session.names = sapply(x, function(x){
            y = attr(x, "session")
            if(is.null(y)) NA else y
        })
    if(length(unique(session.names)) != length(session.names) ||
       anyNA(session.names))
        session.names = names(x)
    if(is.null(session.names)){
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


# collapses session traps into single traps object
make_regiontraps = function(x, ms = TRUE){
    if(inherits(x, "gcapthist")) x = traps(x)
    if(!inherits(x, "traps")) stop("expecting a 'traps' or 'gcapthist' object")
    regiontraps = do.call(rbind, lapply(x, as.data.frame))
    class(regiontraps) = c("traps", "data.frame")
    covariates(regiontraps) = do.call(rbind, lapply(x, covariates))
    attr(regiontraps, "usage") = NULL
    attr(regiontraps, "spacex")  = NA
    attr(regiontraps, "spacey")  = NA
    attr(regiontraps, "spacing") = NA
    if(ms) regiontraps = MS(regiontraps)
    return(regiontraps)
}
