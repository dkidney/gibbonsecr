
# capthist methods =============================================================

#' @rdname plot_capthist
#' @name plot.gcapthist
#' @title Plot a gibbonsecr capthist
#' @description Plotting methods for gibbonsecr capthist objects.
#' @param x a \link[gibbonsecr]{gcapthist} object
#' @param which type of plot (see Details)
#' @param sessions the sessions to plot (default is all sessions)
#' @param ... additional arguments to pass to \link[graphics]{plot.default},
#'   \link[graphics]{points.default} or \link[ggplot2]{geom_point}
#' @details The following options are available for the \code{which} argument:
#'   \tabular{ll}{
#'    \code{traps}      \tab Listening post locations \cr
#'    \code{arrays}     \tab Centre points of listening post arrays \cr
#'    \code{detections} \tab Arrows showing the estimated bearings, if present
#'    (with arrow length equal to to the estimated distance, if present) \cr
#'   }
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example-plot_capthist.r
#' @seealso \link[gibbonsecr]{plot.gmask}
#' @method plot gcapthist
#' @export
plot.gcapthist = function(x, which = c("traps", "arrays", "detections"), sessions = NULL, ...){
    which = match.arg(which)
    plot_capthist(x = x, which = which, sessions = sessions, add = FALSE, ...)
}

#' @rdname plot_capthist
#' @name points.gcapthist
#' @method points gcapthist
#' @export
points.gcapthist = function(x, which = c("traps", "arrays", "detections"), sessions = NULL, ...){
    which = match.arg(which)
    plot_capthist(x = x, which = which, sessions = sessions, add = TRUE, ...)
}

#' @rdname plot_capthist
#' @name geom_capthist
#' @export
geom_capthist = function(x, which = c("traps", "arrays"), sessions = NULL, ...){
    if(!inherits(x, "gcapthist")) stop("expecting a 'gcapthist' object")
    which = match.arg(which)
    plot_capthist(x = x, which = which, sessions = sessions, ggplot = TRUE, ...)
}

# called by plot.gcapthist, points.gcapthist and geom_capthist
plot_capthist = function(x, which = "traps", sessions = NULL, add = FALSE, ggplot = FALSE, ...){
    if(is.null(sessions)) sessions = session(x)
    subcapt = subset(x, sessions)
    dots = list(...)
    if(is.null(dots$pch)) dots$pch = 15
    # traps and arrays ------------------------------------------------------- #
    if(which == "traps"){
        if(is.null(dots$cex)) dots$cex = if(ggplot) 0.75 else 0.5
        data = make_regiontraps(subcapt, ms = FALSE)
    }
    if(which == "arrays"){
        if(is.null(dots$cex)) dots$cex = if(ggplot) 1 else 0.75
        center = function(x) apply(x, 2, mean)
        data = as.data.frame(do.call(rbind, lapply(traps(subcapt), center)))
    }
    if(which %in% c("traps", "arrays")){
        if(ggplot){
            dots$data = data
            dots$mapping = aes_string(x = "x", y = "y", z = NULL, fill = NULL,
                                      shape = NA)
            if(is.null(dots$show.legend)) dots$show.legend = FALSE
            return(do.call(geom_point, dots))
        }else{
            dots$x = data
            if(add){
                FUN = graphics::points.default
            }else{
                FUN = graphics::plot.default
                if(is.null(dots$asp)) dots$asp = 1
            }
            do.call(FUN, dots)
        }
    }
    # detections ------------------------------------------------------------- #
    if(which == "detections"){
        for(i in session(subcapt)) draw_bearings(subcapt[[i]], ...)
    }
}


# - called by plot_capthist
# - input = single session capthist
draw_bearings = function(x, distance = 500, legend = TRUE, groups = NULL, ...){
    if(n_groups(x) == 0){
        message("no detections to plot")
        return(NULL)
    }
    traps = as.data.frame(traps(x))
    if(is.null(groups)) groups = rownames(x)
    if(is.numeric(groups)) groups = rownames(x)[groups]
    # bearings --------------------------------------------------------------- #
    bearings = get_bearings(x)
    if(is.null(bearings)){
        message("no bearings to plot")
        return(NULL)
    }else{
        if(attr(bearings, "details")$units == "degrees")
            bearings = bearings * pi / 180
    }
    # distances -------------------------------------------------------------- #
    distances = get_distances(x)
    if(is.null(distances)){
        distances = bearings
        distances[!is.na(distances)] = distance
    }else{
        if(attr(distances, "details")$units == "km")
            distances = distances * 1000
    }
    # loop over detections --------------------------------------------------- #
    bearings = bearings[groups, , , drop = FALSE]
    distances = distances[groups, , , drop = FALSE]
    INDEX = which(!is.na(bearings), arr.ind = TRUE)
    dots = list(...)
    # colours
    col = INDEX[,1]
    cols = if(is.null(dots$col)) unique(col) else dots$col
    ltys = if(is.null(dots$lty)) 1 + (unique(col) %/% 9) else dots$lty
    # plot points
    arrows_bearings(
        bearing  = bearings[INDEX],
        distance = distances[INDEX],
        trap     = traps[INDEX[,3],],
        col      = cols[col],
        lty      = ltys[col]
    )
    # add legend
    if(legend){
        bty = if(is.null(dots$bty)) "n" else dots$bty
        legend("topright", lty = ltys, col = cols, bty = bty,
               rownames(INDEX)[match(unique(col), INDEX[,1])])
    }
    invisible()
}

# draw bearing arrows
# - called by draw_bearings
#' @importFrom graphics arrows
arrows_bearings = function(bearing = 0, distance = 1000, trap = c(0,0), length = 0.1, ...){
    ne = bearing >= (0.0 * pi) & bearing < (0.5 * pi)
    se = bearing >= (0.5 * pi) & bearing < (1.0 * pi)
    sw = bearing >= (1.0 * pi) & bearing < (1.5 * pi)
    nw = bearing >= (1.5 * pi) & bearing <=(2.0 * pi)
    theta     = bearing
    theta[ne] = bearing[ne]
    theta[se] = pi - bearing[se]
    theta[sw] = bearing[sw] - pi
    theta[nw] = 2 * pi - bearing[nw]
    east  = ifelse(ne | se, 1, -1)
    north = ifelse(nw | ne, 1, -1)
    dx = sin(theta) * distance * east
    dy = cos(theta) * distance * north
    dots = list(...)
    dots$x0 = trap[,1]
    dots$x1 = trap[,1] + dx
    dots$y0 = trap[,2]
    dots$y1 = trap[,2] + dy
    if(is.null(dots$length)) dots$length = 0.1
    do.call(arrows, dots)
    invisible()
}


# mask methods =================================================================

#' @rdname plot_mask
#' @name plot.gmask
#' @title Plot a gibbonsecr mask
#' @description Plotting functions for gibbonsecr mask objects. The plot method
#'   is a wrapper for \link[secr]{plot.mask} and geom_mask is a wrapper for
#'   \link[ggplot2]{geom_point} (or \link[ggplot2]{geom_contour} when
#'   \code{contour = TRUE})
#' @param x a \link[gibbonsecr]{gmask} object
#' @param covariate name of covariate to use for colouring
#' @param sessions sessions to plot (as integer indices, logical indices or
#'   session names)
#' @param contour if \code{TRUE}, contour lines will be plotted
#' @param ... additional arguments to pass to \link[secr]{plot.mask} or
#'   \link[ggplot2]{geom_point}
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link[gibbonsecr]{plot.gcapthist}
#' @example inst/examples/example-plot_mask.r
#' @method plot gmask
#' @export
plot.gmask = function(x, covariate = NULL, sessions = NULL, ...){
    plot_mask(x = x, covariate = covariate, sessions = sessions,
              contour = FALSE, ...)
}

# @rdname plot_mask
# @name image.gmask
#' @method image gmask
#' @importFrom graphics image
# @export
image.gmask = function(x, zvar = NULL, session = 1, ...){
    image(image_mask(x = x, zvar = zvar, session = session), ...)
}

# @rdname plot_mask
# @name contour.gmask
#' @method contour gmask
#' @importFrom graphics contour
# @export
contour.gmask = function(x, zvar = NULL, session = 1, ...){
    contour(image_mask(x = x, zvar = zvar, session = session), ...)
}

image_mask = function(x, zvar, session = 1){
    w = spacing(x)
    if(is.null(session)) session = 1
    mask = x[[session]]
    xlim = range(mask$x)
    ylim = range(mask$y)
    x = seq(min(xlim), max(xlim), w)
    y = seq(min(ylim), max(ylim), w)
    z = matrix(NA, nrow = length(x), ncol = length(y))
    i = cbind(match(mask$x, x), match(mask$y, y))
    z[i] = if(is.null(zvar)) 1 else zvar
    list(x = x, y = y, z = z)
}

#' @rdname plot_mask
#' @name geom_mask
#' @export
geom_mask = function(x, covariate = NULL, sessions = NULL, contour = FALSE, ...){
    if(!inherits(x, "gmask")) stop("expecting a 'gmask' object")
    plot_mask(x = x, covariate = covariate, sessions = sessions, contour = contour,
              ggplot = TRUE, ...)
}

# called by plot.gmask, points.gmask and geom_mask
plot_mask = function(x, covariate = NULL, sessions = NULL, add = FALSE, contour = FALSE, ggplot = FALSE, ...){
    if(is.null(sessions)) sessions = session(x)
    dots = list(...)
    if(is.null(dots$pch)) dots$pch = 15
    if(is.null(dots$cex)) dots$cex = 0.25
    if(is.null(dots$col)) dots$col = "grey"
    if(!is.null(covariate) && !covariate %in% c("x", "y"))
        if(!covariate %in% names(covariates(x[[1]]))){
            warning("covariate not found")
            return(NULL)
        }
    # data = make_regular_regionmask(subset(x, sessions))
    data = make_regionmask(subset(x, sessions), ms = FALSE)
    if(ggplot){
            dots$data = data
        if(is.null(covariate)){
            dots$mapping = aes_string(x = "x", y = "y", z = NULL, fill = NULL)
            FUN = geom_point
        }else{
            dots$mapping = aes_string(x = "x", y = "y",
                                      z = covariate, fill = covariate)
            dots$data = cbind(dots$data, covariates(data))
            if(contour){
                if(is.null(dots$color)) dots$color = "grey25"
                FUN = geom_contour
            }else{
                FUN = geom_tile
                dots$width = dots$height = spacing(x)
            }
            dots[c("pch", "cex", "col")] = NULL
        }
        return(do.call(FUN, dots))
    }else{
        # use plot method from secr package
        dots$x = data
        dots$covariate = covariate
        dots$add = add
        if(!add) if(is.null(dots$asp)) dots$asp = 1
        if(is.null(dots$axes)) dots$axes = TRUE
        do.call(graphics::plot, dots)
    }
}


# fit methods ==================================================================

#' @rdname plot_fit
#' @name plot.gsecr
#' @title Plot a gibbonsecr fitted model
#' @description Plotting methods for gibbonsecr fitted model objects.
#' @param x a \link[gibbonsecr]{gsecr} object
#' @param which plot type
#' @param session session to plot (integer or session name)
#' @param ci if \code{TRUE}, confidence intervals will be plotted
#' @param level confidence level for confidence intervals
#' @param method confidence interval estimation method
#' @param nboot number of bootstrap samples to use when \code{method = "boot"}
#' @param contour if \code{TRUE}, a contour plot will be drawn instead of a
#'   heatmap (for \code{which = "detsurf"} or \code{which = "densurf"})
#' @param newdata optional dataframe of covariate values
#' @param add if \code{TRUE}, adds to existing plot
#' @param ... additional arguments to be passed to the underlying plotting
#'   method (see Details)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example-plot_fit.r
#' @method plot gsecr
#' @export
plot.gsecr = function(x, which = c("detfunc", "detsurf", "bearings", "distances", "densurf", "locations"), session = 1, ci = FALSE, level = 0.95, method = c("delta", "boot"), nboot = 999, newdata = NULL, add = FALSE, ...){
    which = match.arg(which)
    method = match.arg(method)
    plot_fit(x = x, which = which, session = session, ci = ci, level = level,
             method = method, nboot = nboot, newdata = newdata, add = add, ...)
}

#' @rdname plot_fit
#' @name geom_fit
#' @export
geom_fit = function(x, which = c("detfunc", "detsurf", "bearings", "distances", "densurf"), session = 1, ci = FALSE, level = 0.95, method = c("delta", "boot"), nboot = 999, contour = FALSE, newdata = NULL, ...){
    if(!inherits(x, "gsecr")) stop("expecting a 'gsecr' object")
    which = match.arg(which)
    method = match.arg(method)
    plot_fit(x = x, which = which, session = session, ci = ci, level = level,
             method = method, nboot = nboot, contour = contour,
             newdata = newdata, ggplot = TRUE, ...)
}

# called by plot.gsecr, points.gsecr, geom_fit
plot_fit = function(x, which = c("detfunc", "detsurf", "bearings", "distances", "densurf", "locations"), session = 1, ci = TRUE, level = 0.95, method = "delta", nboot = 999, contour = FALSE, newdata = NULL, add = FALSE, ...){
    if(length(session) > 1) stop("session must be a scalar")
    if(is.numeric(session)) session = session(x$capthist)[session]
    if(which %in% c("detfunc", "bearings", "distances")){
        if(which %in% c("bearings", "distances"))
            if(x$model.options[[which]] == 0){
                warning("No ", which, " model to plot")
                return(NULL)
            }
        out = plot_1D(fit = x, which = which, ci = ci, level = level,
                      method = method, nboot = nboot, newdata = newdata,
                      add = add, ...)
    }
    if(which %in% c("detsurf", "densurf")){
        if(ci){
            warning("confidence intervals not yet implemented")
            ci = FALSE
        }
        out = plot_2D(fit = x, which = which, session = session, ci = ci,
                      level = level, method = method, newdata = newdata,
                      add = add, contour = contour, ...)
    }
    if(which == "locations"){
        message("not yet implemented")
        return(NULL)
        # plot_locations(x = x, add = add, ...)
    }
    invisible(out)
}

# called by plot_fit for "detfunc", "bearings" and "distances"
plot_1D = function(fit, which = "detfunc", ci = TRUE, level = 0.95, method = "delta", nboot = 999, newdata = NULL, true.distance = 500, add = FALSE, ggplot = FALSE, ...){
    dots = list(...)
    if(is.null(dots$xlim))
        xlim = switch(which, "bearings" = c(-pi/4, pi/4),
                      c(0, buffer(fit$mask)))
    dots$xlim = NULL
    x = seq(xlim[1], xlim[2], length = 500)
    values = if(ci){
        predict_1D_interval(fit, x = x, which = which, level = level,
                            method = method, nboot = nboot, newdata = newdata,
                            true.distance = true.distance)
    }else{
        list(estimate = predict_1D_point(fit, x = x, which = which,
                                         newdata = newdata,
                                         true.distance = true.distance))
    }
    x = if(which == "bearings") x * 180 / pi else x
    if(ggplot){
        # ggplot ------------------------------------------------------------- #
        dots$data = data.frame(x = x, as.data.frame(values))
        if(ci){
            dots$mapping = aes_string(x = "x", ymin = "lower", ymax = "upper")
            FUN = geom_ribbon
            if(is.null(dots$alpha)) dots$alpha = 0.25
        }else{
            dots$mapping = aes_string(x = "x", y = "estimate")
            FUN = geom_path
        }
        return(do.call(FUN, dots))
    }else{
        # plot.default ------------------------------------------------------- #
        dots$x = x
        dots$y = values$est
        dots$lty = 1
        if(!add){ # set up plotting area
            dots$type = "n"
            if(is.null(dots$main)) dots$main = switch(
                which,
                "detfunc"  = "Detection function",
                "bearings"  = "Bearing error distribution",
                "distances" = "Distance error distribution (truth = 500m)"
            )
            if(is.null(dots$xlab)) dots$xlab = switch(
                which,
                "detfunc"  = "Distance from detector (m)",
                "bearings"  = "Bearing error (degrees)",
                "distances" = "Distance estimate (m)"
            )
            if(is.null(dots$ylab)) dots$ylab = switch(
                which,
                "detfunc"  = "Detection probability",
                "Probability density"
            )
            if(is.null(dots$xaxs)) dots$xaxs = "i"
            if(is.null(dots$yaxs)) dots$yaxs = "i"
            if(is.null(dots$bty))  dots$bty  = "n"
            if(is.null(dots$ylim)) dots$ylim = switch(
                which,
                "detfunc"  = c(0,1),
                c(0, max(sapply(values, max)))
            )
            do.call(graphics::plot.default, dots)
        }
        # point estimate
        dots[c("add","type","main","xlab","ylab","xaxs","yaxs","bty","xlim",
               "ylim")] = NULL
        do.call(graphics::lines.default, dots)
        # interval estimate
        if(ci){
            dots$lty = 2
            for(interval in c("lower","upper")){
                dots$y = values[[interval]]
                do.call(graphics::lines.default, dots)
            }
        }
    }
}

# called by plot_fit for "detsurf" and "densurf"
plot_2D = function(fit, which = "detsurf", session = 1, ci = FALSE, level = 0.95, method = "delta", nboot = 999, newdata = NULL, contour = FALSE, add = FALSE, ggplot = FALSE, ...){
    dots = list(...)
    if(which == "densurf"){
        traps = make_regiontraps(fit$capthist)
        mask  = make_regionmask(fit$mask)
        # mask  = make_regular_regionmask(fit$mask, fit$capthist)
        traps = MS(traps, session)
        mask  = MS(mask, session)
    }
    if(which == "detsurf"){
        traps = traps(subset(fit$capthist, session))
        mask  = subset(fit$mask, session)
    }
    values = if(ci){
        predict_2D_interval(fit, mask = mask, traps = traps, which = which,
                            session = session, newdata = newdata)
    }else{
        list(estimate = predict_2D_point(fit, mask = mask, traps = traps,
                                         which = which, session = session,
                                         newdata = newdata))
    }
    if(ggplot){
        # ggplot ------------------------------------------------------------- #
        zvar = switch(which, "detsurf" = "Probability", "densurf" = "Density")
        dots$mapping = aes_string(x = "x", y = "y", z = zvar, fill = zvar)
        dots$data = as.data.frame(mask[[1]])
        dots$data[[zvar]] = values$est
        if(contour){
            if(is.null(dots$color)) dots$color = "grey25"
            FUN = geom_contour
        }else{
            FUN = geom_tile
            dots$width = dots$height = spacing(mask)
        }
        dots[c("pch", "cex", "col")] = NULL
        return(do.call(FUN, dots))
    }else{
        # image -------------------------------------------------------------- #
        dots$x = mask
        dots$zvar = values$est
        dots$add = add
        if(is.null(dots$asp)) dots$asp = if(add) NULL else 1
        FUN = if(contour) contour.gmask else image.gmask
        do.call(FUN, dots)
    }
}


# shp methods ==================================================================

#' @rdname plot_shp
#' @name plot.gshp
#' @title Plot an imported shapfile
#' @description Plotting methods for imported shapefiles.
#' @param x a \link[gibbonsecr]{gshp} object
#' @param covariate name of covariate to use for colouring
#' @param contour if \code{TRUE}, contour lines will be plotted
#' @param ... additional arguments to be passed to the underlying plotting
#'   method (see Details)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example-plot_shp.r
#' @method plot gshp
#' @export
plot.gshp = function(x, covariate = NULL, contour = FALSE, ...){
    plot_shp(x = x, covariate = covariate, contour = contour, ggplot = FALSE, ...)
}

#' @rdname plot_shp
#' @name geom_shp
#' @export
geom_shp = function(x, covariate = NULL, contour = FALSE, ...){
    if(!inherits(x, "gshp")) stop("expecting a 'gshp' object")
    plot_shp(x = x, covariate = covariate, contour = contour, ggplot = TRUE, ...)
}

plot_shp = function(x, covariate = NULL, contour = FALSE, ggplot = FALSE, ...){

    # checks inputs ---------------------------------------------------------- #
    dots = list(...)
    if("data" %in% slotNames(x$sp)){
        if(!is.null(covariate) && !covariate %in% colnames(x$sp@data)){
            warning("covariate not found")
            covariate = NULL
        }
    }else covariate = NULL
    is.poly = inherits(x$sp, "SpatialPolygons")

    if(ggplot){
        # geom --------------------------------------------------------------- #
        # mapping
        dots$mapping = if(is.poly){
            if(is.null(covariate)){
                aes_string(x = "long", y = "lat", group = "group")
            }else{
                aes_string(x = "long", y = "lat", group = "group", fill = covariate)
            }
        }else{
            if(is.null(covariate)){
                aes_string(x = "coords.x1", y = "coords.x2")
            }else{
                aes_string(x = "coords.x1", y = "coords.x2", fill = covariate)
            }
        }
        # data
        dots$data = if(is.poly){
            if(is.null(covariate)){
                x$fortified
            }else{
                i = factor(x$fortified$id)
                x$fortified[[covariate]] = x$sp@data[[covariate]][i]
                x$fortified
            }
        }else cbind(x$sp@coords, x$sp@data)
        # FUN
        FUN = if(is.poly) geom_polygon else{
            if(is.null(covariate)) geom_point else geom_tile
        }
        # fill
        if(is.null(dots$fill))
            if(is.null(covariate)) dots$fill = NA
        # col
        if(is.null(dots$col))
            if(is.null(covariate)) dots$col = "grey25"
        # lty
        if(is.null(dots$lwd))
            if(is.null(covariate)) if(is.poly) dots$lwd = 0.25
        # cex
        if(is.null(dots$cex))
            if(is.null(covariate)) if(!is.poly) dots$cex = 0.1
        # pch
        if(is.null(dots$pch))
            if(is.null(covariate)) if(!is.poly) dots$pch = 15
        # return geom
        out = do.call(FUN, dots)
        return(out)

    }else{
        # sp::plot ----------------------------------------------------------- #
        if(contour) warning("contours not yet implemented")
        # col
        if(is.null(covariate)){
            if(is.null(dots$col)) dots$col = if(is.poly) NA else "grey"
        }else{
            # number of unique covariate values levels
            nlevels = length(levels(factor(x$sp@data[[covariate]])))
            # number of colours to use
            ncols = if(is.null(dots$col)) nlevels else{
                # throw error if length input cols greater than nlevels
                if(length(dots$col) > nlevels)
                    stop("length(col) is greater than number of unique covariate values")
                length(dots$col)
            }
            # vector of colours
            if(inherits(x$sp@data[[covariate]], c("character", "factor"))){
                # default ggplot cols for factors
                if(is.null(dots$col))
                    dots$col = scales::hue_pal()(nlevels)
            }else{
                # if cols supplied and length(cols) = nlevels then skip
                skip = !is.null(dots$col) && nlevels == length(dots$col)
                if(!skip){
                    cutpoints = cut(x$sp@data[[covariate]], breaks = ncols)
                    # default ggplot cols for numerics
                    pal = scales::seq_gradient_pal(
                        low = "#132B43", high = "#56B1F7", space = "Lab")
                    cols = pal(seq(0, 1, length.out = nlevels))
                    dots$col = if(is.null(dots$col)) cols[cutpoints] else
                        dots$col[cutpoints]
                }
            }
            if(is.poly)
                dots$col = dots$col[factor(x$sp@data[[covariate]])]
        }
        # border
        if(is.poly) if(is.null(dots$border))
            dots$border = if(is.null(covariate)) "grey25" else NA
        # cex
        if(!is.poly) if(is.null(dots$cex)) dots$cex = 0.1
        # pch
        if(!is.poly) if(is.null(dots$pch)) dots$pch = 15
        # main
        if(is.null(dots$main))
            dots$main = covariate
        # call sp::plot
        dots$x = x$sp
        do.call(sp::plot, dots)
    }
}

