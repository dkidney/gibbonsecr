## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

add_category_legend = function(legend, col, ...){

    args = c(as.list(environment()), list(...))
    if(is.null(args$x))     args$x = 'right'
    if(is.null(args$yjust)) args$yjust = 0.5
    if(is.null(args$pch))   args$pch = 15
    if(is.null(args$inset)) args$inset = -0.1
    if(is.null(args$pt.cex))args$pt.cex = 1.5
    xpd = par("xpd")
    on.exit(par(xpd = xpd))
    par(xpd = TRUE)
    do.call("legend", args)
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# # Example
# x = rnorm(1000)
# y = rnorm(1000)
# z = dnorm(x) * dnorm(y) / (dnorm(0)^2)
# zlim = c(0,1)
# ncol = 100
# col = rainbow(ncol, start = 0, end = .7)
# i = cut(z, breaks = seq(min(zlim), max(zlim), length = ncol + 1))
# cols = col[i]
# par(mar = c(4,2,1,5))
# plot(x, y, pch = 19, cex = 0.5, col = cols)
# add_number_legend(zlim, col, yscale = 0.5)

add_number_legend = function(zlim, col, border = 1, xscale = 1.5, yscale = 0.75, xadj = 1){

    xpd = par("xpd")
    on.exit(par(xpd = xpd))
    par(xpd = TRUE)

    ##################################################
    ## legend bounding box

    # coordinates of inner window
    usr = par("usr") ; usr
    # coordinates of outer window as percentages
    plt = par("plt")
    # outer window width
    winw = diff(usr[1:2]) / diff(plt[1:2])
    # bbox width
    boxw = xscale * winw / 50
    # bbox height
    boxh = yscale * diff(usr[3:4])
    # bbox coords
    x0 = usr[2] + winw / 100 * xadj
    x1 = x0 + boxw
    # y0 = usr[3] + boxh * (1 - yscale) / 2
    y0 = usr[3] + (usr[4] - usr[3] - boxh) / 2
    y1 = y0 + boxh
    # make bbox
    bbox = data.frame(x = c(x0, x0, x1, x1),
                      y = c(y0, y1, y1, y0))

    ##################################################
    ## add color cells

    ncol = length(col)
    xs = range(bbox$x)
    ys = seq(min(bbox$y), max(bbox$y), length = ncol + 1)
    # cols = palette(ncols)
    for(i in 1:ncol){ # i=1
        cell = cbind(x = xs[c(1, 1,   2,   2)],
                     y = ys[c(i, i+1, i+1, i)]) ; cell
        polygon(cell, col = col[i], border = col[i])
    }
    polygon(bbox, border = border)

    ##################################################
    ## legend axis

    ticks = pretty(zlim)
    # make sure tick marks don't go outside zlim
    ticks = ticks[ticks >= min(zlim) & ticks <= max(zlim)]
    # convert ticks to tickmark y-axis coords
    ty = bbox$y[1] + diff(range(bbox$y)) * (ticks - min(zlim)) / diff(zlim)
    # make tickmark x-axis coords
    tx = bbox$x[4] + c(0, 1) * diff(range(bbox$x)) / 2
    # plot tickmarks
    segments(x0 = tx[1], x1 = tx[2], y0 = ty, y1 = ty)
    # plot tickmar labels
    text(tx[2], ty, as.character(ticks), pos = 4)

    invisible()

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# number_palette = fields::tim.colors
category_palette = function(ncols){
    h = c(0, 360) + 15
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360 / ncols
    hcl(h = (seq(h[1], h[2], length = ncols)), c = 100, l = 65)
}
# number_palette = function(ncols) rev(rainbow(ncols, start = 0, end = .7))
# number_palette = colorRampPalette(c("blue","red"), space = "Lab")
number_palette = colorRampPalette(c("blue","green","red"), space = "Lab")

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# bearing must be in radians
draw_bearing_arrows = function(bearing = 0, distance = NULL, origin = c(0,0), length = 0.1, ...){
    if(!inherits(origin, c("matrix","data.fame")))
        origin = rbind(origin)
    if(ncol(origin) != 2) stop()
    if(is.null(distance)) distance = 1000
    ne = bearing >= (0.0 * pi) & bearing < (0.5 * pi) ; ne
    se = bearing >= (0.5 * pi) & bearing < (1.0 * pi) ; se
    sw = bearing >= (1.0 * pi) & bearing < (1.5 * pi) ; sw
    nw = bearing >= (1.5 * pi) & bearing <=(2.0 * pi) ; nw
    theta     = bearing              ; theta
    theta[ne] = bearing[ne]          ; theta
    theta[se] = pi - bearing[se]     ; theta
    theta[sw] = bearing[sw] - pi     ; theta
    theta[nw] = 2 * pi - bearing[nw] ; theta
    east  = ifelse(ne | se, 1, -1) ; east
    north = ifelse(nw | ne, 1, -1) ; north
    dx = sin(theta) * distance * east  ; dx
    dy = cos(theta) * distance * north ; dy
    arrows(
        x0 = origin[,1],
        x1 = origin[,1] + dx,
        y0 = origin[,2],
        y1 = origin[,2] + dy,
        length = length,
        ...)
    invisible()
}

# plot(-10:10, -10:10, type = "n", asp = 1)
# n = 17
# bearing = seq(0, 360, length = n) * pi/180 ; bearing
# distance = seq(1, 10, length = n)
# bearing_arrows(bearing, distance, col = 1:n, length = 0.1)

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

plot_bearings = function(capthist, session = NULL, group = NULL, length = 0.05, legend = TRUE, buffer = 6000, add = FALSE){
    if(!inherits(capthist, "capthist"))
        stop("expecting a capthist object")
    capthist = MS(capthist)
    if(is.null(get_bearings(capthist)))
        stop("no bearings to plot")
    sessions = if(is.null(session)) session(capthist) else session
    for(session in sessions){ # session = sessions[1] ; session
        sescapt = capthist[[session]]
        n = n_groups(sescapt)    ; n
        if(n == 0) next
        S = n_occasions(sescapt) ; S
        K = n_traps(sescapt)     ; K
        sestraps = traps(sescapt)

        ##############################################
        ## bearings and distances

        bearings = get_bearings(sescapt)
        # print(bearings) ; stop()
        if(!is.null(bearings)){
            if(attr(bearings, "details")$units == "degrees")
                bearings = bearings * pi/180
        }else next
        distances = get_distances(sescapt)
        if(!is.null(distances)){
            if(attr(distances, "details")$units == "km")
                distances = distances * 1000
            if(is.null(buffer)) buffer = max(distances)
        }

        ##############################################
        ## set up plot

        if(!add){
            buffer.contour(sestraps, buffer, asp = 1, bty = "n", axes = FALSE, col = grey(0.99))
            box()
            plot_traps(sestraps, add = TRUE)
        }

        ##############################################
        ## extract group

        if(!is.null(group)){
            row = which(rownames(sescapt) == group)
            if(length(row) == 0)
                stop("cant find group ID in capthist")
            sescapt = sescapt[row, , , drop = FALSE]
            if(is.null(bearings))
                bearings = bearings[row, , , drop = FALSE]
            if(is.null(distances))
                distances = distances[row, , , drop = FALSE]
        }

        ##############################################
        ## bearing arrows

        index = if(is.null(distances)){
            which(!is.na(bearings), arr.ind = TRUE)
        }else{
            which(!is.na(bearings) & !is.na(distances), arr.ind = TRUE)
        }
        i = sort(unique(index[,1])) ; i
        if(!is.null(attr(sescapt, "locations")))
            points(attr(sescapt, "locations"), col = i, cex = 0.5, pch = 19)
        if(legend) legend("topright", rownames(sescapt)[i], col = i, lty = 1,
                          title = "group", bty = "n")
        for(det in 1:nrow(index)){ # det = 1
            i = index[det,1] ; i
            s = index[det,2] ; s
            k = index[det,3] ; k
            draw_bearing_arrows(
                bearing  = bearings[i,s,k],
                distance = if(is.null(distances)) buffer / 2 else distances[i,s,k],
                col      = i,
                origin   = as.data.frame(sestraps[k,]),
                length   = length
            )
        }
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

plot_capthist = function(x, mask = NULL){
    if(!inherits(x, c("gibbonsecr_fit", "secr", "capthist")))
        stop("x must be a gibbonsecr_fit, secr or capthist object")
    if(!is.null(mask))
        if(!inherits(mask, "mask"))
            stop("requires a 'mask' object")
    if(inherits(x, c("gibbonsecr_fit", "secr"))){
        x = x$capthist
        if(is.null(mask)) mask = x$mask
    }
    x = MS(x)
    if(!is.null(mask)) mask = MS(mask)
    lim = function(x) apply(do.call(rbind, lapply(x, as.matrix)), 2, range)
    lim = if(is.null(mask)) lim(traps(x)) else lim(mask)
    plot(0, 0, xlim = lim[,1], ylim = lim[,2], asp = 1, xlab = "x", ylab = "y")
    for(i in 1:length(x)){ # i=1
        traps = traps(x[[i]])
        points(traps, col = i, pch = 15)
        if(!is.null(mask)){
            # points(as.data.frame(mask[[i]]), col = i, pch = 15)
            buffer = mask_buffer(mask[[i]])
            if(!is.na(buffer)){
                buffer.contour(traps, mask_buffer(mask[[i]]), col = i,
                               lty = ceiling(i / 10), add = TRUE)
            }
        }
    }
    title("Listening post locations")
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# plot_detectfn_auxiliary_old = function(fit, which = c("detectfn","bearings","distances"), session = 1, CI = TRUE, alpha = 0.05, nx = 100, use.global.par.settings = TRUE, ...){
#     which = match.arg(which)
#     plotargs = list(...) # plotargs = list() ; which = "distances" ; session = "2" ; nx = 100
#     # print(plotargs)
#     # stop("1")
#     plotargs$x = switch(
#         which,
#         "detectfn"  = seq(0, mask_buffer(fit$mask, fit$capthist), length = nx),
#         "bearings"  = seq(-pi/2, pi/2, length = nx),
#         "distances" = seq(0, 2000, length = nx)
#     )
#
#     ##################################################
#     ## delta method
#
#     deltaargs = list(beta    = coef(fit),
#                      fit     = fit,
#                      x       = plotargs$x,
#                      session = session,
#                      which   = which,
#                      ...)
#     # print(deltaargs)
#     # stop("2")
#     if(CI){
#         deltaargs$f = fitted_detectfn_auxiliary_values
#         deltaargs$vcov = vcov(fit)
#         delta = do.call(delta_method, deltaargs)
#         zvalue = qnorm(1 - alpha / 2)
#         delta$lower = delta$est - zvalue * delta$se
#         delta$upper = delta$est + zvalue * delta$se
#         delta$se = NULL
#     }else{
#         delta = list(est = do.call(fitted_detectfn_auxiliary_values, deltaargs))
#     }
#     # print(delta)
#     # stop("3")
#     plotargs$y = delta$est
#     if(which == "bearings") plotargs$x = plotargs$x * 180/pi
#
#     ##################################################
#     ## plot
#
#     # set up plotting area if add = NULL or FALSE
#     if(is.null(plotargs$add) || !plotargs$add){
#         plotargs$type = "n"
#         if(is.null(plotargs$main)) plotargs$main = switch(
#             which,
#             "detectfn"  = "Detection function",
#             "bearings"  = "Bearing error distribution",
#             "distances" = "Distance error distribution (truth = 500m)"
#         )
#         if(is.null(plotargs$xlab)) plotargs$xlab = switch(
#             which,
#             "detectfn"  = "Distance from detector (m)",
#             "bearings"  = "Bearing error (degrees)",
#             "distances" = "Distance estimate (m)"
#         )
#         if(is.null(plotargs$ylab)) plotargs$ylab = switch(
#             which,
#             "detectfn"  = "Detection probability",
#             "Probability density"
#         )
#         # if(is.null(plotargs$xaxs)) plotargs$xaxs = "i"
#         # if(is.null(plotargs$yaxs)) plotargs$yaxs = "i"
#         if(is.null(plotargs$bty))  plotargs$bty  = "n"
#         if(is.null(plotargs$ylim)) plotargs$ylim = switch(
#             which,
#             "detectfn"  = c(0,1),
#             c(0, max(sapply(delta,max)))
#         )
#         do.call(plot, plotargs)
#     }
#     # draw plot
#     plotargs[c("add","type","main","xlab","ylab","xaxs","yaxs","bty","xlim","ylim")] = NULL
#     plotargs$type = NULL
#     plotargs$lty = 1
#     do.call(lines, plotargs)
#     if(CI){
#         plotargs$lty = 2
#         for(interval in c("lower","upper")){
#             plotargs$y = delta[[interval]]
#             do.call(lines, plotargs)
#         }
#     }
#     invisible()
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

plot_detectfn_auxiliary = function(fit, which = c("detectfn","bearings","distances"), newdata = NULL, CI = TRUE, alpha = 0.05, nx = 100, use.global.par.settings = TRUE, true.distance = 500, plot = TRUE, ...){
    which = match.arg(which)
    plotargs = list(...) # plotargs = list() ; which = "distances" ; session = "2" ; nx = 100
    # print(plotargs)
    # stop("1")
    plotargs$x = switch(
        which,
        "detectfn"  = seq(0, max(mask_buffer(fit$mask)), length = nx),
        "bearings"  = seq(-pi/2, pi/2, length = nx),
        "distances" = seq(0, 2000, length = nx)
    )

    ##################################################
    ## delta method

    deltaargs = list(beta    = coef(fit),
                     fit     = fit,
                     x       = plotargs$x,
                     newdata = newdata,
                     which   = which,
                     true.distance = true.distance)
    if(CI){
        deltaargs$f = fitted_detectfn_auxiliary_values
        deltaargs$vcov = vcov(fit)
        delta = do.call(delta_method, deltaargs)
        zvalue = qnorm(1 - alpha / 2)
        delta$lower = delta$est - zvalue * delta$se
        delta$upper = delta$est + zvalue * delta$se
        delta$se = NULL
    }else{
        delta = list(est = do.call(fitted_detectfn_auxiliary_values, deltaargs))
    }

    ##################################################
    ## plot

    if(plot){
        plotargs$y = delta$est
        if(which == "bearings") plotargs$x = plotargs$x * 180/pi

        # set up plotting area if add = NULL or FALSE
        if(is.null(plotargs$add) || !plotargs$add){
            plotargs$type = "n"
            if(is.null(plotargs$main)) plotargs$main = switch(
                which,
                "detectfn"  = "Detection function",
                "bearings"  = "Bearing error distribution",
                "distances" = "Distance error distribution (truth = 500m)"
            )
            if(is.null(plotargs$xlab)) plotargs$xlab = switch(
                which,
                "detectfn"  = "Distance from detector (m)",
                "bearings"  = "Bearing error (degrees)",
                "distances" = "Distance estimate (m)"
            )
            if(is.null(plotargs$ylab)) plotargs$ylab = switch(
                which,
                "detectfn"  = "Detection probability",
                "Probability density"
            )
            if(is.null(plotargs$xaxs)) plotargs$xaxs = "i"
            if(is.null(plotargs$yaxs)) plotargs$yaxs = "i"
            if(is.null(plotargs$bty))  plotargs$bty  = "n"
            if(is.null(plotargs$ylim)) plotargs$ylim = switch(
                which,
                "detectfn"  = c(0,1),
                c(0, max(sapply(delta,max)))
            )
            do.call("plot", plotargs)
        }
        # draw plot
        plotargs[c("add","type","main","xlab","ylab","xaxs","yaxs","bty","xlim","ylim")] = NULL
        plotargs$type = NULL
        plotargs$lty = 1
        do.call("lines", plotargs)
        if(CI){
            plotargs$lty = 2
            for(interval in c("lower","upper")){
                plotargs$y = delta[[interval]]
                do.call(lines, plotargs)
            }
        }
    }

    ##################################################
    ## return fitted values

    invisible(delta)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Plot a fitted model
#' @description TODO
#' @inheritParams print.gibbonsecr_fit
#' @param which TODO
#' @details TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link{gibbonsecr_fit}
#' @method plot gibbonsecr_fit
#' @export

# plot.gibbonsecr_fit = function(x, which = c("detectfn","pdot","bearings","distances","density"), session = 1, newdata = NULL, use.global.par.settings = TRUE, mask = NULL, traps = NULL, CI = TRUE, alpha = 0.05, plot = TRUE, ...){
plot.gibbonsecr_fit = function(x, which = c("detectfn","pdot","bearings","distances","density"), ...){
    which = match.arg(which)
    # print(list(...))
    # stop()
    # which = "pdot"; session = 1; use.global.par.settings = TRUE
    # if(!ms(x$capthist)) stop("only works for multi-session data")
    if(which %in% c("bearings","distances"))
        if(x$model.options[[which]] == 0) stop("no ", which, " model")
    # if(!use.global.par.settings) pop = par(no.readonly = TRUE)
    if(which %in% c("detectfn","bearings","distances")){
        # if(!use.global.par.settings)
        # pop = par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(4,4,2,2))
        # invisible(plot_detectfn_auxiliary(x, which = which, newdata = newdata,
                                          # CI = CI, alpha = alpha, plot = plot, ...))
        invisible(plot_detectfn_auxiliary(x, which = which, ...))
    }else{
        # if(!use.global.par.settings)
        # par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(4,4,2,6))
        # invisible(plot_surface(x, which = which, session = session, mask = mask,
                               # traps = traps, CI = CI, alpha = alpha, plot = plot, ...))
        invisible(plot_surface(x, which = which, ...))
    }
    # usr = par()$usr
    # mfg = par()$mfg
    # if(!use.global.par.settings) par(pop)
    # par(usr = usr, mfg = mfg)
    # invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title Plot simulation results
# @description TODO
# @inheritParams print.gibbonsecr_fit
# @param CI TODO
# @param exp TODO
# @param use.global.par.settings TODO
# @details TODO
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @seealso \link{gibbonsecr_sim}
# @method plot gibbonsecr_sim
# @export

plot.gibbonsecr_sim = function(x, CI = TRUE, exp = FALSE, use.global.par.settings = TRUE, ...){

    ##################################################
    ## par settings

    # if(!use.global.par.settings){
    # pop = par(no.readonly = TRUE)
    # on.exit(par(pop))
    # npars = ncol(x$gibbonsecr_fit)
    # par(mfrow = n2mfrow(npars), mar = c(2,2,2,2), oma = c(0,0,2,0))
    # }

    ##################################################
    ## modify secr.fit par names and D values

    if(!is.null(x$secr.fit)){
        for(par in c("D", "g0", "sigma", "z")){ # par = "D"
            i = match(par, colnames(x$secr.fit))
            if(length(i) > 0)
                colnames(x$secr.fit)[i] = paste0(colnames(x$secr.fit)[i], ".(Intercept)")
            if(par == "D"){
                x$secr.fit[,i] = x$secr.fit[,i] + log(100)
            }
        }
    }

    ##################################################
    ## draw histograms

    for(par in colnames(x$gibbonsecr_fit)){ # par = colnames(x$gibbonsecr_fit)[1] ; par

        ##################################################
        ## gibbonsecr_fit results

        col = colnames(x$gibbonsecr_fit) == par
        truth  = attr(x, "truth")[col]
        est1   = x$gibbonsecr_fit[,col]
        mu1    = mean(est1)
        se1    = sd(est1) / sqrt(length(est1))
        upper1 = mu1 + 1.96 * se1
        lower1 = mu1 - 1.96 * se1
        if(exp){
            truth  = exp(truth)
            est1   = exp(est1)
            mu1    = exp(mu1)
            se1    = exp(se1)
            upper1 = exp(upper1)
            lower1 = exp(lower1)
        }
        xrange = range(c(est1, truth))

        ##################################################
        ## secr.fit results

        if(!is.null(x$secr.fit)){
            col = colnames(x$secr.fit) == par
            if(any(col)){
                est0   = x$secr.fit[,col]
                mu0    = mean(est0)
                se0    = sd(est0) / sqrt(length(est0))
                upper0 = mu0 + 1.96 * se0
                lower0 = mu0 - 1.96 * se0
                if(exp){
                    est0   = exp(est0)
                    mu0    = exp(mu0)
                    se0    = exp(se0)
                    upper0 = exp(upper0)
                    lower0 = exp(lower0)
                }
                xrange = range(c(est0, xrange))
            }
        }

        ##################################################
        ## density plot

        plot(density(est1), main = par, xlab = "", ylab = "", xlim = xrange, col = 4)
        abline(v = truth, lty = 2)
        abline(v = mu1, col = 4)
        if(CI) abline(v = c(upper1, lower1), lty = 3, col = 4)

        if(!is.null(x$secr.fit) && any(col)){
            lines(density(est0), col = 2)
            abline(v = mu0, col = 2)
            if(CI) abline(v = c(upper0, lower0), lty = 3, col = 2)
        }
    }

    ##################################################
    ## add title

    if(!use.global.par.settings){
        title(attr(x, "simname"), outer = TRUE)
    }

    invisible()

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title TODO
# @description TODO
# @param mask TODO
# @param traps TODO
# @param region TODO
# @param add TODO
# @param covariate TODO
# @param col TODO
# @param pch TODO
# @param bty TODO
# @param axes TODO
# @param xlab TODO
# @param ylab TODO
# @param main TODO
# @param legend TODO
# @param xaxs TODO
# @param yaxs TODO
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export
plot_mask = function(mask, covariate = NULL, session = NULL, add = FALSE, col = NULL, pch = 15, bty = "n", axes = TRUE, xlab = "", ylab = "", main = "", add.legend = TRUE, xaxs = "r", yaxs = "r", type = "p"){
    # session = NULL; add = FALSE; col = NULL; pch = 15; bty = "n"; axes = FALSE; xlab = ""; ylab = ""; main = ""; legend = TRUE; xaxs = "r"; yaxs = "r"; type = "p"
    if(inherits(mask, "gibbonsecr_fit"))
        mask = mask$mask
    if(!inherits(mask, "mask"))
        stop("requires a mask object", call. = FALSE)
    if(ms(mask) && !is.null(session))
        mask = mask[[session]]
    regionmask = make_regionmask(mask)
    if(!add){
        bbox = mask_bbox(regionmask)
        plot(bbox, type = "n", asp = 1, bty = bty, axes = axes, xlab = xlab,
             ylab = ylab, main = main, xaxs = xaxs, yaxs = yaxs)
    }
    if(type != "n"){
        if(!is.null(covariate)){
            # check covariate exists
            if(is.null(covariates(regionmask)) ||
               !covariate %in% colnames(covariates(regionmask))){
                message("cant find covariate: ", covariate)
                covariate = NULL
                main = ""
            }
        }
        if(is.null(covariate)){
            cols = "grey"
        }else{
            if(is.null(col)){
                var = covariates(regionmask)[[covariate]]
                number = inherits(var, c("integer", "numeric"))
                ncol = 25
                palette = if(number) number_palette else category_palette
                cols = var2col(var, palette = palette, ncol = ncol)
            }
        }
        points(regionmask, pch = pch, col = cols, cex = 0.5)
        if(add.legend){
            if(!is.null(covariate)){
                col = attr(cols, "col")
                if(number){
                    add_number_legend(range(var), col)
                }else{
                    bbox = apply(do.call(rbind, mask_bbox(MS(mask))), 2, range)
                    x = max(bbox[,"x"]) + 0.05 * diff(range(bbox[,"x"]))
                    y = mean(bbox[,"y"])
                    add_category_legend(x = x, y = y, legend = levels(var), col = col)
                }
            }
        }
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title TODO
# @description TODO
# @param shp TODO
# @param traps TODO
# @param add TODO
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export
plot_shp = function(shp, covariate = NULL, palette = NULL, ncol = NULL, zlim = NULL, add.legend = TRUE, axes = TRUE, contour = TRUE){
    # check class
    if(!inherits(shp, c("SpatialPolygons", "SpatialPoints")))
        stop("method only works for SpatialPolygons or SpatialPoints")

    # check covariate exists
    if(!is.null(covariate)){
        if(!"data" %in% slotNames(shp) || !covariate %in% colnames(shp@data)){
            warning("covariate not found")
            covariate = NULL
        }else{
            var = shp@data[[covariate]]
        }
    }

    # make cols
    if(is.null(covariate)){
        cols = "grey"
    }else{
        number = inherits(var, c("integer","numeric"))
        cols = if(number){
            if(is.null(zlim)) zlim = range(var)
            var2col(var, palette = palette, ncol = ncol, zlim = zlim)
        }else{
            var = as.factor(var)
            var2col(var, palette = palette)
        }
    }

    # draw plot
    if(inherits(shp, "SpatialPolygons")){
        sp::plot(shp, col = cols, usePolypath = FALSE, border = NA)
    }else{
        sp::plot(shp, col = cols, pch = 15, cex = 0.5)
    }

    # add legend
    if(add.legend){
        if(!is.null(covariate)){
            col = attr(cols, "col")
            if(number){
                add_number_legend(zlim, col)
            }else{
                #                 strip.legend('right', levels(var), col = col, "other", inset = 0.05,
                #                              height = 0.04 * length(levels(var)),
                #                              width = 0.075, title = covariate)
                x = shp@bbox[1,"max"] + 0.05 * diff(range(shp@bbox[1,]))
                y = mean(shp@bbox[2,])
                add_category_legend(x = x, y = y, legend = levels(var), col = col, bty = "n")
            }
        }
    }
    # add axes
    if(axes){
        axis(1)
        axis(2)
    }
    if(contour){
        if(inherits(shp, "SpatialPoints")){
            contour(sp::SpatialPixelsDataFrame(shp@coords, shp@data[, covariate, drop = FALSE]), add = TRUE)
        }
    }
    invisible()
}


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# mask allows the use of a prediction mask
# centered determines whether or not the axes on the plot should be centered
# ... is for additional arguments to pass to image

plot_surface = function(fit, which = c("pdot","density"), CI = FALSE, contour = TRUE, legend = TRUE, centered = FALSE, session = 1, mask = NULL, traps = NULL, plot = TRUE, alpha = 0.05, ...){
    which = match.arg(which)
    # which = "pdot"; session = "2"; CI = FALSE; contour = TRUE; centered = TRUE

    ##################################################
    ## mask and traps

    if(which == "density"){
        # use regiontraps and regular regionmask
        if(is.null(traps))
            traps = make_regiontraps(traps(fit$capthist))
        if(is.null(mask))
            mask = make_regular_regionmask(fit$mask, fit$capthist)
    }
    if(which == "pdot"){
        # use sessiontraps and sessionmask
        traps = traps(fit$capthist)[[session]]
        mask  = fit$mask[[session]]
    }
    if(is.numeric(session))
        session = session(fit$capthist)[session]
    traps = MS(traps)
    mask  = MS(mask)
    session(traps) = session(mask) = session

    ##################################################
    ## delta method

    deltaargs = list(beta    = coef(fit),
                     fit     = fit,
                     session = session,
                     mask    = mask,
                     traps   = traps,
                     which   = which)
    if(CI){
        deltaargs$f    = fitted_surface_values
        deltaargs$vcov = vcov(fit)
        delta = do.call(delta_method, deltaargs)
        z = qnorm(1 - alpha / 2)
        delta$lower = delta$est - z * delta$se
        delta$upper = delta$est + z * delta$se
        delta$se = NULL
    }else{
        # delta = list(est = do.call(fitted_pdot_values, deltaargs))
        delta = list(estimate = do.call(fitted_surface_values, deltaargs))
    }

    ##################################################
    ## plot

    if(plot){
        plot.range = if(CI) c("estimate", "upper", "lower") else "estimate"
        for(i in plot.range){ # i = "estimate"
            plotargs = list(...) # plotargs = list()
            # plotargs = c(mask_image(mask[[session]], delta$est, plot = FALSE), plotargs)
            plotargs = c(mask_image(mask[[session]], delta[[i]], plot = FALSE), plotargs)
            # legend.mar = plotargs$legend.mar
            # plotargs$legend.mar = NULL
            plotargs = plotargs[!sapply(plotargs, is.null)]
            if(is.null(plotargs$add) || !plotargs$add){
                if(is.null(plotargs$main)) plotargs$main = switch(
                    which,
                    "pdot"    = paste0("Detection surface (", i, ")"),
                    "density" = paste0("Density surface (", i, ")")
                )
                if(is.null(plotargs$xlab)) plotargs$xlab = ""
                if(is.null(plotargs$ylab)) plotargs$ylab = ""
                if(is.null(plotargs$axes)) plotargs$axes = TRUE
                if(is.null(plotargs$xaxs)) plotargs$xaxs = "r"
                if(is.null(plotargs$yaxs)) plotargs$yaxs = "r"
                if(is.null(plotargs$asp))  plotargs$asp  = 1
                if(is.null(plotargs$bty))  plotargs$bty  = "n"
            }
            if(is.null(plotargs$nlevel)) plotargs$nlevel = 25
            if(is.null(plotargs$col))
                plotargs$col = number_palette(plotargs$nlevel)
            if(is.null(plotargs$ylim))   plotargs$zlim   = switch(
                which,
                "pdot"    = c(0,1),
                "density" = range(delta)
            )
            # nlevel = plotargs$nlevel
            plotargs$nlevel = NULL
            axes = plotargs$axes
            if(centered && plotargs$axes) plotargs$axes = FALSE
            # image
            do.call(image, plotargs)
            if(centered && axes){
                # redo this bit so you use trap mean instead of don't use buffer
                buffer = mask_buffer(mask[[session]])
                if(!is.na(buffer)){
                    tick.length = diff(axTicks(1)[1:2])
                    for(i in 1:2){ # i=1
                        cent = mean(traps[[1]][,i])
                        at = sort(unique(c(
                            seq(cent, cent - buffer, by = -tick.length),
                            seq(cent, cent + buffer, by =  tick.length)
                        )))
                        axis(i, at, as.character(at - cent))
                    }
                }
            }
            # save usr and mfg (image.plot changes them)
            # usr = par()$usr
            # mfg = par()$mfg
            # traps
            plot_traps(traps, add = TRUE, cex = 1)
            # contour and legend
            if(abs(diff(range(plotargs$zlim))) > 0){
                # contour
                if(contour) contour(plotargs$x, plotargs$y, plotargs$z, add = TRUE)
                # legend
                if(legend) add_number_legend(zlim = plotargs$zlim, col = plotargs$col)
                #         plotargs$nlevel = nlevel
                #         plotargs$graphics.reset = TRUE
                #         plotargs$legend.only = TRUE
                #         plotargs$legend.mar = legend.mar
                #         plotargs$breaks = seq(plotargs$zlim[1], plotargs$zlim[2], length = nlevel + 1)
                #         do.call(fields::image.plot, plotargs)
            }
        }
    }
    # par(usr = usr, mfg = mfg)

    ##################################################
    ## return fitted values

    invisible(delta)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title TODO
# @description TODO
# @param x TODO
# @param add TODO
# @param pch TODO
# @param cex TODO
# @param col TODO
# @param ... TODO
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @export
plot_traps = function(x, session = NULL, buffer = 1000, ...){
    # x = traps(capthist); session = NULL
    if(inherits(x, "gibbonsecr_fit"))
        x = traps(x$capthist)
    if(!inherits(x, c("capthist", "traps")))
        stop("requires a capthist or traps object", call. = FALSE)
    if(inherits(x, "capthist"))
        x = traps(x)
    if(!ms(x)){
        x = list(x)
        sessions = 1
    }else{
        sessions = if(is.null(session)){
            session(x)
        }else{
            if(is.numeric(session)){
                session(x)[session]
            }else{
                session
            }
        }
        x = x[sessions]
    }
    args = list(...) # args = list(add = TRUE)
    if(is.null(args$add))  args$add  = FALSE
    if(is.null(args$pch))  args$pch  = 15
    if(is.null(args$cex))  args$cex  = 0.5
    if(is.null(args$asp))  args$asp  = 1
    if(is.null(args$type)) args$type = "p"
    type = args$type
    add  = args$add
    args$add = NULL
    if(!add){
        bbox = apply(do.call(rbind, lapply(x, as.data.frame)), 2, range)
        args$x = bbox
        args$type = "n"
        do.call(plot, args)
        # plot(bbox, ...)
    }
    if(type != "n"){
        args$type = NULL
        args$asp = NULL
        for(i in sessions){ # i = sessions[1] ; i
            args$x = as.data.frame(x[[i]])
            do.call(points, args)
            # points(as.data.frame(x[[i]]), pch = pch, cex = cex, col = col)
        }
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

var2col = function(x, col = NULL, palette = NULL, ncol = NULL, zlim = NULL){
    if(inherits(x, c("integer","numeric"))){
        if(is.null(zlim)) zlim = range(x)
        if(is.null(col)){
            if(is.null(ncol)) ncol = 25
            if(is.null(palette)) palette = number_palette
            col = palette(ncol)
        }
        i = cut(x, include.lowest = TRUE,
                breaks = seq(min(zlim), max(zlim), length = length(col) + 1))
        cols = col[i]
    }else{
        x = as.factor(x)
        if(is.null(col)){
            ncol = length(levels(x))
            if(is.null(palette)) palette = category_palette
            col = palette(ncol)
        }
        cols = col[x]
    }
    attr(cols, "col") = col
    return(cols)
}


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
