## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

plot_detectfn_auxiliary = function(fit, which = c("detectfn","bearings","distances"), session = 1, CI = TRUE, alpha = 0.05, nx = 100, use.global.par.settings = FALSE, ...){
    which = match.arg(which)
    plotargs = list(...) # plotargs = list() ; which = "bearings" ; session = "2" ; nx = 100
    plotargs$x = switch(
        which,
        "detectfn"  = seq(0, mask_buffer(fit$mask, fit$capthist), length = nx),
        "bearings"  = seq(-pi/2, pi/2, length = nx),
        "distances" = seq(0, 2000, length = nx)
    )
    
    ##################################################
    ## delta method
    
    deltaargs = list(beta = coef(fit), fit = fit, x = plotargs$x, session = session, which = which)
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
    plotargs$y = delta$est
    if(which == "bearings") plotargs$x = plotargs$x * 180/pi
    
    ##################################################
    ## plot
    
    # set up plotting area if add = NULL or FALSE
    if(is.null(plotargs$add) || !plotargs$add){
        plotargs$type = "n"
        if(is.null(plotargs$main)) plotargs$main = switch(
            which,
            "detectfn"  = "Detection function",
            "bearings"  = "Bearing error distribution",
            "distances" = "Distance error distribution (truth = 1000m)"
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
        do.call(plot, plotargs)
    }
    # draw plot
    plotargs[c("add","type","main","xlab","ylab","xaxs","yaxs","bty","xlim","ylim")] = NULL
    plotargs$type = NULL
    plotargs$lty = 1
    do.call(lines, plotargs)
    if(CI){
        plotargs$lty = 2
        for(interval in c("lower","upper")){
            plotargs$y = delta[[interval]]
            do.call(lines, plotargs)
        }
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

plot_surface = function(fit, which = c("pdot","density"), CI = FALSE, session, ...){
    which = match.arg(which) # which = "pdot"; session = "2"; CI = FALSE

    ##################################################
    ## mask and traps

    if(which == "density"){
        # use regiontraps and regular regionmask
        traps = traps_rbind(traps(fit$capthist))
        mask  = make_regular_regionmask(fit$mask, fit$capthist)
    }
    if(which == "pdot"){
        # use sessiontraps and sessionmask
        traps = traps(fit$capthist)[[session]]
        mask  = fit$mask[[session]]
    }
    traps = MS(traps)
    mask  = MS(mask)
    session(traps) = session(mask) = session
    
    ##################################################
    ## delta method
    
    deltaargs = list(beta = coef(fit), fit = fit, session = session, mask = mask, traps = traps, which = which)
    if(CI){
        deltaargs$f = fitted_surface_values
        deltaargs$vcov = vcov(fit)
        delta = do.call(delta_method, deltaargs)
    }else{
        # delta = list(est = do.call(fitted_pdot_values, deltaargs))
        delta = list(est = do.call(fitted_surface_values, deltaargs))
    }
    
    ##################################################
    ## plot
    
    plotargs = mask_image(mask[[session]], delta$est, plot = FALSE)
    if(is.null(plotargs$add) || !plotargs$add){
        if(is.null(plotargs$main)) plotargs$main = switch(
            which,
            "pdot"    = "Detection surface",
            "density" = "Density surface"
        )
        if(is.null(plotargs$xlab)) plotargs$xlab = ""
        if(is.null(plotargs$ylab)) plotargs$ylab = ""
        if(is.null(plotargs$xaxs)) plotargs$xaxs = "i"
        if(is.null(plotargs$yaxs)) plotargs$yaxs = "i"
        if(is.null(plotargs$asp))  plotargs$asp  = 1
        if(is.null(plotargs$bty))  plotargs$bty  = "n"
    }
    if(is.null(plotargs$nlevel)) plotargs$nlevel = 25
    if(is.null(plotargs$col))    plotargs$col    = heat.colors(plotargs$nlevel)
    if(is.null(plotargs$ylim))   plotargs$zlim   = switch(
        which,
        "pdot"    = c(0,1),
        "density" = range(delta)
    )
    nlevel = plotargs$nlevel
    plotargs$nlevel = NULL
    # image
    do.call(image, plotargs)
    # traps
    plot_traps(traps, add = TRUE, cex = 1)
    if(abs(diff(range(plotargs$zlim))) > 0){
        # contour
        contour(plotargs$x, plotargs$y, plotargs$z, add = TRUE)
        # legend
        plotargs$nlevel = nlevel
        plotargs$graphics.reset = TRUE
        plotargs$legend.only = TRUE
        plotargs$breaks = seq(plotargs$zlim[1], plotargs$zlim[2], length = nlevel + 1)
        do.call(fields::image.plot, plotargs)
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
        capthist = x$capthist
        if(is.null(mask)) mask = x$mask
    }
    if(inherits(x, c("gibbonsecr_fit", "secr"))){
        capthist = x$capthist
        if(is.null(mask)) mask = x$mask
    }
    capthist = MS(capthist)
    if(!is.null(mask)) mask = MS(mask)
    lim = function(x) apply(do.call(rbind, lapply(x, as.matrix)), 2, range)
    lim = if(is.null(mask)) lim(traps(capthist)) else lim(mask)
    plot(0, 0, xlim = lim[,1], ylim = lim[,2], asp = 1, xlab = "x", ylab = "y")
    for(i in 1:length(capthist)){ # i=1
        traps = traps(capthist[[i]])
        points(traps, col = i, pch = 15)
        if(!is.null(mask)){
            # points(as.data.frame(mask[[i]]), col = i, pch = 15)
            buffer.contour(traps, mask_buffer(mask[[i]], traps), col = i, lty = ceiling(i / 10), add = TRUE)
        }
    }
    title("Listening post locations")
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Plot a fitted gibbon secr model
#' @description TODO
#' @inheritParams print.gibbonsecr_fit
#' @param which TODO
#' @param session TODO
#' @param CI TODO
#' @param nx TODO
#' @param use.global.par.settings TODO
#' @details TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link{gibbonsecr_fit}
#' @method plot gibbonsecr_fit
#' @export

plot.gibbonsecr_fit = function(fit, which = c("detectfn","pdot","bearings","distances","density"), session = 1, use.global.par.settings = FALSE, ...){
    which = match.arg(which)
    if(!ms(fit$capthist)) stop("only works for multi-session data")
    if(which %in% c("bearings","distances"))
        if(fit$model.options[[which]] == 0) stop("no ", which, " model")
    pop = par(no.readonly = TRUE)
    on.exit(par(pop))
    if(is.numeric(session))
        session = session(fit$capthist)[session]
    if(which %in% c("detectfn","bearings","distances")){
        if(!use.global.par.settings)
            pop = par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(4,4,2,2))    
        plot_detectfn_auxiliary(fit, which = which, ...)
    }else{
        if(!use.global.par.settings)
            pop = par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(4,4,2,6))    
        plot_surface(fit, which = which, session = session, ...)
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Plot simulation results
#' @description TODO
#' @inheritParams print.gibbonsecr_fit
#' @param CI TODO
#' @param exp TODO
#' @param use.global.par.settings TODO
#' @details TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @seealso \link{gibbonsecr_sim}
#' @method plot gibbonsecr_sim
#' @export

plot.gibbonsecr_sim = function(x, ..., CI = FALSE, exp = FALSE, use.global.par.settings = FALSE){
    
    ################################################## 
    ## par settings
    
    if(!use.global.par.settings){
        op = par(no.readonly = TRUE)
        npars = ncol(x$gibbonsecr_fit)
        par(mfrow = n2mfrow(npars), mar = c(2,2,2,2), oma = c(0,0,2,0))
    }
    
    for(i in colnames(x$gibbonsecr_fit)){ # i = colnames(x$gibbonsecr_fit)[1] ; i
        
        ################################################## 
        ## gibbonsecr_fit results
        
        j = colnames(x$gibbonsecr_fit) == i
        truth  = attr(x, "truth")[j]
        est1   = x$gibbonsecr_fit[,j]
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
            k = colnames(x$secr.fit) == i
            if(any(k)){
                est0   = x$secr.fit[,k]
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
        
        plot(density(est1), main = i, xlab = "", ylab = "", xlim = xrange, col = 4)
        abline(v = truth, lty = 2)
        abline(v = mu1, col = 4)
        if(CI) abline(v = c(upper1, lower1), lty = 3, col = 4)
        
        if(!is.null(x$secr.fit) && any(k)){
            lines(density(est0), col = 2)
            abline(v = mu0, col = 2)
            if(CI) abline(v = c(upper0, lower0), lty = 3, col = 2)
        }        
    }
    
    ################################################## 
    ## add title
    
    if(!use.global.par.settings){
        title(attr(x, "simname"), outer = TRUE)
        par(op)
    }
    
    invisible()
    
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @param mask TODO
#' @param traps TODO
#' @param region TODO
#' @param add TODO
#' @param covariate TODO
#' @param col TODO
#' @param pch TODO
#' @param bty TODO
#' @param axes TODO
#' @param xlab TODO
#' @param ylab TODO
#' @param main TODO
#' @param legend TODO
#' @param xaxs TODO
#' @param yaxs TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export
plot_mask = function(mask, traps = NULL, region = NULL, add = FALSE, covariate = NULL, col = NULL, pch = 15, bty = "n", axes = FALSE, xlab = "", ylab = "", main = "", legend = TRUE, xaxs = "r", yaxs = "r"){
    if(!inherits(mask, "mask"))
        stop("requires a mask object", call. = FALSE)
    if(!add){
        bbox = mask_bbox(mask)
        plot(bbox, type = "n", asp = 1, bty = bty, axes = axes, xlab = xlab, ylab = ylab, main = main, xaxs = xaxs, yaxs = yaxs)
    }
    plot(mask, add = TRUE, pch = pch, covariate = covariate, col = col, legend = legend)
    if(!is.null(traps)) plot_traps(traps, add = TRUE)
    if(!is.null(region)) sp::plot(region, add = TRUE)
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @param shp TODO
#' @param traps TODO
#' @param add TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export
plot_shp = function(shp, traps = NULL, add = FALSE){
    if(!inherits(shp, c("SpatialPolygons", "SpatialPolygonsDataFrame")))
        stop("requires a SpatialPolygons or SpatialPolygonsDataFrame object", call. = FALSE)
    if(!add){
        plot(t(attr(shp, "bbox")), type = "n", asp = 1)
    }
    n_layers = length(shp@polygons)
    col = if(n_layers == 1) NA else terrain.colors(n_layers)
    lapply(1:n_layers, function(i){
        lapply(shp@polygons[[i]]@Polygons, function(x){
            polygon(x@coords, col = col[i])
        })
    })
    if(!is.null(traps)){
        plot_traps(traps, add = TRUE)
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title TODO
#' @description TODO
#' @param x TODO
#' @param add TODO
#' @param pch TODO
#' @param cex TODO
#' @param col TODO
#' @param ... TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export
plot_traps = function(x, add = FALSE, pch = 15, cex = 0.5, col = 1, ...){
    if(!inherits(x, c("capthist", "traps")))
        stop("requires a capthist or traps object", call. = FALSE)
    if(inherits(x, "capthist"))
        x = traps(x)
    if(!ms(x)) x = list(x)
    if(!add){
        bbox = apply(do.call(rbind, lapply(x, as.data.frame)), 2, range)
        plot(bbox, type = "n", ...)
    }
    for(i in 1:length(x)){ # i=1
        points(as.data.frame(x[[i]]), pch = pch, cex = cex, col = col)
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
