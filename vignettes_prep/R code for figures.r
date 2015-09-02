## ************************************************************************** ##
## ************************************************************************** ##

## R code for figures used in gibbonsSECR GUI manual

## ************************************************************************** ##
## ************************************************************************** ##

# rm(list = ls())

suppressPackageStartupMessages({
    require(secr)
    require(gibbonsSECR)
    require(CircStats)
})

figures.folder = "~/Packages/gibbonsSECR/vignettes/figures"
setwd(figures.folder) ; getwd()


# settings ----------------------------------------------------------------

cex = 1.5
col = 1
pch = 15
op = par(no.readonly = TRUE)

width = c(9, 12, 16)
height = c(6, 5, 10)
save(width, file = "plot_width.rda")

jpeg2 = function(title, width = 8, height = 8, units = "cm", pointsize = 6, quality = 150, bg = "white", res = 300, family = "serif"){
    jpeg(paste0(title,".jpeg"), width = width, height = height, units = units, pointsize = pointsize, quality = quality, bg = bg, res = res, family = family)
    
}

par2 = function(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0), cex = 1, cex.main = 1, cex.axis = 1, cex.lab = 1, ...) par(mfrow = mfrow, oma = oma, mar = mar, cex = cex, cex.main = cex.main, cex.axis = cex.axis, cex.lab = cex.lab, ...)


# arrays ------------------------------------------------------------------

# square array

array = expand.grid(x = 1:5, y = 1:5) ; head(array) ; dim(array)
capthist = matrix(c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,1,0,0,0), nc = 1)
buffer = 10
lim = range(array[,1])+c(-1,1)*buffer ; lim
boundary = cbind(c(lim[1],lim[1],lim[2],lim[2],lim[1]),c(lim[1],lim[2],lim[2],lim[1],lim[1]))
mask = secr:::make.mask(array, buffer, spacing = 0.1) ; M = nrow(mask) ; M
distances = gibbonsSECR:::calc_distances(as.matrix(array), as.matrix(mask)) ; dim(distances)
g0 = 0.95
sigma = 0.5
det.probs = secr:::HN(distances, c(g0,sigma)) ; head(det.probs) ; dim(det.probs)
L = matrix(apply(det.probs, 1, function(x) prod(x^capthist * (1-x)^(1-capthist))), sqrt(M))

jpeg2("array1", width[1], height[1])
# par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0), cex = 1, cex.main = 1, cex.axis = 1, cex.lab = 1)
par2()
plot(array, pch = pch, axes = FALSE, xlab = "", ylab = "", asp = 1, col = c(col,"red")[as.factor(capthist)], xlim = c(0.75,5.25), ylim = c(0.75,5.25), cex = cex)
contour(unique(mask[,1]), unique(mask[,2]), L, add = TRUE)
dev.off() ; par(op)


# 3 by 1 array
group = data.frame(x = 0.5, y = 0.8)
capthist = c(0, 1, 1)
traps = secr:::make.grid(nx = 3, ny = 1, spacing = 0.5, detector = "proximity", originxy = c(-0.5,0))
mask = secr:::make.mask(traps, buffer = 3, spacing = 0.1)
x = unique(mask$x)
y = unique(mask$y)
dist = calc_distances(as.matrix(traps), as.matrix(mask))
bear = calc_bearings(as.matrix(traps), as.matrix(mask))
sigma = 1.247
kappa = 70
prob = secr:::HN(dist, c(1,sigma))
xlim = c(-1.5, 1.5)
ylim = c(-2, 2)

jpeg2("array2", width[1], height[1])
# par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0), cex = 1, cex.main = 1, cex.axis = 1, cex.lab = 1)
par2()
plot(as.matrix(traps), axes = FALSE, pch = 15, asp = 1, xlim = xlim, ylim = ylim, cex = cex, col = c(1,2)[capthist+1])
l1 = apply(sapply(1:3, function(k){ # k=1
    prob[,k] * capthist[k] + (1-prob[,k]) * (1-capthist[k])
}), 1, prod) ; l1
z = matrix(l1, nrow = length(x))
contour(x, y, z, add = TRUE)
dev.off()

jpeg2("array3", width[1], height[1])
par2()
# par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0), cex = 1, cex.main = 1, cex.axis = 1, cex.lab = 1)
plot(as.matrix(traps), axes = FALSE, pch = 15, asp = 1, xlim = xlim, ylim = ylim, cex = cex, col = c(1,2)[capthist+1])
true.bearing = calc_bearings(as.matrix(traps), as.matrix(group)) ; true.bearing
l2 = apply(sapply(which(capthist == 1), function(k){ # k=2
    dvm(bear[,k], true.bearing[k], kappa)
}), 1, prod)
z = matrix(l1*l2, nrow = length(unique(mask$x)))
contour(x, y, z, add = TRUE)
arrows(x0 = c(0, 0.5), y0 = 0, x1 = group$x + c(-0.1,0), y1 = group$y + c(0.1,-0.1), col = 2, lty = 1)
dev.off()



# bearings ----------------------------------------------------------------

plot2 = function(main) plot(1, 1, type = "n", xlim = c(-90,90), ylim = c(0,3.5), ylab = "Probability density", xlab = "Bearing error (degrees)", bty = "n", xaxs = "i", yaxs = "i", main = main)
legend2 = function(x) legend('topright', paste(x), bty = "n", col = 2:4, lwd = 2)

jpeg2("bearing_error", width[2], height[2])

# par(mfrow = c(1,2), mar = c(4,4,2,2), cex = 1, cex.main = 1, cex.axis = 1, cex.lab = 1)
par2(mfrow = c(1,2), mar = c(4,4,2,2)) 
plot2("von Mises")
kappa = c(60, 20, 5)
for(i in 1:length(kappa)) curve(dvm(x * pi/180, 0, kappa[i]), lwd = 2, add = TRUE, col = i+1)
legend2(kappa)

plot2("wrapped Cauchy")
kappa = c(0.9, 0.8, 0.7)
for(i in 1:length(kappa)) curve(dwrpcauchy(x * pi/180, 0, kappa[i]), lwd = 2, add = TRUE, col = i+1)
legend2(kappa)

dev.off() ; par(op)



# detection function ------------------------------------------------------

plot2 = function(main) plot(1, 1, type = "n", xlim = c(0,1), ylim = c(0,1), ylab = "Detection probability", xlab = "Distance (km)", bty = "n", xaxs = "i", yaxs = "i", main = main)
legend2 = function(x) legend('topright', paste(x), title = deparse(substitute(x)), bty = "n", col = 2:4, lwd = 2)

jpeg2("detection_function", width[3], height[3])

# par(mfrow = c(2,3), mar = c(4.5,4,4,2.5), oma = c(0,0,0,0), cex = 1, cex.main = 1, cex.axis = 1, cex.lab = 1)
par2(mfrow = c(2,3), mar = c(4.5,4,4,2.5))

sigma = c(0.2, 0.4, 0.6)
g0    = c(0.5, 0.75, 1)
z     = c(2, 5, 10)

plot2("Half normal")
for(i in 1:3) curve(secr:::HN(x, c(1,sigma[i])), lwd = 2, add = TRUE, col = i+1)
legend2(sigma)

plot2("Half normal")
for(i in 1:3) curve(secr:::HN(x, c(g0[i],0.5)), lwd = 2, add = TRUE, col = i+1)
legend2(g0)

plot.new()

plot2("Hazard rate")
for(i in 1:3) curve(secr:::HR(x, c(1,sigma[i],5)), lwd = 2, add = TRUE, col = i+1)
legend2(sigma)

plot2("Hazard rate")
for(i in 1:3) curve(secr:::HR(x, c(g0[i],0.5,5)), lwd = 2, add = TRUE, col = i+1)
legend2(g0)

plot2("Hazard rate")
for(i in 1:3) curve(secr:::HR(x, c(1,0.5,z[i])), lwd = 2, add = TRUE, col = i+1)
legend2(z)

dev.off() ; par(op)



# detection surface -------------------------------------------------------


pdot = 1 - apply(1 - prob, 1, prod)
z = matrix(pdot, nrow = length(unique(mask$x)))

jpeg2("detection_surface1", width[1], height[1])
par2()
persp(x, y, z, theta = 30, phi = 30, xlab = "x", ylab = "y", zlab = "Probability", expand = 0.5, zlim = c(0,1))
dev.off() ; par(op)

jpeg2("detection_surface2", width[1], height[1])
par2(mar = c(2,2,0.5,0))
filled.contour(x, y, z, asp = 1, zlim = c(0,1), frame.plot = FALSE, xaxs = "i", yaxs = "i",
               color.palette = heat.colors,
               plot.axes= {
                   points(as.matrix(traps), pch = 15, cex = 1.5)
                   contour(x, y, z, add = TRUE)
                   axis(1)
                   axis(2)
               })
dev.off() ; par(op)


jpeg2("detection_surface3", width[3], height[1])

# par(mfrow = c(1,3), mar = c(0,1,0,0), oma = c(0,0,0,0), cex = 1, cex.main = 1, cex.axis = 1, cex.lab = 1)
par2(mfrow = c(1,3), mar = c(0,1,0,0))
n = 26
x = 1:n
y = 1:n
z = matrix(0.5,n,n)
persp(x, y, z, theta = 30, phi = 30, xlab = "x", ylab = "y", zlab = "Probability", expand = 0.5, zlim = c(0,1))

z[1:(nrow(z)/2),] = 1
z[(nrow(z)/2+1):nrow(z),] = 0
persp(x, y, z, theta = 30, phi = 30, xlab = "x", ylab = "y", zlab = "Probability", expand = 0.5, zlim = c(0,1))

for(j in 1:ncol(z)) z[,j] = (j-1)/(ncol(z)-1)
persp(x, y, z, theta = 30, phi = 30, xlab = "x", ylab = "y", zlab = "Probability", expand = 0.5, zlim = c(0,1))

dev.off() ; par(op)







# filename = "detection_surface_1.jpeg"
# jpeg2(filename, 8, 8)
# pdot = matrix(gibbonsSECR:::calc_pdot(distances, 1, theta, 1, 1), sqrt(M))
# plot(array, axes = FALSE, xlab = "", ylab = "", asp = 1, xlim = c(0,6), ylim = c(0,6))
# image(unique(mask[,1]), unique(mask[,2]), pdot, add = TRUE)
# contour(unique(mask[,1]), unique(mask[,2]), pdot, add = TRUE, labcex = 1)
# points(array, pch = pch, cex = cex, col = col)
# dev.off() 
# 
# filename = "detection_surface_2.jpeg"
# jpeg(filename = filename, width = width, height = width, units = "cm", pointsize = pointsize, quality = quality, res = res, family = family)
# par(oma = c(0,0,0,0), mar = c(0,1,0,1))
# pdot = matrix(gibbonsSECR:::calc_pdot(distances, 1, theta, 1, 1), sqrt(M))
# persp(unique(mask[,1]), unique(mask[,2]), pdot, theta = 30, phi = 30, xlab = "x", ylab = "y", zlab = "Probability", expand = 0.5, zlim = c(0,1))
# dev.off() 


# ################################################################################
# # Density surface
# 
# filename = "density_surface_1.jpeg"
# 
# jpeg(filename = filename, width = width, height = width, units = "cm", pointsize = pointsize, quality = quality, res = res, family = family)
# 
# density = matrix(apply(mask, 1, function(x) x[2]), sqrt(M))
# 
# par(op)
# 
# persp(unique(mask[,1]), unique(mask[,2]), density, theta = 30, phi = 30, xlab = "x", ylab = "y", zlab = "Density", expand = 0.5)
# 
# dev.off() 
# 
# #-------------------------------------------------------------------------------
# 
# filename = "density_surface_2.jpeg"
# 
# jpeg(filename = filename, width = width, height = width, units = "cm", pointsize = pointsize, quality = quality, res = res, family = family)
# 
# par(op)
# 
# persp(unique(mask[,1]), unique(mask[,2]), density*pdot, theta = 30, phi = 30, xlab = "x", ylab = "y", zlab = "Density", expand = 0.5, zlim = range(density))
# 
# dev.off() 
# 
# 
# ################################################################################
# # Skates
# 
# require(skatesSECR) ; data(jura)
# 
# op = par(oma = c(0,0,0,0), mar = c(1,1,4,1))
# 
# filename = "skates_sufrace_0.jpeg"
# 
# jpeg(filename = filename, width = width, height = width, units = "cm", pointsize = pointsize, quality = quality, res = res, family = family)
# 
# par(op)
# 
# plot(jura$boundary, type = "l", asp = 1, xaxs = "i", yaxs = "i", axes = FALSE, main = "", xlab = "", ylab = "")
# 
# points(jura$detectors, pch = 15, cex = 2)
# 
# dev.off() 
# 
# if(0){
#     
#     # model.options = list(g0     = list(form = ~1, pars = boot:::logit(0.9)),
#     #                      sigma  = list(form = ~1, pars = log(5)),
#     #                      rho    = list(form = ~1, pars = boot:::logit(0.5)),
#     #                      density = list(form = ~1, pars = rep(0,1)))
#     # 
#     # fit = fit.skates.secr(jura, model.options, method = "nlm", itnmax = 1000) ; fit
#     # 
#     # par(mfrow = c(2,3)) ; plot(fit)
#     
#     #-------------------------------------------------------------------------------
#     
#     # model.options = list(g0     = list(form = ~1, pars = boot:::logit(0.9)),
#     #                      sigma  = list(form = ~1, pars = log(5)),
#     #                      rho    = list(form = ~1, pars = boot:::logit(0.5)),
#     #                      density = list(form = ~z, pars = rep(0,2)))
#     # 
#     # fit = fit.skates.secr(jura, model.options, method = "nlm", itnmax = 1000) ; fit
#     # 
#     # par(mfrow = c(2,3)) ; plot(fit)
#     
#     #-------------------------------------------------------------------------------
#     
#     # model.options = list(g0     = list(form = ~1, pars = boot:::logit(0.9)),
#     #                      sigma  = list(form = ~1, pars = log(5)),
#     #                      rho    = list(form = ~1, pars = boot:::logit(0.5)),
#     #                      density = list(form = ~z + z:sex, pars = rep(0,3)))
#     # 
#     # fit = fit.skates.secr(jura, model.options, method = "nlm", itnmax = 1000) ; fit
#     # 
#     # par(mfrow = c(2,3)) ; plot(fit)
#     
#     #-------------------------------------------------------------------------------
#     
#     model.options = list(g0     = list(form = ~1, pars = boot:::logit(0.9)),
#                          sigma  = list(form = ~1, pars = log(5)),
#                          rho    = list(form = ~1, pars = boot:::logit(0.5)),
#                          density = list(form = ~z + z:week, pars = rep(0,3)))
#     
#     fit = fit.skates.secr(jura, model.options, method = "nlm", itnmax = 1000) ; fit
#     
#     par(mfrow = c(2,3)) ; plot(fit, s = 1)
#     par(mfrow = c(2,3)) ; plot(fit, s = 17)
#     par(mfrow = c(2,3)) ; plot(fit, s = 34)
#     
#     #-------------------------------------------------------------------------------
#     
#     # model.options = list(g0     = list(form = ~1, pars = boot:::logit(0.9)),
#     #                      sigma  = list(form = ~1, pars = log(5)),
#     #                      rho    = list(form = ~1, pars = boot:::logit(0.5)),
#     #                      density = list(form = ~z + z:sex + z:week, pars = rep(0,4)))
#     # 
#     # fit = fit.skates.secr(jura, model.options, method = "nlm", itnmax = 1000) ; fit
#     # 
#     # par(mfrow = c(2,3)) ; plot(fit, s = 1)
#     # par(mfrow = c(2,3)) ; plot(fit, s = 17)
#     # par(mfrow = c(2,3)) ; plot(fit, s = 34)
#     
#     save(fit, model.options, file = "skates_fit.RData")
#     
# }else{
#     
#     load("skates_fit.RData")
#     
# }
# 
# #-------------------------------------------------------------------------------
# 
# covariates   = fit$data$covariates$individual
# 
# boundary     = fit$data$boundary
# 
# parameters   = estimates(fit)
# 
# detectors    = fit$data$detectors
# 
# par = skatesSECR:::get.model.pars(model.options) ; par
# 
# codes = par$codes ; codes
# 
# vals = if(is.null(parameters)) par$vals else parameters ; vals
# 
# grid = make.mask(spacing = 0.5, boundary = make.bbox(boundary), z.axis = 1.75)
# 
# xs = unique(grid[, 1])
# 
# ys = unique(grid[, 2])
# 
# inside = SDMTools:::pnt.in.poly(grid[,c("x","y")], boundary)[3] = = 1 
# 
# a = attr(grid, "area.cell") ; a
# 
# intercept = log(1 / (sum(inside) * a)) ; intercept
# 
# N.covariates = data.frame(sex = c("M", "F"), length = tapply(covariates$length, covariates$sex, mean))
# 
# N.covariates$sex = relevel(N.covariates$sex, ref = "M") ; N.covariates$sex
# 
# M.covariates = grid[inside, ]
# 
# par = skatesSECR:::get.model.pars(model.options) ; par
# 
# #-------------------------------------------------------------------------------
# 
# filename = "skates_detection_surface.jpeg"
# 
# M = nrow(grid) ; M
# distances = gibbonsSECR:::calc_distances(as.matrix(detectors), as.matrix(grid)) ; dim(distances)
# g0 = boot:::inv.logit(parameters["g0_(Intercept)"]) ; g0
# sigma = exp(parameters["sigma_(Intercept)"]) ; sigma
# theta = c(g0, sigma)
# pdot = matrix(gibbonsSECR:::calc_pdot(distances, 1, theta, 1, 1), length(unique(grid[,1])), length(unique(grid[,2])))
# pdot[!inside] = NA
# 
# jpeg(filename = filename, width = width, height = width, units = "cm", pointsize = pointsize, quality = quality, res = res, family = family)
# 
# par(op)
# 
# plot(boundary, type = "l", asp = 1, xaxs = "i", yaxs = "i", axes = FALSE, main = "", xlab = "", ylab = "")
# 
# points(detectors, pch = 15, cex = 2)
# 
# image(unique(grid[,1]), unique(grid[,2]), pdot, add = TRUE)
# 
# contour(unique(grid[,1]), unique(grid[,2]), pdot, add = TRUE, labcex = 1)
# 
# points(detectors, pch = 15, cex = 2)
# 
# lines(boundary)
# 
# dev.off() 
# 
# #-------------------------------------------------------------------------------
# 
# S = 34
# 
# for(s in seq(1,S,3)){ # s = 1
#     
#     filename = paste("skates_sufrace_",s,".jpeg", sep = "")
#     
#     jpeg(filename = filename, width = width, height = width, units = "cm", pointsize = pointsize, quality = quality, res = res, family = family)
#     
#     par(op)
#     
#     S.covariates = data.frame(week = ((1:S) - mean(1:S))[s]) ; S.covariates
#     
#     X = skatesSECR:::make.model.matrices(list(density = model.options$density), N.covariates, S.covariates, M.covariates)
#     
#     density = skatesSECR:::make.submodel.arrays(X, vals, codes, N = 2, S = 1, M = nrow(grid[inside, ]), a = a, intercept = intercept)[[1]]
#     
#     plot(boundary, type = "l", asp = 1, xaxs = "i", yaxs = "i", axes = FALSE, main = paste("Occasion",s), xlab = "", ylab = "")
#     
#     z = rep(NA, nrow(grid))
#     
#     z[inside] = density[, 1, 1]
#     
#     z = matrix(z, length(xs), length(ys))
#     
#     zlim = range(density[,1,1], na.rm = TRUE) ; zlim
#     
#     image(xs, ys, z, zlim = zlim, add = TRUE, col = colorRampPalette(c("white", col))(12))
#     
#     lines(boundary)
#     
#     contour(xs, ys, z, add = TRUE)
#     
#     points(detectors, pch = 15, cex = 2)
#     
#     dev.off() 
#     
# }
# 
# ################################################################################
