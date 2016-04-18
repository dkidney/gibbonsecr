\dontrun{

data(N.annamensis)
capthist = N.annamensis.capthist
mask = N.annamensis.mask
fit = N.annamensis.fit

data(peafowl)
capthist = peafowl.capthist
mask = peafowl.mask
fit = peafowl.fit


# plot and points methods ------------------------------------------------------

# all sessions

par(mfrow = c(1,1), mar = c(2,2,0.5,0.5))
plot(mask)
points(capthist)
points(capthist, "arrays", cex = 3, pch = 1)

# subset of sessions

points(mask, sessions = 1:5, col = 2)
points(capthist, sessions = 1:5, col = 4)
points(capthist, "arrays", cex = 3, pch = 1, sessions = 1:5, col = 4)

# mask covariates

plot(mask, covariate = "x")
points(capthist, "arrays")

# bearing estimates

par(mfrow = c(2,2), mar = c(2,2,0.5,0.5))
for(i in 1:4){
    plot(capthist, sessions = i,
         ylim = range(secr::traps(capthist[[i]])$y) + 1000 * c(-1, 1))
    plot(capthist, "detections", sessions = i)
}

# fitted model

par(mfrow = c(1,2), mar = c(4,4,2,2))
plot(fit, "detfunc")
plot(fit, "bearings")
plot(fit, "detfunc", ci = TRUE)
plot(fit, "bearings", ci = TRUE)

par(mfrow = c(1,1), mar = c(4,4,2,2))
plot(fit, "detsurf", session = 1, zlim = c(0,1))
points(fit$capthist, sessions = 1, cex = 0.75)
plot(fit, "densurf")
points(fit$capthist, "arrays")


# ggplot methods ---------------------------------------------------------------

library(ggplot2)

# all sessions

plot = ggplot() + coord_fixed() +
    geom_mask(mask) +
    geom_capthist(capthist, cex = 0.75) +
    geom_capthist(capthist, "array", pch = 1, cex = 7)
print(plot)

# subset of sessions

plot +
    geom_mask(mask, sessions = 1:5, col = 2) +
    geom_capthist(capthist, sessions = 1:5, cex = 0.75, col = 4) +
    geom_capthist(capthist, "arrays", sessions = 1:5, cex = 7, col = 4, pch = 1)

# mask covariates - need to fix make_regular_regionmask and add_covariates

ggplot() + coord_fixed() +
    geom_mask(mask, covariate = "x") +
    geom_mask(mask, covariate = "x", contour = TRUE) +
    scale_fill_gradientn(colors = heat.colors(12)) +
    geom_capthist(capthist, "arrays")

# fitted model

ggplot() + geom_fit(fit)
ggplot() + geom_fit(fit) + geom_fit(fit, ci = TRUE)

ggplot() + geom_fit(fit, "bearings")
ggplot() + geom_fit(fit, "bearings") + geom_fit(fit, "bearings", ci = TRUE)

ggplot() + coord_fixed() +
    geom_fit(fit, "detsurf", session = 1) +
    geom_fit(fit, "detsurf", session = 1, contour = TRUE) +
    scale_fill_gradientn(colors = heat.colors(12), limits = c(0,1)) +
    geom_capthist(capthist, sessions = 1, cex = 1)

ggplot() + coord_fixed() +
    geom_fit(fit, "densurf") +
    geom_fit(fit, "densurf", contour = TRUE) +
    scale_fill_distiller(palette = "Spectral") +
    geom_capthist(capthist, "arrays")

}
