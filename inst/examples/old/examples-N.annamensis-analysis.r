\dontrun{

# load data --------------------------------------------------------------------

data(N.annamensis)
summary(N.annamensis.capthist)
summary(N.annamensis.mask)

# plot methods for mask and traps ----------------------------------------------

par(mar = c(2,2,0.5,0.5))
plot(N.annamensis.mask)
points(N.annamensis.capthist)

# ggplot methods for mask and traps --------------------------------------------

library(ggplot2)
ggplot() + coord_fixed() +
    geom_mask(N.annamensis.mask, cex = 0.5) +
    geom_capthist(N.annamensis.capthist, cex = 1) +
    labs(x = "Longitude", y = "Latitude")

# fit a model ------------------------------------------------------------------

# use the default model options:
# - half normal detection function
# - von Mises bearings error distribution
# - no model for estimated distances
fit = gfit(
    capthist = N.annamensis.capthist,
    mask     = N.annamensis.mask,
    model.options = list(detfunc = 0, bearings = 1)
)

# method functions
summary(fit)
AIC(fit)
coef(fit)
vcov(fit)
confint(fit)
predict(fit)

# plot methods for fitted models -----------------------------------------------

par(mar = c(4,4,2,2))

# detction function
plot(fit, "detfunc")
plot(fit, "detfunc", ci = TRUE) # delta method by default
plot(fit, "detfunc", ci = TRUE, method = "boot") # parametric bootstrap

# detction surface
plot(fit, "detsurf", session = 1)
plot(fit, "detsurf", session = 1, contour = TRUE, add = TRUE, nlevels = 5)
points(N.annamensis.capthist, session = 1, cex = 1)

# bearings distribution
plot(fit, "bearings")
plot(fit, "bearings", ci = TRUE)
plot(fit, "bearings", ci = TRUE, method = "boot")

# density surface
plot(fit, "densurf")
points(N.annamensis.capthist, "array")
plot(fit, "densurf", session = 1, contour = TRUE, add = TRUE, nlevels = 5)

# ggplot methods for fitted models ---------------------------------------------

library(ggplot2)

# detction function
ggplot() + ylim(0, 1) +
    geom_fit(fit, "detfunc")
ggplot() + ylim(0, 1) +
    geom_fit(fit, "detfunc") +
    geom_fit(fit, "detfunc", ci = TRUE) # delta method by default
ggplot() + ylim(0, 1) +
    geom_fit(fit, "detfunc") +
    geom_fit(fit, "detfunc", ci = TRUE, method = "boot") # parametric bootstrap

# detction surface
ggplot() + coord_fixed() +
    geom_fit(fit, "detsurf", session = 1) +
    geom_fit(fit, "detsurf", session = 1, contour = TRUE) +
    scale_fill_distiller(palette = "Spectral", limits = c(0, 1)) +
    geom_capthist(fit$capthist, sessions = 1, cex = 2)

# bearings distribution
ggplot() +
    geom_fit(fit, "bearings")
ggplot() +
    geom_fit(fit, "bearings") +
    geom_fit(fit, "bearings", ci = TRUE)
ggplot() +
    geom_fit(fit, "bearings") +
    geom_fit(fit, "bearings", ci = TRUE, method = "boot")

# density surface
ggplot() + coord_fixed() +
    geom_fit(fit, "densurf") +
    geom_capthist(fit$capthist, "array", cex = 2)

# # fit with secr package ---------------------------------------------------
#
# # secr methods
# library(secr)
# plot(N.annamensis.mask[[1]], asp = 1)
# plot(N.annamensis.capthist[[1]], add = TRUE)
# plot(traps(N.annamensis.capthist[[1]]), add = TRUE)
#
# # secr.fit
# fit0 = suppressWarnings(secr.fit(
#     capthist = as_secr(N.annamensis.capthist),
#     mask     = as_secr(N.annamensis.mask),
#     fixed    = list(g0 = 1),
#     trace    = FALSE
# ))
# exp(coef(fit0)["D", "beta"]) * 100 # N/km2
# exp(coef(fit0)["sigma", "beta"]) # metres

}
