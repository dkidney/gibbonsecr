\dontrun{

library(ggplot2)
data(N.annamensis)

# default settings -------------------------------------------------------------

# - half normal detection function
# - von Mises bearings error distribution
# - no model for estimated distances
fit = gfit(
    capthist = N.annamensis.capthist,
    mask     = N.annamensis.mask
)
summary(fit)

# D ~ habitat ------------------------------------------------------------------

fit = gfit(
    capthist = N.annamensis.capthist,
    mask     = N.annamensis.mask,
    model    = list(D ~ habitat)
)
summary(fit)

ggplot() + coord_fixed() +
    geom_fit(fit, "densurf") +
    geom_capthist(fit$capthist, "array") +
    scale_fill_distiller(palette = "Spectral") +
    labs(x = "Longitude", y = "Latitude")

# predict density for each habitat
newdata = data.frame(habitat = c("primary", "secondary"))
predict(fit, submodels = "D", newdata = newdata)


# sigma ~ habitat ------------------------------------------------------------------

fit = gfit(
    capthist = N.annamensis.capthist,
    mask     = N.annamensis.mask,
    model    = list(sigma ~ habitat)
)
summary(fit)

newdata = data.frame(habitat = c("primary", "secondary"))

ggplot() +
    geom_fit(fit, "detfunc", newdata = newdata[1, , drop = FALSE], col = 2) +
    geom_fit(fit, "detfunc", newdata = newdata[2, , drop = FALSE], col = 4) +
    labs(x = "Radial distance (m)", y = "Detection probability")

ggplot() +
    geom_fit(fit, "detfunc", newdata = newdata[1, , drop = FALSE], col = 2) +
    geom_fit(fit, "detfunc", newdata = newdata[1, , drop = FALSE], fill = 2,
             ci = TRUE, method = "boot") +
    geom_fit(fit, "detfunc", newdata = newdata[2, , drop = FALSE], col = 4) +
    geom_fit(fit, "detfunc", newdata = newdata[2, , drop = FALSE], fill = 4,
             ci = TRUE, method = "boot") +
    labs(x = "Radial distance (m)", y = "Detection probability")

# predict density for each habitat
predict(fit, submodels = "sigma", newdata = newdata)


# D ~ s(x, y) ------------------------------------------------------------------

fit = gfit(
    capthist = N.annamensis.capthist,
    mask     = N.annamensis.mask,
    model    = list(D ~ s(x, y, k = 3))
)
summary(fit)

ggplot() + coord_fixed() +
    geom_fit(fit, "densurf") +
    geom_fit(fit, "densurf", contour = TRUE) +
    geom_capthist(fit$capthist, "array") +
    scale_fill_distiller(palette = "Spectral") +
    labs(x = "Longitude", y = "Latitude")

}

