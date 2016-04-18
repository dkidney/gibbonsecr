\dontrun{

data(N.annamensis)
fit = N.annamensis.fit

# plot methods -----------------------------------------------------------------

# > detection function ----

par(mfrow = c(1,1), mar = c(4,4,2,1))
plot(fit)
plot(fit, ci = TRUE)
plot(fit, ci = TRUE, method = "boot")

# > bearing error ----

plot(fit, "bearings")
plot(fit, "bearings", ci = TRUE)
plot(fit, "bearings", ci = TRUE, method = "boot")

# > detection surface ----

plot(fit, "detsurf", session = 1)
points(fit$capthist, session = 1)

# > density surface ----

plot(fit, "densurf")

# ggplot methods ---------------------------------------------------------------

# > detection function ----

ggplot() +
    geom_fit(fit) +
    labs(x = "Radial distance (m)", y = "Detection probability")

ggplot() +
    geom_fit(fit) +
    geom_fit(fit, ci = TRUE) +
    labs(x = "Radial distance (m)", y = "Detection probability")

ggplot() +
    geom_fit(fit) +
    geom_fit(fit, ci = TRUE, method = "boot") +
    labs(x = "Radial distance (m)", y = "Detection probability")

# > bearing error ----

ggplot() +
    geom_fit(fit, "bearings") +
    labs(x = "Bearing error (degrees)", y = "Probability density")

ggplot() +
    geom_fit(fit, "bearings") +
    geom_fit(fit, "bearings", ci = TRUE) +
    labs(x = "Bearing error (degrees)", y = "Probability density")

ggplot() +
    geom_fit(fit, "bearings") +
    geom_fit(fit, "bearings", ci = TRUE, method = "boot") +
    labs(x = "Radial distance (m)", y = "Detection probability")

# > detection surface ----

ggplot() + coord_fixed() +
    geom_fit(fit, "detsurf", session = 1) +
    geom_fit(fit, "detsurf", session = 1, contour = TRUE) +
    geom_capthist(fit$capthist, session = 1) +
    scale_fill_distiller(palette = "Spectral", limits = c(0, 1)) +
    labs(x = "Longitude", y = "Latitude")

# > density surface ----

ggplot() + coord_fixed() +
    geom_fit(fit, "densurf") +
    geom_capthist(fit$capthist, "array") +
    scale_fill_distiller(palette = "Spectral") +
    labs(x = "Longitude", y = "Latitude")
}


