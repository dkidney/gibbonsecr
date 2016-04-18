\dontrun{

data(N.annamensis)
mask = N.annamensis.mask
capthist = N.annamensis.capthist

# plot methods -----------------------------------------------------------------

par(mar = c(2,2,1,1))

plot(mask)
points(capthist, cex = 0.5)

plot(mask, covariate = "x", legend = FALSE)
points(capthist, cex = 0.5)

plot(mask, covariate = "y", legend = FALSE)
points(capthist, cex = 0.5)

plot(mask, covariate = "habitat", legend = FALSE)
points(capthist, cex = 0.5)

# ggplot methods ---------------------------------------------------------------

library(ggplot2)

ggplot() + coord_fixed() +
    geom_mask(mask) +
    geom_capthist(capthist) +
    labs(x = "Longitude", y = "Latitude")

ggplot() + coord_fixed() +
    geom_mask(mask, covariate = "x") +
    geom_capthist(capthist) +
    scale_fill_distiller(palette = "Spectral") +
    labs(x = "Longitude", y = "Latitude", fill = "Longitude")

ggplot() + coord_fixed() +
    geom_mask(mask, covariate = "y") +
    geom_capthist(capthist) +
    scale_fill_distiller(palette = "Spectral") +
    labs(x = "Longitude", y = "Latitude", fill = "Latitutde")

ggplot() + coord_fixed() +
    geom_mask(mask, covariate = "habitat") +
    geom_capthist(capthist) +
    scale_fill_manual(values = c("darkgreen", "lightgreen")) +
    labs(x = "Longitude", y = "Latitude", fill = "habitat")
}


