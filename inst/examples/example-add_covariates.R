\dontrun{

library(secr)
library(ggplot2)
data(N.annamensis)

mask = make_mask(N.annamensis.capthist, spacing = 250, buffer = 6000)
head(covariates(mask[[1]]))

ggplot() + coord_fixed() +
    geom_mask(mask) +
    geom_capthist(N.annamensis.capthist) +
    labs(x = "Longitude", y = "Latitude")

mask = add_covariates(mask, N.annamensis.habitat)
head(covariates(mask[[1]]))

ggplot() + coord_fixed() +
    geom_mask(mask, covariate = "habitat") +
    geom_capthist(N.annamensis.capthist) +
    scale_fill_manual(values = c("darkgreen", "lightgreen")) +
    labs(x = "Longitude", y = "Latitude")
}


