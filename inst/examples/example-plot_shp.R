\dontrun{

data(N.annamensis)

# plot methods -----------------------------------------------------------------

plot(N.annamensis.region, col = "grey")

plot(N.annamensis.habitat, covariate = "habitat", col = c("darkgreen", "lightgreen"))
plot(N.annamensis.region, add = TRUE)

# ggplot methods ---------------------------------------------------------------

library(ggplot2)

ggplot() + coord_fixed() +
    geom_shp(N.annamensis.region)

ggplot() + coord_fixed() +
    geom_shp(N.annamensis.habitat, covariate = "habitat") +
    scale_fill_manual(values = c("darkgreen", "lightgreen"))

ggplot() + coord_fixed() +
    geom_shp(N.annamensis.habitat, covariate = "habitat") +
    geom_shp(N.annamensis.region) +
    scale_fill_manual(values = c("darkgreen", "lightgreen"))
}
