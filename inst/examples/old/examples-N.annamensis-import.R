\dontrun{

# Example: import N.annamensis data ============================================

# import capthist from csv files -----------------------------------------------

folder = system.file("extdata/N.annamensis", package = "gibbonsecr")
capthist = import_data(
    detections = file.path(folder, "detections.csv"),
    posts      = file.path(folder, "posts.csv"),
    covariates = file.path(folder, "covariates.csv"),
    details    = list(bearings = list(units = "degrees",
                                      type = "continuous"))
)
summary(capthist)

# import capthist from data frames ---------------------------------------------

detections = read.csv(file.path(folder, "detections.csv"))
posts      = read.csv(file.path(folder, "posts.csv"))
covariates = read.csv(file.path(folder, "covariates.csv"))
head(detections)
head(posts)
head(covariates)
# using default details
capthist = import_data(
    detections = detections,
    posts      = posts,
    covariates = covariates
)
summary(capthist)

# import shapefiles ------------------------------------------------------------

debugonce(import_shp)
region = import_shp(file.path(folder, "region"))
habitat = import_shp(file.path(folder, "habitat"))

head(habitat$fortified)

# debugonce(gibbonsecr:::plot_shp)
plot(region, col = "grey")
plot(habitat, covariate = "habitat", col = c("darkgreen", "lightgreen"))
plot(region, add = TRUE)

library(ggplot2)
# debugonce(gibbonsecr:::plot_shp)
ggplot() + coord_fixed() +
    geom_shp(region)
ggplot() + coord_fixed() +
    geom_shp(habitat, covariate = "habitat") +
    geom_shp(region) +
    scale_fill_manual(values = c("darkgreen", "lightgreen"))
}
