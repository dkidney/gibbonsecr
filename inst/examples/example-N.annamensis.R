\dontrun{

library(gibbonsecr)
par(mfrow = c(1,1), mar = c(4,4,2,1))


# import survey data -----

folder = system.file("extdata/N.annamensis", package = "gibbonsecr")
capthist = import_data(
    detections = file.path(folder, "detections.csv"),
    posts      = file.path(folder, "posts.csv"),
    covariates = file.path(folder, "covariates.csv"),
    details    = list(bearings = list(units = "degrees", type = "continuous"))
)
summary(capthist)
plot(capthist)


# import shapefiles -----

region = import_shp(file.path(folder, "region.shp"))
plot(region)
points(capthist)

habitat = import_shp(file.path(folder, "habitat.shp"))
plot(habitat)
plot(region, add = TRUE)
points(capthist)


# make mask -----

mask1 = make_mask(capthist, buffer = 6000, spacing = 100)
summary(mask1)
plot(mask1)
points(capthist)

mask2 = make_mask(capthist, buffer = 6000, spacing = 100, poly = region$sp)
summary(mask2)
plot(mask2)
points(capthist)

mask3 = add_covariates(mask2, habitat)
summary(mask3)
plot(mask3, covariate = "habitat", col = c("darkgreen","lightgreen"))
points(capthist)


# fit model ----

fit1 = gfit(capthist = capthist, mask = mask1)
summary(fit1)

fit2 = gfit(capthist = capthist, mask = mask2)
summary(fit2)

fit3 = gfit(capthist = capthist, mask = mask3, model = list(D ~ habitat))
summary(fit3)

AIC(fit2)
AIC(fit3)


# plot results -----

plot(fit3)
plot(fit3, ci = TRUE)
plot(fit3, ci = TRUE, method = "boot")

plot(fit3, "bearings")
plot(fit3, "bearings", ci = TRUE)
plot(fit3, "bearings", ci = TRUE, method = "boot")

plot(fit3, "detsurf", session = 7)
points(fit3$capthist, session = 7)

plot(fit3, "densurf", col = c("darkgreen","lightgreen"))
points(fit3$capthist)


# get predictions ----

newdata = data.frame(habitat = c("primary", "secondary"))
predict(fit3, submodels = "D", newdata = newdata)
}
