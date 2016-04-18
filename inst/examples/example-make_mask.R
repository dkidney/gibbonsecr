\dontrun{

data(N.annamensis)

mask = make_mask(N.annamensis.capthist, buffer = 6000, spacing = 250)
summary(mask)
class(mask)

head(mask[[1]])
head(covariates(mask)[[1]])

plot(mask)
points(N.annamensis.capthist)
}


