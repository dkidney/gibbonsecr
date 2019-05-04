library(gibbonsecr)
folder = "~/Github/gibbonsecr"

# capthist -----
dsn_extdata  = path.expand(file.path(folder, "inst/extdata/N.annamensis"))
file.exists(dsn_extdata)
detections = file.path(dsn_extdata, "detections.csv")
posts      = file.path(dsn_extdata, "posts.csv")
covariates = file.path(dsn_extdata, "covariates.csv")
file.exists(detections)
file.exists(posts)
file.exists(covariates)
details = list(
    bearings  = list(
        units = "degrees",
        type  = "continuous"
    ),
    distances = list(
        units = "km",
        type  = "continuous"
    )
)
capthist = import_data(
    detections = detections,
    posts      = posts,
    covariates = covariates,
    details    = details
)
class(capthist)
summary(capthist)

# mask -----
mask = make_mask(capthist, spacing = 250, buffer = 6000)
mask
if(0){
    gibbonsecr:::check_mask(mask, capthist)
    class(mask)
    summary(mask)
    gibbonsecr:::size(mask)
    gibbonsecr:::Area(mask)
    gibbonsecr:::area(mask)
    gibbonsecr:::buffer(mask)
    gibbonsecr:::spacing.gmask(mask)
    plot(mask)
}

# shapefiles -----
region = import_shp(file.path(folder, "inst/extdata/N.annamensis/region.shp"))
habitat = import_shp(file.path(folder, "inst/extdata/N.annamensis/habitat.shp"))
mask = add_covariates(mask, habitat)
capthist = add_covariates(capthist, habitat)

# fit -----
fit = gfit(capthist, mask = mask)
fit

# save -----
N.annamensis.capthist = capthist
N.annamensis.mask     = mask
N.annamensis.fit      = fit
N.annamensis.region   = region
N.annamensis.habitat  = habitat
save(N.annamensis.capthist, N.annamensis.mask, N.annamensis.fit,
     N.annamensis.region, N.annamensis.habitat,
     file = file.path(folder, "data/N.annamensis.rda"),
     compress = 'xz')

