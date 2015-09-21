

# setup -------------------------------------------------------------------

library(secr)
library(rgdal)
library(gibbonsecr)

main.folder = "~/Dropbox/packages/gibbonsecr/gibbonsecr_1.0/gibbonsecr"
wd = setwd(main.folder)
op = options(stringsAsFactors = FALSE)



# process raw data and add to inst/extdata  -------------------------------

## gis files ##

# load from data_raw folder
dsn.raw = path.expand(file.path(main.folder, "data_raw/N.annamensis/gis"))
region  = readOGR(dsn = dsn.raw, layer = "Veun Sai survey area", verbose = FALSE)
habitat = readOGR(dsn = dsn.raw, layer = "habitat",              verbose = FALSE)

# save to inst/extdata folder
dsn.extdata  = path.expand(file.path(main.folder, "inst/extdata/N.annamensis"))
writeOGR(obj = region,  dsn = dsn.extdata, layer = "region",  driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(obj = habitat, dsn = dsn.extdata, layer = "habitat", driver = "ESRI Shapefile", overwrite_layer = TRUE)


# use import_data ---------------------------------------------------------

# dir(system.file("extdata", package = "gibbonsecr"))
# dir(system.file("extdata/gis", package = "gibbonsecr"))

# setwd(system.file("extdata/N.annamensis", package = "gibbonsecr"))
setwd(file.path(main.folder, "inst/extdata/N.annamensis"))
detections = "detections.csv"
posts      = "posts.csv"
covariates = "covariates.csv"
# region     = file.path(main.folder, "inst/extdata/N.annamensis/region.shp")
# habitat    = file.path(main.folder, "inst/extdata/N.annamensis/habitat.shp")

file.exists(detections)
file.exists(posts)
file.exists(covariates)
# file.exists(region)
# file.exists(habitat)

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

N.annamensis = import_data(
    detections = detections,
    posts      = posts,
    covariates = covariates,
    # region     = region,
    # habitat    = habitat,
    details    = details
)


# save --------------------------------------------------------------------

save(N.annamensis, file = file.path(main.folder, "data/N.annamensis.rda"))



# clean up ----------------------------------------------------------------

setwd(wd)
options(op)

## ************************************************************************** ##
## ************************************************************************** ##
