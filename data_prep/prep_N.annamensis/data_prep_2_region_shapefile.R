
library(rgdal)
folder = "~/Github/gibbonsecr"

# load region ----
dsn_raw = path.expand(file.path(folder, "data_raw"))
file.exists(dsn_raw)
region  = readOGR(
    dsn = dsn_raw,
    layer = "Veun Sai survey area",
    verbose = FALSE
)

# save to inst/extdata ----
dsn_extdata  = path.expand(file.path(folder, "inst/extdata/N.annamensis"))
file.exists(dsn_extdata)
if(0){
    writeOGR(
        obj = region,
        dsn = dsn_extdata,
        layer = "region",
        driver = "ESRI Shapefile",
        overwrite_layer = TRUE
    )
}