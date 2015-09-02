

# setup -------------------------------------------------------------------

suppressPackageStartupMessages({
    require(rgdal)
})

main.folder = "~/Dropbox/packages/gibbonsecr/gibbonsecr_1.0/gibbonsecr"
wd = setwd(main.folder)
op = options(stringsAsFactors = FALSE)



# load region shp file ---------------------------------------------------------

dsn = path.expand(file.path(main.folder, "data_raw/N.annamensis/gis")) ; dsn

region = readOGR(
    dsn     = dsn,
    layer   = "Veun Sai survey area",
    verbose = FALSE
)

region.coords = region@polygons[[1]]@Polygons[[1]]@coords

head(region.coords)

par(mfrow = c(1,1), mar = c(2,2,0,0))
plot(region.coords, type = "n", asp = 1)
plot(region, add = TRUE)


# make habtiat coords -----------------------------------------------------

poly1 = rbind(c(655000, 1540000),
              c(687500, 1540000),
              c(687500, 1580000),
              c(655000, 1580000))

poly2 = rbind(c(687500, 1540000),
              c(710000, 1540000),
              c(710000, 1580000),
              c(687500, 1580000))

polygon(poly1, col = 2)
polygon(poly2, col = 4)

# convert to list
habitat.coords.list = list(poly1, poly2)

# convert to list of Polygon objects
habitat.Polygon = lapply(1:2, function(i){
    sp::Polygon(
        coords = habitat.coords.list[[i]], 
        hole   = FALSE
    )
})

# convert to list of Polygons objects
habitat.Polygons = lapply(1:2, function(i){
    sp::Polygons(
        srl = list(habitat.Polygon[[i]]),
        ID  = i
    )
})

# convert to SpatialPolygons object
habitat.SpatialPolygons = sp::SpatialPolygons(
    Srl = habitat.Polygons,
    proj4string = sp::CRS(sp::proj4string(region))
)

# convert to SpatialPolygonsDataFrame object
habitat.SpatialPolygonsDataFrame = sp::SpatialPolygonsDataFrame(
    Sr = habitat.SpatialPolygons,
    data = data.frame(habitat = c("primary", "secondary"), stringsAsFactors = TRUE)
)

plot(habitat.SpatialPolygonsDataFrame, col = c("darkgreen","lightgreen"))
plot(region, add = TRUE)


# save --------------------------------------------------------------------

writeOGR(
    obj    = habitat.SpatialPolygonsDataFrame, 
    dsn    = dsn, 
    layer  = "habitat", 
    driver = "ESRI Shapefile",
    overwrite_layer = TRUE
)


# clean up ----------------------------------------------------------------

setwd(wd)
options(op)

## ************************************************************************** ## 
## ************************************************************************** ## 
