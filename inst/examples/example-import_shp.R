\dontrun{

folder = system.file("extdata/N.annamensis", package = "gibbonsecr")
region = import_shp(file.path(folder, "region.shp"))
habitat = import_shp(file.path(folder, "habitat.shp"))
}
