\dontrun{

# import from csv files --------------------------------------------------------

folder = system.file("extdata/N.annamensis", package = "gibbonsecr")
capthist = import_data(
    detections = file.path(folder, "detections.csv"),
    posts      = file.path(folder, "posts.csv"),
    covariates = file.path(folder, "covariates.csv"),
    details    = list(bearings = list(units = "degrees",
                                      type = "continuous"))
)
summary(capthist)

# import capthist data frames --------------------------------------------------

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
    covariates = covariates,
    details    = list(bearings = list(units = "degrees",
                                      type = "continuous"))
)
summary(capthist)
}
