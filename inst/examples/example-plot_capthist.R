\dontrun{

data(N.annamensis)
capthist = N.annamensis.capthist

# plot methods -----------------------------------------------------------------

# listening posts
plot(capthist)

# arrays
plot(capthist, "arrays")

# overlay
plot(capthist, "arrays", cex = 4, pch = 1)
points(capthist)
points(capthist, sessions = 1:3, col = "red", cex = 1)
points(capthist, "arrays", sessions = 4, col = 4, cex = 4, pch = 19)

# ggplot methods ---------------------------------------------------------------

library(ggplot2)

# listening posts
ggplot() + coord_fixed() +
    geom_capthist(capthist) +
    labs(x = "Longitude", y = "Latitude")

# arrays
ggplot() + coord_fixed() +
    geom_capthist(capthist, "arrays") +
    labs(x = "Longitude", y = "Latitude")

# overlay
ggplot() + coord_fixed() +
    geom_capthist(capthist) +
    geom_capthist(capthist, "arrays", pch = 1, cex = 9) +
    geom_capthist(capthist, sessions = 1:3, col = "red", cex = 1) +
    geom_capthist(capthist, "arrays", sessions = 4, col = 4, cex = 9, pch = 19) +
    labs(x = "Longitude", y = "Latitude")
}


