\dontrun{

data(peafowl)
capthist = peafowl.capthist
mask = peafowl.mask

# capthist ---------------------------------------------------------------------

# plot traps
ggplot(capthist, col = "red")

# plot array centres
ggplot(capthist, "arrays")

# plot specfific sessions
ggplot(capthist, "arrays") +
    geom_capthist(capthist, "arrays", sessions = 1:4, col = "red", cex = 5)

# overlay array centres over trap locations
arrays = geom_capthist(capthist, "arrays")
ggplot(capthist, col = "red", cex = 2) + arrays

# mask ------------------------------------------------------------------

buffer(mask)
spacing(mask)
area(mask)
Area(mask)

# mask points
ggplot(mask, color = "grey", cex = 0.1)

# overlay array centres over mask points
ggplot(mask, color = "grey", cex = 0.1) + arrays

# mask covariates
ggplot(mask, "hab_type")
ggplot(mask, "riverine")
ggplot(mask, "dist_river")
ggplot(mask, "dist_villa")

ggplot(mask, "dist_villa") + stat_contour()


ggplot(mask, "dist_villa") + geom_mask(mask, "dist_villa", contour = TRUE)

geom_mask_contour(mask)

debugonce(ggplot.gmask)
debugonce(ggplot_base)
debugonce(geom_mask)


}


