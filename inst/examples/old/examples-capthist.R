\dontrun{

data(peafowl)
capthist = peafowl.capthist
mask = peafowl.mask

# summary ----------------------------------------------------------------------

summary(capthist)

# attributes -------------------------------------------------------------------

n_arrays(capthist)
n_traps(capthist)
n_occasions(capthist)
n_groups(capthist)
n_detections(capthist)

# plot -------------------------------------------------------------------------

debugonce(gibbonsecr:::plot.gmask)

plot(capthist)
points(capthist, sessions = 1:3, col = "red")
points(capthist, sessions = 4, col = 1:10)

plot(capthist, "riverine")
plot(capthist, "dist_river")

# ggplot -----------------------------------------------------------------------

library(ggplot2)
ggplot(capthist)

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


