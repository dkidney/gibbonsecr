## Not run: 

data(N.annamensis)

fit = gibbonsecr_fit(N.annamensis)

AIC(fit)
coef(fit)
#confint(fit)
#plot(fit)
#predict(fit)
#print(fit)
vcov(fit)

## End(Not run)
