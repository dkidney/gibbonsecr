## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## testthat package tests for gibbonsSECR functions

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##


# note that this file does not run tests on the gibbonsSECR GUI


# N.annamensis data --------------------------------------------------------

# context("N.annamensis data")
# 
# test_that("N.annamensis", {
#     
#     # check it exists    
#     data(N.annamensis)
#     expect_true( exists("N.annamensis") )
#     
#     # check attributes
#     expect_true( inherits(N.annamensis, "capthist") )
#     expect_false( is.null(traps(N.annamensis)) )
#     
# })


# checking functions ------------------------------------------------------

# context("checking functions")
# 
# test_that("check_model_options", {
#     
#     capthist = N.annamensis
#     mo = check_model_options(c(detectfn = 0, bearings = 0, distances = 0), N.annamensis)
#     
#     expect_equal( mo$detectfn, 0 )
#     expect_equal( mo$bearings, 0 )
#     expect_equal( mo$distances, 0 )
#     expect_error( check_model_options(c(0, 0, 0), N.annamensis) )
#     expect_error( check_model_options(list(detectfunc = 2), N.annamensis) )
#     expect_error( check_model_options(list(detectfn = 2), N.annamensis) )
#     expect_error( check_model_options(list(bearing = 0), N.annamensis) )
#     expect_error( check_model_options(list(bearings = 3), N.annamensis) )
#     expect_error( check_model_options(list(distance = 0), N.annamensis) )
#     expect_error( check_model_options(list(distances = 3), N.annamensis) )
#     
# })

# test_that("check_fixed", {
#     
#     mo = check_model_options(list(), N.annamensis)
#     
#     expect_equal( check_fixed(NULL, mo, N.annamensis), list(g0 = 1, pcall = 1) )
#     expect_equal( check_fixed(list(), mo, N.annamensis), list(g0 = 1, pcall = 1) )
#     expect_equal( check_fixed(c(g0 = 0.5), mo, N.annamensis)$g0, 1 )
#     expect_equal( check_fixed(c(pcall = 0.5), mo, N.annamensis)$pcall, 1 )
#     expect_null( check_fixed(c(z = 5), mo, N.annamensis)$z )
#     expect_null( check_fixed(c(distances = 0.5), mo, N.annamensis)$distances )
#     expect_error( check_fixed(c(sigma = -1), mo, N.annamensis) )
#     expect_error( check_fixed(c(G0 = 2), mo, N.annamensis) )
#     
# })

# test_that("check_model", {
#     
#     mo = check_model_options(list(), N.annamensis)
#     fixed = check_fixed(list(), mo, N.annamensis)
#     
#     expect_error( check_model(c(~ 1), mo, fixed) )
#     expect_error( check_model(c(d ~ 1), mo, fixed) )
#     expect_error( check_model(c(d = ~ 1), mo, fixed) )
#     expect_error( check_model(c("d ~ 1"), mo, fixed) )
#     expect_error( check_model(c(d = "~ 1"), mo, fixed) )
#     
# })

# rcpp functions ----------------------------------------------------------

# context("bearings and distances rcpp")
# 
# test_that("bearings and distances rcpp", {
#     
#     A = matrix(-1,1,2)
#     
#     Z = rbind(
#         c( 0        ,  1        ),
#         c( 1/sqrt(2),  1/sqrt(2)),
#         c( 1        ,  0        ),
#         c( 1/sqrt(2), -1/sqrt(2)),
#         c( 0        , -1        ),
#         c(-1/sqrt(2), -1/sqrt(2)),
#         c(-1        ,  0        ),
#         c(-1/sqrt(2),  1/sqrt(2))
#     )
#     
#     Z[,1] = Z[,1] + A[1,1]
#     Z[,2] = Z[,2] + A[1,2]
#     
#     # plot(Z, pch = 19, col = 4, asp = 1)
#     # points(A, pch = 19, col = 2)
#     
#     bearings = calc_bearings(A, Z)
#     expect_equal( bearings, matrix(pi / 4 * 0:7, 8, 1) )
#     
#     distances = calc_distances(A, Z)
#     expect_equal( distances, matrix(1,8,1) )
#     
# })


# helper functions --------------------------------------------------------

# context("mask area and size")
# 
# test_that("mask area and size", {
#     
#     # single occasion    
#     traps = make.grid(1, 3, spacing = 500)
#     mask = make.mask(traps, buffer = 1000, spacing = 100, type = "traprect")
#     
#     expect_equal( mask_area(mask), 0.01 )
#     expect_equal( mask_size(mask), 600 )
#     expect_equal( mask_Area(mask), 0.01 * 600 )
#     
#     # multi occasion
#     S = 2
#     traps = do.call("MS.traps", lapply(1:S, function(x) traps))
#     mask = do.call("MS.mask", lapply(1:S, function(x) mask))
#     
#     expect_true( ms(traps) )
#     expect_true( ms(mask) )
#     expect_equal( unname(mask_area(mask)), rep(0.01, S) )
#     expect_equal( unname(mask_size(mask)), rep(600, S) )
#     expect_equal( unname(mask_Area(mask)), rep(0.01 * 600, S) )
#     
# })


# fitting functions -------------------------------------------------------

# context("fitting functions")
# 
# suppressMessages({
#     
#     capthist      = check_capthist(N.annamensis)
#     model.options = check_model_options(list(), capthist)  
#     fixed         = check_fixed(list(), model.options, capthist)
#     model         = check_model(list(), model.options, fixed)
#     mask          = check_mask(NULL, capthist)
#     
#     R = n_arrays(capthist)
#     n = n_groups(capthist)
#     S = n_occasions(capthist) 
#     K = n_traps(capthist) 
#     
#     a = mask_area(mask)
#     M = mask_size(mask)
#     
#     usage = lapply(capthist, function(x) usage(traps(x)))
#     
#     smooth.setup = make_smooth_setup(model[["D"]], mask)
#     
#     design.matrices = make_design_matrices(model, capthist, mask, smooth.setup) 
#     
# })

# test_that("make_design_matrices", {
#     
#     expect_equal( length(design.matrices), R )
#     
#     expect_equivalent(names(design.matrices[[1]]) , c("D", "g0", "sigma", "bearings", "pcall") )
#     
#     dims = lapply(design.matrices[[1]], dim)
#     expect_equivalent( dims[["D"]]       , c(M[1],1) )
#     expect_equivalent( dims[["g0"]]      , c(K[1],1) )
#     expect_equivalent( dims[["sigma"]]   , c(K[1],1) )
#     expect_equivalent( dims[["bearings"]], c(K[1],1) )
#     expect_equivalent( dims[["pcall"]]   , c(S[1],1) )
#     
# })
# 
# par.labels = make_par_labels(design.matrices, fixed) 
# 
# test_that("make_par_labels", {
#     
#     expect_equivalent( par.labels[,"submodel"], c("D","sigma","bearings") )
#     expect_equivalent( par.labels[,"term"],     rep("(Intercept)", 3) )
#     expect_equivalent( par.labels[,"unique"],   paste(c("D","sigma","bearings"), "(Intercept)", sep = ".") )
#     
#     
# })
# 
# captures = get_captures(capthist)
# bearings = get_bearings(capthist)
# distances = get_distances(capthist)
# 
# data = lapply(session(capthist), function(i){
#     list(
#         capthist  = captures[[i]],
#         bearings  = bearings[[i]],
#         distances = distances[[i]]
#     )
# })
# 
# bearings = calc_bearings(traps(capthist), mask)
# distances = calc_distances(traps(capthist), mask)
# 
# mask.info = lapply(session(capthist), function(i){
#     list(
#         bearings  = bearings[[i]],
#         distances  = distances[[i]]
#     )
# })
# 
# 
# test_that("bearings and distances methods", {
#     
#     expect_equal( length(data), R )
# 
#     expect_equal( length(data[[1]]), 3 )
# 
#     expect_equal( names(data[[1]]), c("capthist", "bearings", "distances") )
#     
#     expect_equal( dim(data[[1]]$capthist)[1], unname(n[1]) )
#     expect_equal( dim(data[[1]]$capthist)[2], unname(S[1]) )
#     expect_equal( dim(data[[1]]$capthist)[3], unname(K[1]) )
#     
#     expect_equal( dim(data[[1]]$bearings)[1], unname(n[1]) )
#     expect_equal( dim(data[[1]]$bearings)[2], unname(S[1]) )
#     expect_equal( dim(data[[1]]$bearings)[3], unname(K[1]) )
# 
#     expect_equal( dim(data[[1]]$distances)[1], unname(n[1]) )
#     expect_equal( dim(data[[1]]$distances)[2], unname(S[1]) )
#     expect_equal( dim(data[[1]]$distances)[3], unname(K[1]) )
#     
#     expect_equal( length(data$bearings), R )
#     expect_equal( sapply(data$bearings, function(x) dim(x)[1]), n )
#     expect_equal( sapply(data$bearings, function(x) dim(x)[2]), S )
#     expect_equal( sapply(data$bearings, function(x) dim(x)[3]), K )
#     
#     expect_equal( length(data$distances), R )
#     expect_equal( sapply(data$distances, function(x) dim(x)[1]), n )
#     expect_equal( sapply(data$distances, function(x) dim(x)[2]), S )
#     expect_equal( sapply(data$distances, function(x) dim(x)[3]), K )
#     
#     expect_equal( sapply(mask.info$bearings, function(x) dim(x)[1]), M )
#     expect_equal( sapply(mask.info$distances, function(x) dim(x)[1]), M )
    
# })
# 
# start = check_start_values(NULL, par.labels, model.options, capthist, mask, fixed)
# 
# inv.link = get_inv_link(model, fixed, model.options)        
# 
# # detected = lapply(capthist, function(x) apply(x, c(1,2), function(x) 1 * any(x == 1))) 
# 
# detected = get_captures(capthist, summarise = "occasions")
# 
# submodel.arrays = make_submodel_arrays(start, par.labels, fixed, design.matrices, inv.link, n, S, K)
# 
# test_that("make_submodel_arrays", {
#     
#     expect_equal( names(submodel.arrays), session(capthist) )
#     expect_equal( length(submodel.arrays[[1]]), 5 )
#     expect_true( "D" %in% names(submodel.arrays[[1]]) )
#     expect_true( "g0" %in% names(submodel.arrays[[1]]) )
#     expect_true( "sigma" %in% names(submodel.arrays[[1]]) )
#     expect_true( "bearings" %in% names(submodel.arrays[[1]]) )
#     expect_true( "pcall" %in% names(submodel.arrays[[1]]) )
#     expect_equal( dim(submodel.arrays[[1]][["D"]]),        unname(c(M[1], 1)) )
#     expect_equal( dim(submodel.arrays[[1]][["g0"]]),       unname(c(S[1], K[1])) )
#     expect_equal( dim(submodel.arrays[[1]][["sigma"]]),    unname(c(S[1], K[1])) )
#     expect_equal( dim(submodel.arrays[[1]][["bearings"]]), unname(c(S[1], K[1])) )
#     expect_equal( dim(submodel.arrays[[1]][["pcall"]]),    unname(c(S[1], 1)) )
#     
# })


#     i=1
#     negloglik_rcpp(
#             
#             data               = data[[i]], 
#             mask               = mask.info[[i]], 
#             pars               = submodel.arrays[[i]], 
#             detectfn_code      = model.options[["detectfn"]],
#             bearings_pdf_code  = model.options[["bearings"]],
#             distances_pdf_code = model.options[["distances"]],
#             detected           = detected[[i]], 
#             usage              = usage[[i]], 
#             n                  = n[i], 
#             S                  = S[i], 
#             K                  = K[i], 
#             M                  = M[i],
#             a                  = a[i]
#             
#         )
#     
#     
#     sum(sapply(1:R, function(i){ # i=1
#         
#         negloglik_rcpp(
#             
#             data               = data[[i]], 
#             mask               = mask.info[[i]], 
#             pars               = submodel.arrays[[i]], 
#             detectfn_code      = model.options[["detectfn"]],
#             bearings_pdf_code  = model.options[["bearings"]],
#             distances_pdf_code = model.options[["distances"]],
#             detected           = detected[[i]], 
#             usage              = usage[[i]], 
#             n                  = n[i], 
#             S                  = S[i], 
#             K                  = K[i], 
#             M                  = M[i],
#             a                  = a[i]
#             
#         )
#         
#     }))
#     
# negloglik_wrapper(start, par.labels, fixed, design.matrices, inv.link, data, mask.info, model.options, usage, detected, R, n, S, K, M, a)
# 
# fit = gibbonsecr_fit(capthist)
# 
# 
# # methods functions -------------------------------------------------------
# 
# AIC(fit)
# 
# coef(fit)
# 
# confint(fit)
# 
# logLik(fit)
# 
# # plot(fit)
# 
# # predict(fit)
# 
# print(fit)
# 
# # simulate(fit)
# 
# summary(fit)
# 
# vcov(fit)
# 
# 
# # check against secr.fit --------------------------------------------------
# 
# context("fit_gibbonsecr vs secr.fit")
# 
# test_that("fit_gibbonsecr vs secr.fit", {
#     
#     data(N.annamensis)
# 
#     mask = secr::make.mask(secr::traps(N.annamensis), buffer = 5000, spacing = 250, type = "trapbuffer")
# 
#     fit0 = secr::secr.fit(N.annamensis, mask = mask, fixed = list(g0 = 1), trace = FALSE)
#     
#     fit1 = gibbonsecr_fit(N.annamensis, model.options = list(bearings = 0), mask = mask)
#     
#     fit0$proctime
#     fit1$run.time
#   
#     fit0.D = exp(coef(fit0)["D","beta"]) * 100
#     fit1.D = unname(exp(coef(fit1)[1]))
#     
#     fit0.sigma = exp(coef(fit0)["sigma","beta"])
#     fit1.sigma = unname(exp(coef(fit1)[2]))
# 
#     expect_equal( fit0.D, fit1.D, tolerance = .0001)
#     expect_equal( fit0.sigma, fit1.sigma, tolerance = .001)
# 
# })
# 



#  ------------------------------------------------------------------------

# context("example csv data files")
# 
# test_that("example csv data files", {
#     
#     detections = system.file("inst/extdata/example_detections_file.csv", package = "gibbonsSECR")
#     posts      = system.file("inst/extdata/example_posts_file.csv"     , package = "gibbonsSECR") 
#     covariates = system.file("inst/extdata/example_covariates_file.csv", package = "gibbonsSECR") 
#     
#     expect_true(detections != "" )
#     expect_true(posts      != "" )
#     expect_true(covariates != "" )
#     
#     capthist = import_data(detections, posts)
#     
#     expect_true( inherits(capthist, "capthist") )
#     expect_false( is.null(traps(capthist)) )
#     
#     mask = make.mask(traps(capthist), buffer = 3000, spacing = 100, type = "trapbuffer")
#     secr.fit.results = secr.fit(capthist, mask = mask, fixed = list(g0 = 1), trace = FALSE)
#     secr.fit.D = exp(coef(secr.fit.results)["D","beta"]) * 100
#     secr.fit.sigma = exp(coef(secr.fit.results)["sigma","beta"])
#     
#     expect_equal( secr.fit.D, 0.8254296, tolerance = .0001)
#     expect_equal( secr.fit.sigma, 756.6603352, tolerance = .001)
#     
# })







## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
