## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

calc_detprob = function(detectfn, g0, sigma, z, distances, usage, M, S, K){
    FUN = list(hn, hr)[[detectfn + 1]]
    detprob = array(0, dim = unname(c(M,S,K)), dimnames = list(
        dimnames(distances)[[1]], dimnames(usage)[[2]], dimnames(distances)[[2]]))
    for(k in 1:K){ # k=1
        for(s in 1:S){ # s=1
            if(usage[k,s] == 1){
                for(m in 1:M){ # m=1
                    detprob[m,s,k] = FUN(
                        distances[m,k],
                        theta = c(g0[s,k], sigma[s,k], z[s,k])
                    )
                }
            }
        }
    }
    return(detprob)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

calc_esa = function(detectfn, beta, parindx, fixed, design.matrices, distances, usage, inv.link, S, K, M, a){
    submodel.arrays = make_submodel_arrays(beta, parindx, fixed, design.matrices, inv.link, S, K)
    session.names = names(design.matrices)
    esa = sapply(session.names, function(session){ # session = session.names[1] ; session
        pdot = calc_pdot(
            detectfn  = detectfn,
            g0        = submodel.arrays[[session]][["g0"]],
            sigma     = submodel.arrays[[session]][["sigma"]],
            z         = submodel.arrays[[session]][["z"]],
            pcall     = submodel.arrays[[session]][["pcall"]],
            distances = distances[[session]],
            usage     = usage[[session]],
            M         = M[session],
            S         = S[session],
            K         = K[session]
        )
        esa = sum(pdot) * unname(a[session])
        return(esa)
    })
    return(esa)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

calc_pdot = function(detectfn, g0, sigma, z, pcall, distances, usage, M, S, K){
    one_minus_detprob = 1 - calc_detprob(detectfn, g0, sigma, z, distances, usage, M, S, K)
    # non-detection probabilities for each maskpoint-occasion combination
    prodK = apply(one_minus_detprob, c(1,2), prod)
    # non-detection probabilities for each maskpoint
    # see DK thesis (Sec 6.2, p104) for an explanation of the pcall bit
    prodSK = array(1, dim = M)
    for(s in 1:S){
        if(pcall[s] < 1){
            for(m in 1:M) prodSK[m] = prodSK[m] * { (1.0 - pcall[s]) + pcall[s] * prodK[m,s] }
        }else{
            for(m in 1:M) prodSK[m] = prodSK[m] * prodK[m,s]
        }
    }
    # detection probabilities for each maskpoint
    pdot = 1.0 - prodSK
    return(pdot)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# check to see if a formula contains gam smooth terms
# contains_smooth_terms = function(formula){
#     if(!inherits(formula, "formula")) stop("expecting a formula")
#     length(smooth_terms(formula)) > 0
# }
#
# # extract smooth terms from gam formula
# smooth_terms = function(formula){
#     if(!inherits(formula, "formula")) stop("expecting a formula")
#     terms = attr(terms(formula), "term.labels")
#     i = grepl("s\\(", terms) |
#         grepl("te\\(", terms) |
#         grepl("ti\\(", terms) |
#         grepl("t2\\(", terms)
#     return(terms[i])
# }
#
# # extract variables used in smooth terms in gam formula
# smooth_vars = function(formula){
#     if(!inherits(formula, "formula")) stop("expecting a formula")
#     if(contains_smooth_terms(formula)){
#         unique(do.call(c, lapply(smooth_terms(formula), function(x){
#             all.vars(as.formula(paste("~", x)))
#         })))
#     }else{
#         character(0)
#     }
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# gamma par is the shape parameter
# lnorm par is sigma

cv_to_pdfpar = function(cv = 0.3, which = c("vm","gamma","lnorm")){
    which = match.arg(which)
    switch(
        which,
        "gamma" = cv^(-2),
        "lnorm" = sqrt(log(1 + cv^2))
    )
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title Delta method
# @description Function to estimate the variance of nonlinear functions of normal variables
# @details Uses the \code{stats::numericDeriv} function.
# @param f Function of vector beta
# @param beta Parameter vector
# @param vcov Variance-covariance matrix for beta
# @param ... additional arguments to pass to \code{f}
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @examples
# \dontrun{
# # Example:
# hn = function(beta, r){
#    g0 <- invlogit(beta[1])
#    sigma <- exp(beta[2])
#    g0 * exp(-r^2/2/sigma^2)
# }
# beta = c(logit(0.5), log(1000))
# vcov = matrix(c(4.337749e-02,-1.869121e-03,-1.869121e-03,7.368978e-04), 2, 2) ; vcov
# r = seq(0, 3000, length = 25)
# delta = delta_method(f, beta, vcov, r = r)
# plot(r, delta$est, type = "l", xlim = c(0,3000), ylim = c(0,1))
# lines(r, delta$est + 1.96 * delta$se, lty = 2, col = 2, lwd = 2)
# lines(r, delta$est - 1.96 * delta$se, lty = 2, col = 2, lwd = 2)
# }
# @export

delta_method = function(f, beta, vcov, ...){

    ##################################################
    ## checks

    # check that f is a function of beta
    if(!"beta" %in% names(formals(f)))
        stop("f must be a function of vector 'beta'")
    # check that f doesn't have a betanames argument
    if("betanames" %in% names(formals(f)))
        stop("f can't be a function of 'betanames'")

    ##################################################
    ## arguments to new function g

    # force evaluate dots
    dots = list(...) # dots = list(r = 0:10)
    # expand beta vector to a series of individual betas
    beta.args = paste("beta", 1:length(beta), sep = "") # beta.args
    # add betanames and dots
    all.args = c(beta.args, "betanames") # all.args
    # add dots
    if(length(dots) > 0) all.args = c(all.args, names(dots)) # all.args
    # collapse
    all.args.collapsed = paste(all.args, collapse = ", ") # args

    ##################################################
    ## evaluate all arguments in the current environment

    thisenv = environment()
    betanames = names(beta)
    for(i in 1:length(beta)){
        eval(parse(text = paste0(beta.args[i]," = beta[",i,"]")))
    }
    if(!all(sapply(beta.args, exists, envir = thisenv)))
        stop("beta objects don't exist")
    if(length(dots) > 0){
        for(i in 1:length(dots))
            eval(parse(text = paste0(names(dots)[i], " = dots[[i]]")))
        if(!all(sapply(names(dots), exists, envir = thisenv)))
            stop("... objects don't exist")
    }

    ##################################################
    ## body of new function g

    # extract body from function f
    body = paste(as.character(body(f)), collapse = "\n") # cat(body)

    # remove leading/trailing braces/returns
    for(i in c("^\\{","^\n","\\}$","\n$"))
        body = gsub(i, "", body) # cat(body)
    # add new line reconstructing beta vector
    newline = paste0("beta = setNames(c(", paste(beta.args, collapse = ", "), "), betanames)\n") # cat(newline)
    body = paste0(newline, body) # cat(body)

    ##################################################
    ## make new function g

    g = eval(parse(text = paste0("function(", all.args.collapsed, "){\n", body, "\n}")))  # g

    ##################################################
    ## make a call expression for new function g

    calltext = paste0("g(", all.args.collapsed,")") # calltext
    expr = quote(eval(parse(text = calltext))) # expr

    ##################################################
    ## estimates and estimated variances

    # est = eval(expr)
    est  = stats::numericDeriv(expr = expr, theta = beta.args, rho = thisenv)
    grad = attr(est, "gradient") # grad
    var  = sapply(1:nrow(grad), function(i) t(grad[i, ]) %*% vcov %*% grad[i, ])
    est  = as.numeric(est)

    # return list
    list(estimate = est, se = sqrt(var))
}



## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# @title Delta method for linear models
# @description Function to estimate confidence intervals for functions of estimated parameters
# @details This is a wrapper function for \code{delta_method}
# @param X Design matrix
# @param beta Parameter vector
# @param vcov Variance-covariance matrix for beta
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @examples
# \dontrun{
# # Examples:
# n = 100
# x = seq(0, 1, length = n)
# y = x^2 + rnorm(n, 0, 0.1)
# plot(x, y)
# fit = lm(y ~ I(x^2))
#
# # conventional approach
# preds = predict(fit, se = TRUE)
# lines(x, preds$fit, col = 4, lwd = 2)
# lines(x, preds$fit + qnorm(0.05/2) * preds$se, lty = 2, col = 4, lwd = 2)
# lines(x, preds$fit + qnorm(1-0.05/2) * preds$se, lty = 2, col = 4, lwd = 2)
#
# # delta method
# X = model.matrix(fit)
# beta = coef(fit)
# vcov = vcov(fit)
# intervals = delta_method_Xbeta(X, beta, vcov)
# lines(x, intervals$est, col = 2, lwd = 2)
# lines(x, intervals$lower, lty = 2, col = 2, lwd = 2)
# lines(x, intervals$upper, lty = 2, col = 2, lwd = 2)
# }
# @export

delta_method_Xbeta = function(X, beta, vcov){
    if(length(dim(X)) != 2)
        stop("X must be a matrix")
    if(length(beta) != ncol(X))
        stop("length(beta) must equal ncol(X)")
    if(!all(dim(vcov) == rep(ncol(X), 2)))
        stop("nrow(vcov) and ncol(vcov) must equal ncol(X)")
    npars = ncol(X) # npars
    f.body = paste0("as.numeric(X %*% c(", paste("beta[", 1:npars, "]", sep = "", collapse = ", "), "))") # f.body
    f.text = paste0("function(beta){", f.body, "}") # f.text
    f = eval(parse(text = f.text)) # f
    delta_method(f, beta, vcov, X = X)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# uses the lognormal method to get estimates and intervals for density

# density_intervals = function(EX, VarX, alpha = 0.05){
#     # E[exp(X)]
#     est = exp(EX + VarX/2)
#     # sqrt(Var[exp(X)])
#     se  = sqrt((exp(VarX) - 1) * exp(2 * EX + VarX))
#     out = cbind(
#         est   = est,
#         lower = est + qnorm(alpha / 2) * se,
#         upper = est - qnorm(alpha / 2) * se
#     )
#     return(out)
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

dgamma = function(x, shape, EX){
    stats::dgamma(x = x, shape = shape, scale = EX / shape)
}

dlnorm = function(x, sigma, EX){
    stats::dlnorm(x = x, meanlog = log(EX) - sigma^2 / 2, sdlog = sigma)
}

dvm = function(x, kappa, EX){
    CircStats::dvm(theta = x, mu = EX, kappa = kappa)
}

dwrpcauchy = function(x, rho, EX){
    CircStats::dwrpcauchy(theta = x, mu = EX, rho = rho)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# extract the design matrix component from the smooth.setup object
# need to identify which rows of X correspond to which session

get_design_matrices = function(smooth.setup, model.frames){
    # count the number of rows per session for each submodel model frame
    rows = sapply(names(smooth.setup), function(submodel){
        # get number of rows per model frame
        nrows = sapply(model.frames, function(x){
            nrow(x[[submodel]])
        })
        # convert to row numbers in smooth.setup X
        rows = sapply(nrows, function(i) 1:i, simplify = FALSE)
        for(i in 2:length(rows)) rows[[i]] = rows[[i]] + max(rows[[i-1]])
        return(rows)
    }, simplify = FALSE)
    # extract the rows of X's from the submodel smooth.setup object
    # corresponding to each session
    sapply(names(model.frames), function(session){
        sapply(names(smooth.setup), function(submodel){
            i = rows[[submodel]][[session]]
            X = smooth.setup[[submodel]][["X"]][i, , drop = FALSE]
            colnames(X) = smooth.setup[[submodel]][["term.names"]]
            return(X)
        }, simplify = FALSE)
    }, simplify = FALSE)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

get_inv_link = function(model, fixed, model.options){
    inv.link = list(
        "D"         = exp,
        "g0"        = invlogit,
        "sigma"     = exp,
        "z"         = exp,
        "pcall"     = invlogit,
        "bearings"  = if(model.options$bearings == 1) exp else invlogit,
        #"distances" = if(model.options$distances == 1) exp else invlogit
        "distances" = exp
    )
    for(i in names(inv.link)){
        if(!i %in% names(model))
            inv.link[[i]] = NULL
    }
    # fixed pars have identity link
    if(length(fixed) > 0){
        for(par in names(fixed)){
            inv.link[[par]] = function(x) x
        }
    }
    return(inv.link)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# gibbonsecr_fit_old = function(capthist, model = list(), mask = NULL, fixed = list(), model.options = list(), mask.options = list(), fitting.options = list(), start = NULL, trace = FALSE, debug = TRUE){
#
#     start.time = Sys.time()
#
#     ##################################################
#     ## check inputs
#
#     # original.capthist = capthist
#     capthist      = check_capthist(capthist)
#     mask          = check_mask(mask, capthist, mask.options)
#     model.options = check_model_options(model.options, capthist)
#     fixed         = check_fixed(fixed, model.options, capthist)
#     model         = check_model(model, fixed, model.options, capthist, mask)
#
#     ##################################################
#     ## capthist and mask stats
#
#     R = n_arrays(capthist)
#     n = n_groups(capthist)
#     S = n_occasions(capthist)
#     K = n_traps(capthist)
#     a = mask_area(mask)
#     M = mask_npoints(mask)
#
#     ##################################################
#     ## fitting function arguments
#
#     model.frames = make_model_frames(
#         model         = model,
#         traps         = traps(capthist),
#         mask          = mask,
#         n_occasions   = n_occasions(capthist),
#         sessioncov    = sessioncov(capthist),
#         timecov       = timecov(capthist),
#         debug         = debug
#     )
#
#     ##################################################
#     ## in development: use gam to get model frame, model matrix and smooth setup
#
#     if(0){
#         make_G = function(formula, data){
#             # if no response variable then add dummy response to lhs of formula
#             if(attr(terms(formula), "response") == 0){
#                 i = "dummy"
#                 continue = TRUE
#                 while(continue){
#                     if(!i %in% colnames(data)) continue = FALSE
#                     i = paste0(i, "_1")
#                 }
#                 eval(parse(text = paste0("formula = update.formula(formula,", i, " ~ .)")))
#             }
#             # add response variable to data
#             data[[all.vars(formula)[1]]] = rep(1, nrow(data))
#             mgcv::gam(formula, data = data, fit = FALSE)
#         }
#
#         # G = lapply(names(model.frames), function(session){ # session = names(model.frames)[1] ; session
#             # lapply(names(model), function(submodel){ # submodel = names(model)[1] ; submodel
#         for(session in names(model.frames)){ # session = names(model.frames)[1] ; session
#             for(submodel in names(model)){ # session = names(model.frames)[1] ; session
#                 make_G(formula = model[[submodel]],
#                        data    = model.frames[[session]][[submodel]])
#             }
#         }
#             # })
#         # })
#
#
#
#     }
#
#     # traps = traps(capthist) ; n_occasions = n_occasions(capthist) ; sessioncov = sessioncov(capthist) ; timecov = timecov(capthist) ; sessions = NULL ; submodels = NULL
#
#     smooth.setup = make_smooth_setup(model, model.frames)
#     design.matrices = make_design_matrices(model, model.frames, smooth.setup, debug = debug)
#    # model.frames = model.frames ; smooth.setup = NULL ; sessions = NULL ; submodels = NULL
#
#     par.labels = make_par_labels(design.matrices, fixed)
#     parindx    = make_parindx(par.labels[,"unique"])
#     inv.link   = get_inv_link(model, fixed, model.options)
#     detected   = get_captures(capthist, summarise = "occasions")
#     data       = prepare_data(capthist, model.options)
#     mask.info  = prepare_mask_info(mask, capthist, model.options)
#     usage      = usage(traps(capthist))
#     start      = check_start_values(start, capthist, mask, model.options, fixed, S, K, M, a, usage, design.matrices, par.labels, parindx, inv.link, mask.info)
#     # use default fitting options if not supplied
#     default.fitting.options = list(hessian = TRUE, iterlim = 1000, LLonly = FALSE)
#     fitting.options = replace(default.fitting.options, names(fitting.options), fitting.options)
#
#     ##################################################
#     # choose fitting function
#     nll.function = negloglik_wrapper
#     nll.args = list(
#         beta            = start,
#         data            = data,
#         mask.info       = mask.info,
#         design.matrices = design.matrices,
#         parindx         = parindx,
#         fixed           = fixed,
#         inv.link        = inv.link,
#         model.options   = model.options,
#         usage           = usage,
#         detected        = detected,
#         R               = R,
#         n               = n,
#         S               = S,
#         K               = K,
#         M               = M,
#         a               = a,
#         trace           = trace
#     )
#
#     # debugging
#     if(0){
#         for(i in names(nll.args))
#             eval(parse(text = paste(i, "= nll.args[[i]]")), envir = globalenv())
#     }
#
#     ##################################################
#     ## LLonly
#     if(fitting.options$LLonly){
#         nll = try(do.call(nll.function, nll.args))
#         if(inherits(nll,"try-error")){
#             stop("Unable to evaluate loglikelihood using start values")
#         }else{
#             return(-nll)
#         }
#     }
#
#     ##################################################
#     ## fit model using nlm
#     nll.args$beta = NULL
#     nll.args = c(nll.args, list(
#         f               = nll.function,
#         p               = start,
#         hessian         = fitting.options$hessian,
#         iterlim         = fitting.options$iterlim
#     ))
#     nlm.results = try(do.call(nlm, nll.args))
#
# #     nlm.args = list(
# #         f               = nll.function,
# #         p               = start,
# #         data            = data,
# #         mask.info       = mask.info,
# #         design.matrices = design.matrices,
# #         par.labels      = par.labels,
# #         fixed           = fixed,
# #         inv.link        = inv.link,
# #         model.options   = model.options,
# #         usage           = usage,
# #         detected        = detected,
# #         R               = R,
# #         n               = n,
# #         S               = S,
# #         K               = K,
# #         M               = M,
# #         a               = a,
# #         trace           = trace,
# #         hessian         = fitting.options$hessian,
# #         iterlim         = fitting.options$iterlim)
#
#     # #     negloglik = if(locations){
#     # #         negloglik_locations_known_wrapper
#     # #     }else{
#     # #         negloglik_locations_unknown_wrapper
#     # #     }
#
#     #     #     if(!locations){
#     #     #         nlm.args$detected = detected
#     #     #         nlm.args$S = S
#     #     #     }
#     #
#
#     # fit model
#     # nlm.results = try(do.call(nlm, nlm.args))
#
#     ##################################################
#     ## process nlm results
#
#     if(nlm.results$code > 2) warning("nlm code = ", nlm.results$code)
#     if(nlm.results$iterations <= 1)  warning("nlm iterations = ", nlm.results$iterations)
#     names(nlm.results$estimate) = par.labels[,"unique"]
#     if(!is.null(nlm.results$hessian))
#         rownames(nlm.results$hessian) = colnames(nlm.results$hessian) = par.labels[,"unique"]
#
#     ##################################################
#     ## return gibbonsecr_fit object
#
#     fit = list(
#         capthist        = capthist,
#         # capthist        = original.capthist,
#         mask            = mask,
#         model           = model,
#         model.options   = model.options,
#         fixed           = fixed,
#         model.frames    = model.frames,
#         design.matrices = design.matrices,
#         inv.link        = inv.link,
#         start           = start,
#         smooth.setup    = smooth.setup,
#         mask.info       = mask.info,
#         # locations       = locations,
#         nlm             = nlm.results,
#         parindx         = parindx,
#         par.labels      = par.labels,
#         run.time        = difftime(Sys.time(), start.time)
#     )
#     class(fit) = c("gibbonsecr_fit", class(fit))
#
#     return(fit)
#
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Fit an SECR model to acoustic gibbon survey data
#' @description TODO
#' @details TODO
#' @param capthist a \code{\link{capthist}} object
#' @param model TODO
#' @param mask TODO
#' @param fixed TODO
#' @param model.options a list of control options determining the type of model to be fitted (see Details).
#' @param mask.options TODO
#' @param fitting.options TODO
#' @param start TODO
#' @param trace TODO
#' @param debug TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example_fit.r
#' @export

gibbonsecr_fit = function(capthist, model = list(), mask = NULL, fixed = list(), model.options = list(), mask.options = list(), fitting.options = list(), start = NULL, trace = FALSE, debug = TRUE){

    start.time = Sys.time()

    ##################################################
    ## check inputs

    # original.capthist = capthist
    capthist      = check_capthist(capthist)
    mask          = check_mask(mask, capthist, mask.options)
    model.options = check_model_options(model.options, capthist)
    fixed         = check_fixed(fixed, model.options, capthist)
    model         = check_model(model, fixed, model.options, capthist, mask)

    ##################################################
    ## capthist and mask stats

    R = n_arrays(capthist)
    n = n_groups(capthist)
    S = n_occasions(capthist)
    K = n_traps(capthist)
    a = mask_area(mask)
    M = mask_npoints(mask)

    ##################################################
    ## fitting function arguments

    model.frames = make_model_frames(
        model         = model,
        traps         = traps(capthist),
        mask          = mask,
        n_occasions   = n_occasions(capthist),
        sessioncov    = sessioncov(capthist),
        timecov       = timecov(capthist),
        debug         = debug
    )

    if(0){
        names(model.frames)
        names(model.frames[[1]])
        head(model.frames[[1]][["D"]]) ; dim(model.frames[[1]][["D"]])
        head(model.frames[[1]][["g0"]]) ; dim(model.frames[[1]][["g0"]])
        head(model.frames[[1]][["sigma"]]) ; dim(model.frames[[1]][["sigma"]])
        head(model.frames[[1]][["bearings"]]) ; dim(model.frames[[1]][["bearings"]])
        head(model.frames[[1]][["distances"]]) ; dim(model.frames[[1]][["distances"]])
        head(model.frames[[1]][["pcall"]]) ; dim(model.frames[[1]][["pcall"]])
    }

    ##################################################
    ## in development: use gam to get model frame, model matrix and smooth setup

    smooth.setup = make_smooth_setup(model, model.frames)

    if(0){
        names(smooth.setup)
        names(smooth.setup[[1]])
    }

    design.matrices = get_design_matrices(smooth.setup, model.frames)

    if(0){
        names(design.matrices)
        names(design.matrices[[1]])
        head(design.matrices[[1]][["D"]]) ; dim(design.matrices[[1]][["D"]])
        head(design.matrices[[1]][["g0"]]) ; dim(design.matrices[[1]][["g0"]])
        head(design.matrices[[1]][["sigma"]]) ; dim(design.matrices[[1]][["sigma"]])
        head(design.matrices[[1]][["bearings"]]) ; dim(design.matrices[[1]][["bearings"]])
        head(design.matrices[[1]][["distances"]]) ; dim(design.matrices[[1]][["distances"]])
        head(design.matrices[[1]][["pcall"]]) ; dim(design.matrices[[1]][["pcall"]])
    }

    par.labels = make_par_labels(design.matrices, fixed)
    parindx    = make_parindx(par.labels[,"unique"])
    inv.link   = get_inv_link(model, fixed, model.options)
    detected   = get_captures(capthist, summarise = "occasions")
    data       = prepare_data(capthist, model.options)
    mask.info  = prepare_mask_info(mask, capthist, model.options)
    usage      = usage(traps(capthist))
    start      = check_start_values(start, capthist, mask, model.options, fixed, S, K, M, a, usage, design.matrices, par.labels, parindx, inv.link, mask.info)
    # use default fitting options if not supplied
    default.fitting.options = list(hessian = TRUE, iterlim = 1000, LLonly = FALSE)
    fitting.options = replace(default.fitting.options, names(fitting.options), fitting.options)

    ##################################################
    ## fitting function

    nll.function = negloglik_wrapper
    nll.args = list(
        beta            = start,
        data            = data,
        mask.info       = mask.info,
        design.matrices = design.matrices,
        parindx         = parindx,
        fixed           = fixed,
        inv.link        = inv.link,
        model.options   = model.options,
        usage           = usage,
        detected        = detected,
        R               = R,
        n               = n,
        S               = S,
        K               = K,
        M               = M,
        a               = a,
        trace           = trace
    )

    # debugging
    if(0){
        for(i in names(nll.args))
            eval(parse(text = paste(i, "= nll.args[[i]]")), envir = globalenv())
    }

    ##################################################
    ## LLonly

    if(fitting.options$LLonly){
        nll = try(do.call(nll.function, nll.args))
        if(inherits(nll,"try-error")){
            stop("Unable to evaluate loglikelihood using start values")
        }else{
            return(-nll)
        }
    }

    ##################################################
    ## fit model using nlm

    nll.args$beta = NULL
    nll.args = c(nll.args, list(
        f               = nll.function,
        p               = start,
        hessian         = fitting.options$hessian,
        iterlim         = fitting.options$iterlim
    ))
    nlm.results = try(do.call(nlm, nll.args))

    # #     negloglik = if(locations){
    # #         negloglik_locations_known_wrapper
    # #     }else{
    # #         negloglik_locations_unknown_wrapper
    # #     }

    #     #     if(!locations){
    #     #         nlm.args$detected = detected
    #     #         nlm.args$S = S
    #     #     }
    #

    ##################################################
    ## process nlm results

    if(nlm.results$code > 2) warning("nlm code = ", nlm.results$code)
    if(nlm.results$iterations <= 1)  warning("nlm iterations = ", nlm.results$iterations)
    names(nlm.results$estimate) = par.labels[,"unique"]
    if(!is.null(nlm.results$hessian))
        rownames(nlm.results$hessian) = colnames(nlm.results$hessian) = par.labels[,"unique"]

    ##################################################
    ## return gibbonsecr_fit object

    fit = list(
        capthist        = capthist,
        mask            = mask,
        model           = model,
        model.options   = model.options,
        fixed           = fixed,
        model.frames    = model.frames,
        design.matrices = design.matrices,
        inv.link        = inv.link,
        start           = start,
        smooth.setup    = smooth.setup,
        mask.info       = mask.info,
        # locations       = locations,
        nlm             = nlm.results,
        parindx         = parindx,
        par.labels      = par.labels,
        run.time        = difftime(Sys.time(), start.time)
    )
    class(fit) = c("gibbonsecr_fit", class(fit))

    return(fit)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

gibbonsecr_sim = function(fit, nsims = 99, ncores = NULL, buffer = 6000, spacing = 10, use.secr.fit = FALSE, simname = "testsim", beta = NULL){

    start = Sys.time()

    ##################################################
    ## check inputs

    if(!inherits(fit, "gibbonsecr_fit"))
        stop("requires a gibbonsecr_fit object", call. = FALSE)
    if(is.null(beta)){
        beta = coef(fit)
    }else{
        if(length(beta) != length(coef(fit)))
            stop("length(beta) must equal length(coef(fit)", call. = FALSE)
        if(is.null(names(beta))){
            names(beta) = names(coef(fit))
        }else{
            if(!all(names(beta) == names(coef(fit))))
                stop("names(beta) should be the same as names(coef(fit)", call. = FALSE)
        }
    }
    ncores = if(is.null(ncores)){
        parallel::detectCores() - 1
    }else if(ncores > parallel::detectCores()){
        parallel::detectCores()
    }else{
        ncores
    }

    ##################################################
    ## temp directories for saving fitted objects

    dir = file.path(tempdir(), simname)
    dir.create(dir, FALSE)
    unlink(list.files(dir, full.names = TRUE), TRUE, TRUE)
    dir.create(file.path(dir, "gibbonsecr_fit"), FALSE)
    if(use.secr.fit)
        dir.create(file.path(dir, "secr.fit"), FALSE)

    ##################################################
    ## arguments for secr.fit

    if(use.secr.fit){
        args = list(
            model = fit$model,
            fixed = fit$fixed,
            start = beta
        )
        for(submodel in c("pcall","bearings","distances")){
            args$model = args$model[!names(args$model) == submodel]
            args$fixed = args$fixed[!names(args$fixed) == submodel]
            args$start = args$start[!grepl(submodel, names(args$start))]
        }
    }

    ##################################################
    ## set up cluster and print progress to screen

    cat("\n")
    message("Simulation: ", simname)
    message("- using ", ncores, " cores (", parallel::detectCores(), " available)")
    message("- recording sim progress in: '", normalizePath(file.path(dir,"gibbonsecr_fit")), "'")
    message("- setting up cluster...")
    cl = parallel::makeCluster(ncores)
    message("- loading packages...")
    invisible(parallel::clusterEvalQ(cl, library(gibbonsecr)))
    message("- exporting objects...")
    parallel::clusterExport(cl, ls(), envir = environment())
    message("- running simulation...")

    ##################################################
    ## simulate capthist data and fit models

    seed = do.call(c, parallel::parLapply(cl, 1:nsims, function(sim){ # sim = 1

        converged = FALSE

        while(!converged){

            ##################################################
            ## simulate capthist

            seed = round(runif(1, 0, 10e6))
            simdata = try(suppressMessages({
                simulate(
                    object  = fit,
                    buffer  = buffer,
                    spacing = spacing,
                    beta    = beta,
                    seed    = seed,
                    debug   = FALSE
                )
            }), TRUE)
            # summary_capthist(simdata) ; usage(traps(simdata))
            if(inherits(simdata, "try-error") || is.null(simdata)) next

            ##################################################
            ## fit with secr.fit

            if(use.secr.fit){
                fit0 = try(suppressMessages({
                    secr::secr.fit(
                        capthist   = simdata,
                        mask       = fit$mask,
                        model      = args$model,
                        fixed      = args$fixed,
                        start      = args$start,
                        timecov    = attr(fit$capthist, "timecov"),
                        sessioncov = attr(fit$capthist, "sessioncov"),
                        verify     = FALSE,
                        trace      = FALSE
                    )
                }), TRUE)
                if(inherits(fit0, "try-error") ||
                   fit0$fit$code > 2 ||
                   fit0$fit$iterations <= 1) next
                save(fit0, file = file.path(dir, "secr.fit", paste0("sim_", sim, ".rda", sep = "")))
            }

            ##################################################
            ## fit with gibbonsecr_fit

            fit1 = try(suppressMessages({
                gibbonsecr_fit(
                    capthist      = simdata,
                    mask          = fit$mask,
                    model         = fit$model,
                    fixed         = fit$fixed,
                    start         = beta,
                    model.options = fit$model.options,
                    trace         = FALSE
                )
            }), TRUE)
            if(inherits(fit1, "try-error") ||
               fit1$nlm$code > 2 ||
               fit1$nlm$iterations <= 1) next
            save(fit1, file = file.path(dir, "gibbonsecr_fit", paste0("sim_", sim, ".rda", sep = "")))

            ##################################################
            ## update converged

            converged = TRUE

        }

        return(seed)

    }))

    ##################################################
    ## get coefs from fitted models

    message("- preparing results...")
    res1 = do.call(rbind, parallel::parLapply(cl, 1:nsims, function(sim){ # sim=1
            fit1 = NULL
        load(file.path(dir, "gibbonsecr_fit", paste0("sim_", sim, ".rda", sep = "")))
        coef(fit1)
    }))
    if(use.secr.fit){
        res0 = do.call(rbind, parallel::parLapply(cl, 1:nsims, function(sim){ # sim=1
            fit0 = NULL
            load(file.path(dir, "secr.fit", paste0("sim_", sim, ".rda", sep = "")))
            # setNames(coef(fit0)[,"beta"] + c(log(100), rep(0, nrow(coef(fit0)) - 1)), rownames(coef(fit0)))
            setNames(coef(fit0)[,"beta"], rownames(coef(fit0)))
        }))
    }

    ##################################################
    ## process and return results

    attr(res1, "truth") = coef(fit)
    rownames(res1) = 1:nrow(res1)
    # colnames(res1) = gsub("\\.\\(Intercept\\)", "",  colnames(res1))
    results = list(gibbonsecr_fit = res1)
    if(use.secr.fit) results$secr.fit = res0
    attr(results, "truth") = beta
    # attr(results, "truth") = setNames(beta, gsub("\\.\\(Intercept\\)", "", names(beta)))
    attr(results, "seed") = seed
    attr(results, "simname") = simname
    class(results) = c("gibbonsecr_sim", class(results))
    message("- closing cluster...")

    parallel::stopCluster(cl)

    run.time = difftime(Sys.time(), start)
    message("Time taken: ", round(run.time, 1), " ", attr(run.time, "units"))

    return(results)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# detection functions

# half normal
hn = function(x, theta, ...){
    theta[1] * exp(-0.5 * x * x / theta[2] / theta[2])
}

# hazard rate
hr = function(x, theta, ...){
    theta[1] * ( 1 - exp(-(x / theta[2])^(-theta[3])) )
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Import survey data from csv files
#' @description TODO
#' @details Details - file paths will have been obtained via the browse button
#'   in the GUI
#' @param detections file containing the recapture data and the estiamated
#'   bearings and distances
#' @param posts      file containing the locations of the posts
#' @param covariates optional file containing the covariate data
#' @param details    optional argument determining the type and units of the
#'   estiamted bearings and distances
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' library(sp)
#' library(secr)
#' wd = setwd(system.file("extdata/N.annamensis", package = "gibbonsecr"))
#' pop = par(no.readonly = TRUE)
#'
#' capthist = import_data(
#'     detections = "detections.csv",
#'     posts      = "posts.csv",
#'     covariates = "covariates.csv"
#' )
#' class(capthist)
#'
#' region = import_shp("region")
#'
#' par(mfrow = c(1,1), mar = c(0,0,0,0))
#' plot(region)
#' plot(traps(capthist), add = TRUE)
#'
#' setwd(wd)
#' par(pop)
#' @export

import_data = function(detections, posts, covariates = NULL, details = list()){

    # detections = system.file("extdata/example_detections_file.csv", package = "gibbonsecr")
    # posts      = system.file("extdata/example_posts_file.csv"     , package = "gibbonsecr")

    ##################################################
    ## detections and posts data

    # read detections and posts data csv files
    if(!is.data.frame(detections))
        detections = read.csv(detections, stringsAsFactors = FALSE)
    # read posts and posts data csv files
    if(!is.data.frame(posts))
        posts = read.csv(posts, stringsAsFactors = FALSE)
    # convert to tbl class
    detections = dplyr::tbl_df(detections)
    posts      = dplyr::tbl_df(posts)
    # run checks on detections and posts data
    details    = check_details(details)
    detections = check_detections(detections, details)
    posts      = check_posts(posts, detections, details)

    ##################################################
    ## capthist

    # construct a multi-session capthist object
    array.names = unique(posts$array)
    capthist = sapply(array.names, function(array){ # array = array.names[2] ; array

        # print(array)
        ##================================================
        ## traps

        i = posts$array == array
        traps = with(posts[i,], data.frame(x = x, y = y, row.names = post))
        traps = read.traps(data = traps, detector = "proximity")
        usage(traps) = do.call(rbind, lapply(strsplit(posts$usage[i], NULL), as.numeric))

        ##================================================
        ## captures

        i = detections$array == array
        captures = with(detections[i,], {
            data.frame(
                session  = array,
                ID       = group,
                occasion = occasion,
                detector = post
            )
        })

        ##================================================
        ## capthist
        if(nrow(captures) == 0){
            capthist = array(dim = c(0,ncol(usage(traps)),nrow(traps)),
                             dimnames = list(NULL, as.character(1:ncol(usage(traps))), rownames(traps)))
            class(capthist) = "capthist"
            session(capthist) = array
            traps(capthist) = traps
            covariates(capthist) = data.frame()
        }else{
            capthist = make.capthist(captures, traps, noccasions = ncol(usage(traps)))
            dimnames(capthist)[[3]] = rownames(traps)
        }
        # check for errors
        # verify(capthist)

        return(capthist)
    }, simplify = FALSE)
    # convert to multi-session by default
    capthist = MS(capthist, session.names = array.names)

    ##################################################
    ## bearings and distances

    add.bearings = "bearing" %in% colnames(detections)
    add.distances = "distance" %in% colnames(detections)
    if(add.bearings || add.distances){
        for(session in session(capthist)){ # session = session(capthist)[1] ; session
            dets = detections[detections$array == session,]
            capt = capthist[[session]]
            group.ids  = dimnames(capt)[[1]]
            noccasions = dim(capt)[2]
            post.ids   = dimnames(capt)[[3]]
            if(add.bearings){
                bearings = array(NA, dim(capt), dimnames(capt))
            }
            if(add.distances){
                distances = array(NA, dim(capt), dimnames(capt))
            }
            if(nrow(dets) > 0){
                for(det in 1:nrow(dets)){
                    i = which(group.ids    == dets$group[det])
                    s = which(1:noccasions == dets$occasion[det])
                    k = which(post.ids     == dets$post[det])
                    if(add.bearings)  bearings[i,s,k]  = dets$bearing[det]
                    if(add.distances) distances[i,s,k] = dets$distance[det]
                }
            }
            if(add.bearings){
                attr(bearings, "details") = details$bearings
                attr(capthist[[session]], "bearings") = bearings
            }
            if(add.distances){
                attr(distances, "details") = details$distances
                attr(capthist[[session]], "distances") = distances
            }
        }
    }

    ##################################################
    ## covariates

    # covariates = system.file("extdata/example_covariates_file.csv", package = "gibbonsecr")

    if(is.null(covariates)){
        # make a set of null covariates
        covariates = do.call(rbind, lapply(session(capthist), function(session){
            expand.grid(
                array = session,
                post = rownames(traps(capthist[[session]])),
                occasion = 1:n_occasions(capthist[[session]]),
                stringsAsFactors = FALSE
            )
        }))
    }else{
        # read csv file
        if(!is.data.frame(covariates)){
            covariates = read.csv(covariates, stringsAsFactors = FALSE)
        }
    }
    # convert to tbl class
    covariates = dplyr::tbl_df(covariates)
    # run checks
    covariates = check_covariates(covariates, capthist) # head(covariates)
    # add covariates to capthist
    for(level in c("sessioncov", "timecov", "trapcov", "timevaryingcov")){ # level = "sessioncov"
        # make an id variable to group covariate data
        ids = with(covariates, switch(level,
                                      "sessioncov"     = paste(array),
                                      "timecov"        = paste(array, occasion, sep = "_"),
                                      "trapcov"        = paste(array, post, sep = "_"),
                                      "timevaryingcov" = paste(array, post, occasion, sep = "_")
        ))
        # determine which covariates are constant within each id
        covnames = apply(do.call(rbind, sapply(unique(ids), function(id){ # id = session_id[1] ; id
            apply(covariates[ids == id, , drop = FALSE], 2, function(x){
                all(x == x[1])
            })
        }, simplify = FALSE)), 2, all)
        covnames = colnames(covariates)[covnames]
        # extract the relvant columns from the covariates
        tempcov = covariates[!duplicated(ids), covnames, drop = FALSE]
        temparray = tempcov$array

        ##================================================
        ## groupcov

        # not currently implemented
        for(session in session(capthist)){
            covariates(capthist[[session]]) = NULL
        }

        ##================================================
        ## sessioncov

        # save as a single dataframe (n_rows = n_sessions)
        # append as an attribute
        # note that this is is different to how Murray deals with sessioncov in the secr package:
        # secr::secr.fit allows the inclusion of sessioncov and timecov via function arguments
        if(level == "sessioncov"){
            # dont use occasion
            tempcov = tempcov[, colnames(tempcov) != "occasion", drop = FALSE]
            rownames(tempcov) = session(capthist)
            # attr(capthist, "sessioncov") = tempcov
            sessioncov(capthist) = tempcov
        }else{
            # for all remaining levels, don't use sessioncov
            tempcov = tempcov[, !colnames(tempcov) %in% colnames(attr(capthist, "sessioncov")), drop = FALSE]
        }

        ##================================================
        ## timecov

        # a list of dataframes (n_elements = n_sessions)
        if(level == "timecov"){
            # if single-occasion then there can be no timecovs
            if(all(n_occasions(capthist) == 1)) next
            # dont use post
            tempcov = tempcov[, colnames(tempcov) != "post", drop = FALSE]
            if(ncol(tempcov) == 0) next
            # add list of timecovs as an attribute to capthist
#             attr(capthist, "timecov") = sapply(session(capthist), function(session){
#                 sub = tempcov[tempcov$array == session, , drop = FALSE]
#                 rownames(sub) = NULL
#                 return(sub)
#             }, simplify = FALSE)
            timecov(capthist) = sapply(session(capthist), function(session){ # session = session(capthist)[1] ; session
                # print(session)
                sub = tempcov[temparray == session, , drop = FALSE]
                rownames(sub) = NULL
                return(sub)
            }, simplify = FALSE)
        }else{
            # for all remaining levels, don't use timecov
            tempcov = tempcov[, !colnames(tempcov) %in% colnames(attr(capthist, "timecov")), drop = FALSE]
            # if no remaining covariates, then go to next loop
            if(ncol(tempcov) == 0) next
        }

        ##================================================
        ## trapcov

        # is a trap attribute and is accessed via secr::covariates(traps)
        # traps for separate sessions have their own data.frames
        if(level == "trapcov"){
            # dont use post or occasion
            tempcov = tempcov[, !colnames(tempcov) %in% c("post","occasion"), drop = FALSE]
            if(ncol(tempcov) == 0) next
            # add tempcovs to each session
            for(session in session(capthist)){ # session = session(capthist)[1] ; session
                sub = tempcov[temparray == session, , drop = FALSE]
                rownames(sub) = NULL
                covariates(traps(capthist[[session]])) = sub
            }
            # for all remaining levels, don't use trapcov
            tempcov = tempcov[, !colnames(tempcov) %in% colnames(covariates(traps(capthist[[1]]))), drop = FALSE]
            # if no remaining covariates, then go to next loop
            if(ncol(tempcov) == 0) next
        }

        ##================================================
        ## timevaryingcov

        # accessed via secr::timevaryingcov(traps)
        # but there is no useful description on any of the help pages
        # will have to work it out by trial and error
        if(level == "timevaryingcov"){
            # dont use occasion or post
            tempcov = tempcov[, !colnames(tempcov) %in% c("occasion","post"), drop = FALSE]
            if(ncol(tempcov) == 0) next
            # add timevaryingcovS to each session
            for(session in session(capthist)){ # session = session(capthist)[1] ; session
                sub = tempcov[temparray == session, , drop = FALSE]
                rownames(sub) = NULL
                # timevaryingcov(traps(capthist[[session]])) = sub
                timevaryingcov(traps(capthist[[session]])) = NULL
            }
        }
    }

    # add centred x and y covariates to traps
#     centre = apply(do.call(rbind, traps(capthist)), 2, mean)
#     for(session in session(capthist)){ # session = session(capthist)[1] ; session
#         coords = mapply(function(x, y) (x - y), traps(capthist[[session]]), centre)
#         rownames(coords) = rownames(traps(capthist[[session]]))
#         if(is.null(covariates(traps(capthist[[session]])))){
#             covariates(traps(capthist[[session]])) = coords
#         }else{
#             covariates(traps(capthist[[session]])) = cbind(
#                 coords,
#                 covariates(traps(capthist[[session]]))
#             )
#         }
#     }

    #     # add covariates to capthist
    #     for(session in session(capthist)){ # session = session(capthist)[1] ; session
    #
    #         # group-level covariates
    #         # covariates(capthist[[session]]) = covlevels[[session]][["groupcov"]]
    #
    #         # add trap-level covariates to traps
    #         trapcov = covlevels[[session]][["trapcov"]] ; trapcov
    #         covariates(traps(capthist[[session]])) = trapcov
    #
    #         # identify and timevarying covariates
    #         timevaryingcov.names = unique(unlist(lapply(strsplit(colnames(trapcov), "\\."), function(x){
    #             if(length(x) == 2) return(x[1])
    #         }))) ; timevaryingcov.names
    #
    #         # if there are any timevaryingcovs, then update the trapcov to indicate the associated columns
    #         if(!is.null(timevaryingcov.names)){
    #             timevaryingcov(traps(capthist[[session]])) = sapply(timevaryingcov.names, function(tvc){
    #                 which(grepl(tvc, colnames(trapcov)))
    #             }, simplify = FALSE)
    #         }
    #
    #         covariates(traps(capthist[[session]]))
    #
    # }

    return(capthist)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

#' @title Import region and habitat shapefiles
#' @description TODO
#' @details TODO
#' @param filepath TODO
#' @param type TODO
#' @param verbose TODO
#' @param ... TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export

import_shp = function(filepath, type = c("region","habitat"), verbose = FALSE, ...){
    type = match.arg(type)
    if(tools::file_ext(filepath) == "")
        filepath = paste0(filepath, ".shp")
    if(tools::file_ext(filepath) != "shp")
        stop("full path to an .shp file required", call. = FALSE)
    if(!file.exists(filepath))
        filepath = file.path(getwd(), filepath)
    if(!file.exists(filepath))
        stop("can't find file - check file path", call. = FALSE)
    x = try({
        raster::shapefile(filepath, verbose = verbose, ...)
    }, TRUE)
    if(inherits(x, "try-error"))
        stop("couldn't import shapefile", call. = FALSE)
    if(!inherits(x, c("SpatialPolygons", "SpatialPoints")))
        stop("only works for polygons and points")

    ##################################################
    ## check data

    if(inherits(x, c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame"))){
        for(j in 1:ncol(x@data)){
            if(inherits(x@data[[j]], c("character","logical"))){
                x@data[[j]] = as.factor(x@data[[j]])
            }
        }
    }


    ##################################################
    ## check habitat

#     if(type == "habitat"){
#         if(length(x@polygons) == 1)
#             stop("only one layer in habitat shp file")
#         # convert to SpatialPolygonsDataFrame if necessary
#         # and use polygon IDs as habitat labels if no habitat column
#         spdf = inherits(x, "SpatialPolygonsDataFrame")
#         habcol = spdf && "habitat" %in% colnames(x@data)
#         if(!spdf || !habcol){
#             message("- no habitat variable in shp data so using polygon IDs")
#             IDs = sapply(x@polygons, function(x) x@ID)
#             data = data.frame(habitat = factor(IDs), row.names = IDs)
#             colnames(data) = tools::file_path_sans_ext(basename(filepath))
#             if(!spdf)
#                 x = sp::SpatialPolygonsDataFrame(x, data)
#             if(!habcol)
#                 x@data = data
#         }
#     }

    ##################################################
    ## check region

#     if(type == "region"){
#
#     }

    return(x)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

make_design_matrices = function(model, model.frames, smooth.setup, sessions = NULL, submodels = NULL, debug = FALSE){
    if(is.null(sessions)) sessions = names(model.frames)
    if(is.null(submodels)) submodels = names(model.frames[[1]])
    design.matrices = sapply(sessions, function(session){
        # session = names(model.frames)[1] ; session
        sapply(submodels, function(submodel){
            # submodel = submodels[1] ; submodel
#             make_model_matrix(
#                 formula      = model[[submodel]],
#                 data         = model.frames[[session]][[submodel]],
#                 smooth.setup = smooth.setup[[submodel]]
#             )
            make_model_matrix(
                data         = model.frames[[session]][[submodel]],
                smooth.setup = smooth.setup[[submodel]]
            )
        }, simplify = FALSE)
    }, simplify = FALSE)

    ##################################################
    ## debugging

    if(debug){
        if(!all(names(design.matrices) == sessions))
            stop("!all(names(design.matrices) == sessions)")
        if(!all(names(design.matrices[[1]]) == submodels))
            stop("!all(names(design.matrices[[1]]) == submodels)")
        for(session in sessions){ # session = sessions[1] ; session
            for(submodel in submodels){ # submodel = "D"
                temp = design.matrices[[session]][[submodel]]
                nrows = nrow(model.frames[[session]][[submodel]])
                result = try(all(nrow(temp) == nrows), TRUE)
                if(inherits(result, "try-error") || !result)
                    stop("nrow(design.matrix) not equal to nrow(model.frame) for '",
                         submodel, "' in session '", session, "'")
            }
        }
    }
    return(design.matrices)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

make_model_frames = function(model, traps, mask, n_occasions, sessioncov = NULL, timecov = NULL, sessions = NULL, submodels = NULL, debug = FALSE){

    # "D"       maskcov
    # "pcall"   timecov
    # rest      timevaryingcov

    # model = fit$model; traps = traps(fit$capthist); mask = fit$mask; n_occasions = n_occasions(fit$capthist); sessioncov = sessioncov(fit$capthist); timecov = timecov(fit$capthist); sessions = "2"; submodels = "D"

    # sessions = submodels = NULL

    ##################################################
    ## check inputs

    if(!ms(traps))
        stop("only works with multi-session traps")
    if(!ms(mask))
        stop("only works with multi-session masks")
    if(!is.null(sessions)){
        if(!all(sessions %in% session(mask)))
            stop("not all sessions are in session(mask)")
    }else sessions = session(mask)
    if(!all(sessions %in% session(mask)))
       stop("not all sessions are in session(mask)")
    if(!all(sessions %in% session(traps)))
       stop("not all sessions are in session(traps)")
    if(!all(sessions %in% names(n_occasions)))
       stop("not all sessions are in names(n_occasions)")
    if(!is.null(sessioncov))
        if(!all(sessions %in% rownames(sessioncov)))
            stop("not all sessions are in rownames(sessioncov)")
    if(!is.null(timecov))
        if(!all(sessions %in% names(timecov)))
            stop("not all sessions are in names(timecov)")
    if(!is.null(submodels)){
        if(!all(submodels %in% names(model)))
            stop("not all submodels are in names(traps)")
    }else submodels = names(model)

    ##################################################
    ## make list of covariates

    model.frames = sapply(sessions, function(session){ # session = sessions[1] ; session

        # session objects and data stats
        temptraps = traps[[session]]
        tempmask  = mask[[session]]
        S         = n_occasions[session]
        K         = nrow(temptraps)
        M         = mask_npoints(tempmask)
        temptimecov = timecov[[session]]
        tempsessioncov = if(is.null(sessioncov)) NULL else{
            sessioncov[session, , drop = FALSE]
        }

        ##################################################
        ## trapcov

        # only covariates that are constant for each trap-session combination
        # e.g. can't use covariates that change accross sessions

        trapcov = trapcov(temptraps)
        # remove timevaryingcov - these will be added in modified form later
        if(!is.null(timevaryingcov(temptraps))){
            tvc = do.call(c, timevaryingcov(temptraps))
            trapcov = trapcov[, -tvc, drop = FALSE]
        }
        # add sessioncov
        if(!is.null(tempsessioncov)){
            trapcov = cbind(
                trapcov,
                tempsessioncov[rep(1, K), , drop = FALSE]
            )
        }

        ##################################################
        ## timevaryingcov

        # can use any covariates, including those that change across trap-occasion

        if(is.null(timevaryingcov(temptraps))){
            timevaryingcov = NULL
        }else{
            timevaryingcov = as.data.frame(
                lapply(timevaryingcov(temptraps), function(x){
                    unname(do.call(c, as.list(covariates(temptraps)[,x])))
                })
            )
        }
        # add timecov
        if(!is.null(temptimecov)){
            temptimecov = temptimecov[rep(1:S, each = K), , drop = FALSE]
            timevaryingcov = if(is.null(timevaryingcov)){
                temptimecov
            }else{
                cbind(timevaryingcov, temptimecov)
            }
        }
        # add trapcov
        temptrapcov = trapcov[rep(1:K, each = S), , drop = FALSE]
        timevaryingcov = if(is.null(timevaryingcov)){
            temptrapcov
        }else{
            cbind(timevaryingcov, temptrapcov)
        }
        rownames(timevaryingcov) = NULL

        ##################################################
        ## maskcov

        maskcov = maskcov(tempmask)
        # maskcov = as.data.frame(tempmask)
        # if(!is.null(covariates(tempmask))){
            # maskcov = cbind(maskcov, covariates(tempmask)) # head(maskcov)
        # }
        # add sessioncov
        if(!is.null(tempsessioncov)){
            maskcov = cbind(
                maskcov,
                tempsessioncov[rep(1, M), , drop = FALSE]
            )
        }

        ##################################################
        ## timecov

        # timecov(capthist[[session]])
        timecov = if(is.null(temptimecov)){
            data.frame(row.names = 1:S)
            # data.frame(dummy = rep(1,S))
        }else{
            temptimecov
        }
        # add sessioncov
        if(!is.null(sessioncov)){
            # temptimecov = tempsessioncov[rep(1, S), , drop = FALSE]
            timecov = if(is.null(timecov)){
                tempsessioncov[rep(1, S), , drop = FALSE]
            }else{
                cbind(timecov, tempsessioncov[rep(1, S), , drop = FALSE])
            }
        }

        ##################################################
        ## loop over submodels and get model frame for each

        sapply(submodels, function(submodel){ # submodel = "pcall"
            switch(submodel,
                   "D"     = maskcov,
                   "pcall" = timecov,
                   timevaryingcov
            )[, all.vars(model[[submodel]]), drop = FALSE]
            # )
        }, simplify = FALSE)

    }, simplify = FALSE)

    ##################################################
    ## debugging

    if(debug){
        if(!all(names(model.frames) == sessions))
            stop("!all(names(model.frames) == sessions)")
        if(!all(names(model.frames[[1]]) == submodels))
            stop("!all(names(model.frames[[1]]) == submodels)")
        for(session in sessions){ # session = sessions[1] ; session
            for(submodel in submodels){ # submodel = "pcall"
                temp = model.frames[[session]][[submodel]]
                if(is.null(dim(temp)))
                    stop("something went wrong with make_model_frames for '",
                         submodel, "' in session '", session, "'")
            }
        }
    }

    return(model.frames)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# # used inside make_design_matrices
# # if no smooth.setup then uses model.matrix
# # if smooth.setup is provided then this is used to pursuade mgcv::predict.gam to
# # provide a design matrix using an existing gam model and new covariate data
#
# make_model_matrix_old = function(formula, data, smooth.setup){
#     if(is.null(smooth.setup)){
#         # if no gam model then use conventional method
#         X = model.matrix(formula, data)
#     }else{
#         # add dummy response to lhs of formula
#         formula = update.formula(formula, dummy ~ .)
#         # make sure left hand side variable is present in data
#         response = all.vars(formula)[1]
#         if(is.null(data[[response]])) data[[response]] = 1
#         # get design matrix from predict.gam
#         class(smooth.setup) = "gam"
#         smooth.setup$coefficients = rep(NA, ncol(smooth.setup$X))
#         X = mgcv::predict.gam(smooth.setup, newdata = data, type = 'lpmatrix')
#         colnames(X) = smooth.setup$term.names
#     }
#     return(X)
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# used inside make_design_matrices
# if no smooth.setup then uses model.matrix
# if smooth.setup is provided then this is used to pursuade mgcv::predict.gam to
# provide a design matrix using an existing gam model and new covariate data

make_model_matrix = function(data, smooth.setup){
    formula = smooth.setup$formula
    response = all.vars(formula)[1]
    # add response variable to data
    if(is.null(data[[response]])) data[[response]] = 1
    # get design matrix from predict.gam
    class(smooth.setup) = "gam"
    smooth.setup$coefficients = rep(NA, ncol(smooth.setup$X))
    X = mgcv::predict.gam(smooth.setup, newdata = data, type = 'lpmatrix')
    colnames(X) = smooth.setup$term.names
    return(X)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# works in side the fit_gibbonsecr function
# makes a reference table of labels for estimated parameters

make_par_labels = function(design.matrices, fixed){
    # names of all submodel
    submodel.names = names(design.matrices[[1]])
    # names of submodels with parameters to be estimated
    submodel.names = submodel.names[!submodel.names %in% names(fixed)]
    # submodel.names = submodel.names[submodel.names != "D_locations" & !submodel.names %in% names(fixed)]
    # list of term names by submodel
    # - list names are equal to submodel names
    # - list elements are term names
    term.names = sapply(submodel.names, function(submodel){
        colnames(design.matrices[[1]][[submodel]])
    }, simplify = FALSE)
    # return a table with three columns:
    # - sumbmodel = submodel name ("D", "g0", sigma", etc.)
    # - term      = name of term in submodel, generated by model.matrix (e.g. "(Intercept)")
    # - unique    = unique parameter label, combining the submodel and term names
    cbind(
        submodel = do.call(c, lapply(names(term.names), function(submodel){ # submodel = "D"
            rep(submodel, length(term.names[[submodel]]))
        })),
        term = do.call(c, unname(term.names)),
        unique = do.call(c, lapply(names(term.names), function(submodel){ # submodel = "D"
            paste(submodel, term.names[[submodel]], sep = ".")
        }))
    )
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

make_parindx = function(betanames){
    submodel.names = c("D", "g0", "sigma", "z", "pcall", "bearings", "distances")
    parindx = sapply(submodel.names, function(submodel){
        matches = grepl(paste0("^", submodel, "\\."), betanames)
        if(any(matches)) return(which(matches)) else return(NULL)
    }, simplify = FALSE)
    parindx = parindx[!sapply(parindx, is.null)]
    return(parindx)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# works inside the make_submodel_arrays function
# makes a list of all parameter values - both fixed and current estimated parameter values in fitting process
# each element represents a different submodel

# make_par_list = function(beta, par.labels, fixed){
#     submodel.names = unique(par.labels[,"submodel"])
#     parlist = sapply(submodel.names, function(submodel){ # submodel = submodel.names[1] ; submodel
#         i = par.labels[,"submodel"] == submodel
#         temp = beta[i]
#         names(temp) = par.labels[i,"unique"]
#         return(temp)
#     }, simplify = FALSE)
#     parlist = c(parlist, fixed)
#     return(parlist)
# }

make_parlist = function(beta, parindx, fixed){
    c(lapply(parindx, function(x){
        beta[x]
    }), fixed)
}


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

make_regular_regionmask = function(mask, traps){ # mask=fit$mask; traps=fit$capthist
    if(!inherits(mask, "mask") || !ms(mask))
        stop("requires a multi-session mask object")
    if(!inherits(traps, c("traps","capthist")) || !ms(traps))
        stop("requires a multi-session traps or capthist object")
    if(inherits(traps, "capthist"))
        traps = traps(traps)
    if(!all(session(mask) == session(traps)))
        stop("!all(session(mask) == session(traps))")
    regionmask = make_regionmask(mask)
    regiontraps = make_regiontraps(traps)
    regularmask = make.mask(regiontraps,
                     buffer  = mask_buffer(regionmask, regiontraps),
                     spacing = mask_spacing(regionmask),
                     type    = mask_type(regionmask))
    if(!is.null(covariates(regionmask))){
        regularmask = addCovariates(regularmask, regionmask)
    }
    return(regularmask)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# if no smooth terms then return null
# otherwise trik mgcv::gam into providing
# smooth.setup used inside make_model_matrix (which is inside make_design_matrices)

# make_smooth_setup_old = function(model, model.frames){
#     sapply(names(model), function(submodel){
#         if(!contains_smooth_terms(model[[submodel]]))
#             return(NULL)
#         # combine covariates from all sessions
#         data = do.call(rbind, sapply(names(model.frames), function(session){
#                 model.frames[[session]][[submodel]]
#             }, simplify = FALSE)
#         )
#         # add a dummy response variable since gam requires a two-sided formula
#         data$dummy = 1
#         formula = update.formula(model[[submodel]], dummy ~ .)
#         # get smooth details using gam
#         mgcv::gam(formula, data = data, fit = FALSE)
#     })
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# return a 'smooth.setup' object (i.e 'G') for all models
# smooth.setup used inside make_model_matrix (which is inside make_design_matrices)

# function to make smooth setup for all models
# allows use of more complex formula syntax (e.g. as.numeric(x))

#' @importFrom dplyr bind_rows

make_smooth_setup = function(model, model.frames){
    sapply(names(model), function(submodel){ # submodel = "D"
        # combine covariates for all sessions
        data = as.data.frame(do.call(
            bind_rows,
            sapply(names(model.frames), function(session){
                model.frames[[session]][[submodel]]
            }, simplify = FALSE)
        ))
        # add dummy respobse variable to left hand side to formula
        # use the submodel name since this is unlikely to the name of a covariate
        formula = model[[submodel]]
        eval(parse(text = paste0("formula = update.formula(formula,",
                                 submodel, " ~ .)")))
        # add dummy column to the data
        data[[submodel]] = 1
        # get the 'G' object from the gam
        mgcv::gam(formula, data = data, fit = FALSE)
    }, simplify = FALSE)
}

# -------------------------------------------------------------------------- ##
# -------------------------------------------------------------------------- ##

# works inside the gibbons_negloglik_wrapper function
# creates lists of arrays of submodel values from the design matrices and
# current parameter values in the fitting procedure
# submodel values need to be stored in arrays since values can change with
# spatial location, occasion, post, etc.

# make_submodel_arrays = function(beta, par.labels, fixed, design.matrices, inv.link, n, S, K){
make_submodel_arrays = function(beta, parindx, fixed, design.matrices, inv.link, S, K, sessions = NULL, submodels = NULL){
    # beta = start
    # parlist = make_par_list(beta, parindx, fixed)
    parlist = make_parlist(beta, parindx, fixed)
    if(is.null(sessions)) sessions = names(design.matrices)
    if(is.null(submodels)) submodels = names(parlist)
    sapply(sessions, function(session){ # session = sessions[1] ; session
        sapply(submodels, function(submodel){ # submodel = "g0"
            response.scale = inv.link[[submodel]](as.numeric(design.matrices[[session]][[submodel]] %*% parlist[[submodel]]))
            if(submodel %in% c("D","pcall")){
                # density uses a M by 1 matrix
                # pcall uses a S by 1 matrix
                matrix(response.scale, ncol = 1)
            }else{
                # the rest use S by K matrices
                # fill by row, since design matrices are ordered by S then K
                matrix(response.scale, nrow = S[session], ncol = K[session], byrow = TRUE)
            }
        }, simplify = FALSE)
    }, simplify = FALSE)
}

# -------------------------------------------------------------------------- ##
# -------------------------------------------------------------------------- ##

# works inside the gibbons_negloglik_wrapper function
# creates lists of arrays of submodel values from the design matrices and
# current parameter values in the fitting procedure
# submodel values need to be stored in arrays since values can change with
# spatial location, occasion, post, etc.

# make_submodel_arrays2 = function(beta, parindx, fixed, design.matrices, inv.link, S, K){
#     # beta = start
#     parlist = make_parlist(beta, parindx, fixed)
#     sapply(names(design.matrices), function(session){
#         # session = names(design.matrices)[1] ; session
#         sapply(names(design.matrices[[session]]), function(submodel){
#             # submodel = "g0"
#             response.scale = inv.link[[submodel]](as.numeric(design.matrices[[session]][[submodel]] %*% parlist[[submodel]]))
#             if(submodel %in% c("D","pcall")){
#                 # density uses a M by 1 matrix
#                 # pcall uses a S by 1 matrix
#                 matrix(response.scale, ncol = 1)
#             }else{
#                 # the rest use S by K matrices
#                 # fill by row, since design matrices are ordered by S then K
#                 matrix(response.scale, nrow = S[session], ncol = K[session], byrow = TRUE)
#             }
#         }, simplify = FALSE)
#     }, simplify = FALSE)
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# evaluates the log-likelihood values separately for data from each array
# returns the sum of the loglik values

negloglik_wrapper = function(beta, parindx, fixed, design.matrices, inv.link, data, mask.info, model.options, usage, detected, R, n, S, K, M, a, trace = FALSE){
    # beta = start
    submodel.arrays = make_submodel_arrays(beta, parindx, fixed, design.matrices, inv.link, S, K)
    # lapply(submodel.arrays[[1]], head) ; lapply(submodel.arrays[[1]], dim)
    nll = sum(sapply(1:R, function(i){ # i=1
        negloglik_rcpp(
            data               = data[[i]],
            mask               = mask.info[[i]],
            pars               = submodel.arrays[[i]],
            detectfn_code      = model.options[["detectfn"]],
            bearings_pdf_code  = model.options[["bearings"]],
            distances_pdf_code = model.options[["distances"]],
            detected           = detected[[i]],
            usage              = usage[[i]],
            n                  = n[i],
            S                  = S[i],
            K                  = K[i],
            M                  = M[i],
            a                  = a[i]
        )
    }))
    if(!is.finite(nll)) nll = 1e10
    if(trace){
        cat("loglik =", -nll)
        for(i in 1:length(beta)) cat(", beta", i, " = ", round(beta[i], 2), sep="")
        cat("\n")
    }
    return(nll)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# evaluates the log-likelihood values separately for data from each array
# returns the sum of the loglik values

negloglik_wrapper2 = function(beta, parindx, fixed, design.matrices, inv.link, data, mask.info, model.options, usage, detected, R, n, S, K, M, a, trace = FALSE){
    # beta = start
    submodel.arrays = make_submodel_arrays(beta, parindx, fixed, design.matrices, inv.link, S, K)
    # lapply(submodel.arrays[[1]], head) ; lapply(submodel.arrays[[1]], dim)
    nll = sum(sapply(1:R, function(i){ # i=1
        negloglik_rcpp(
            data               = data[[i]],
            mask               = mask.info[[i]],
            pars               = submodel.arrays[[i]],
            detectfn_code      = model.options[["detectfn"]],
            bearings_pdf_code  = model.options[["bearings"]],
            distances_pdf_code = model.options[["distances"]],
            detected           = detected[[i]],
            usage              = usage[[i]],
            n                  = n[i],
            S                  = S[i],
            K                  = K[i],
            M                  = M[i],
            a                  = a[i]
        )
    }))
    if(!is.finite(nll)) nll = 1e10
    if(trace){
        cat("loglik =", -nll)
        for(i in 1:length(beta)) cat(", beta", i, " = ", round(beta[i], 2), sep="")
        cat("\n")
    }
    return(nll)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

num2col = function(values, palette = rainbow, ncol = 12){
    levels = cut(values, breaks = ncol)
    # levels = cut(values, breaks = seq(min(values), max(values), length = ncol + 1))
    palette(ncol)[levels]
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# prepare data into a format that can be read by the negloglik wrapper function

prepare_data = function(capthist, model.options, locations = FALSE){
    captures  = get_captures(capthist)
    bearings  = get_bearings(capthist)
    distances = get_distances(capthist)
    data = lapply(session(capthist), function(i){
        list(
            capthist  = captures[[i]],
            bearings  = bearings[[i]],
            distances = distances[[i]]
        )
    })
    return(data)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# calculate distance and bearings to all mask points from all traps

prepare_mask_info = function(mask, capthist, model.options){
    bearings = calc_bearings(traps(capthist), mask)
    distances = calc_distances(traps(capthist), mask)
    mask.info = sapply(session(capthist), function(i){
        list(
            bearings  = bearings[[i]],
            distances = distances[[i]]
        )
    }, simplify = FALSE)
    return(mask.info)
}


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

print_loop_progress = function(loop, nloops, width = 50){
    ndashes = round(width * loop / nloops)
    slashb = rep('\b', width + 8)
    lines = rep("-", ndashes)
    spaces = rep(" ", width - ndashes)
    cat(paste(c(slashb, "|", lines, spaces, "|"), collapse = ""), round(100 * loop / nloops), "%")
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
