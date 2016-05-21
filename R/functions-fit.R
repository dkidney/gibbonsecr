
# main fitting function ========================================================

#' @title Fit an SECR model to survey data
#' @description Fit a Spatially Explicit Capture-Recapture (SECR) model to
#'   survey data collected from arrays of proximity detectors.
#' @param capthist \link[gibbonsecr]{gcapthist} object (or a
#'   \link[secr]{capthist} object with \link[secr]{proximity} traps)
#' @param model list of formulae for the model parameters (see Details)
#' @param mask \link[gibbonsecr]{gmask} object (if \code{NULL} a default mask
#'   is used)
#' @param fixed list of fixed values for the model parameters
#' @param model.options list of control options determining the type of model
#'   to be fitted (see Details)
#' @param mask.options list of options for the default mask (see Details)
#' @param fitting.options list of options controlling the fitting process (see
#'   Details)
#' @param start numeric vector of starting values for the fitting algorithm
#' @param trace if \code{TRUE}, information for each iteration of the fitting
#'   procedure is printed to the console
#' @details Parameter estimates are obtained via numerical optimization using
#'   the \code{\link[stats]{nlm}} function.
#' \describe{
#'  \item{\strong{\code{model}}}{
#'   Should contain a list of formulae for the following paramters:
#'   \tabular{ll}{
#'    \code{D}         \tab density (number of animals/groups per square
#'    kilometre) \cr
#'    \code{g0}        \tab detection function intercept \cr
#'    \code{sigma}     \tab detection function scale (metres) \cr
#'    \code{z}         \tab detection function shape \cr
#'    \code{bearings}  \tab estimated bearings distribution scale \cr
#'    \code{distances} \tab estimated distances distribution shape \cr
#'    \code{pcall}     \tab availability (proportion) \cr
#'   }
#'   If no model is specified then intercept-only formulae are used.
#'
#'   Model formulae can be specified using any of the the available covrariates
#'   (see \link[gibbonsecr]{gibbonsecr-covariates} for advice on model
#'   covariates).
#'
#'   Smooth terms from the \pkg{\link[mgcv]{mgcv}} package -- including
#'   \code{\link[mgcv]{s}}, \code{\link[mgcv]{te}} and \code{\link[mgcv]{ti}} --
#'   can also be used. However, only the un-penalised components of the smooth
#'   terms are used (the design matrix is obtained from \code{\link[mgcv]{gam}}
#'   using \code{fit = FALSE}) and no smoothing penalties are estimated (i.e.
#'   models are fitted as un-penalised regression splines).
#'  }
#'  \item{\strong{\code{model.options}}}{
#'   Should be a list of three elements, named \code{detfunc},
#'   \code{bearings} and \code{distances}, with integer values. The table below
#'   shows the available settings (defaults in italics):
#'   \tabular{llll}{
#'    \code{detfunc} \tab 0 = \emph{half normal} \tab 1 = hazard rate \tab \cr
#'    \code{bearings} \tab 0 = no model \tab 1 = \emph{von Mises} \tab 2 =
#'    wrapped Cauchy \cr
#'    \code{distances} \tab 0 = \emph{no model} \tab 1 = gamma \tab 2 =
#'    lognormal \cr
#'   }
#'   If any of the elements are left blank then the default values are used.
#'  }
#'  \item{\strong{\code{mask.options}}}{
#'   Ignored unless the \code{mask} argument is \code{NULL}, in which case it
#'   should be a list of two elements named \code{buffer} and \code{spacing}
#'   giving the buffer and spacing to be used when constructing a default mask.
#'   If the mask and arguments are both \code{NULL} the should be a list of
#'   three elements, named \code{detfunc},
#'  }
#'  \item{\strong{\code{fitting.options}}}{
#'   Should be a list of the following elements:
#'   \tabular{ll}{
#'    \code{hessian} \tab if \code{TRUE}, the estimated
#'    hessian matrix is returned (default = TRUE) \cr
#'    \code{iterlim} \tab maximum number of fitting iterations (default = 1000)
#'    \cr
#'    \code{LLonly} \tab if \code{TRUE}, then the log-likelihood using the
#'    starting values is returned (default = FALSE) \cr
#'    \code{fit} \tab if \code{FALSE}, the arguments passed to
#'    \code{\link[stats]{nlm}} are returned (default = TRUE) \cr
#'   }
#'   If any of the elements are left blank then the default values are used.
#'  }
#' }
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link[gibbonsecr]{plot.gsecr}
#' @example inst/examples/example-gfit.r
#' @importFrom stats nlm
#' @export

gfit = function(capthist, model = list(), mask = NULL, fixed = list(), model.options = list(), mask.options = list(), fitting.options = list(), start = NULL, trace = FALSE){
    message("fitting SECR model...")

    start.time = Sys.time()

    # check inputs ----------------------------------------------------------- #
    capthist = check_capthist(capthist)
    mask = check_mask(mask, capthist, mask.options)
    model.options = check_model_options(model.options, capthist)
    fixed = check_fixed(fixed, model.options, capthist)
    model = check_model(model, fixed, model.options, capthist, mask)

    # capthist and mask stats ------------------------------------------------ #
    R = n_arrays(capthist)
    n = n_groups(capthist)
    S = n_occasions(capthist)
    K = n_traps(capthist)
    a = area(mask)
    M = size(mask)

    # Fitting function arguments --------------------------------------------- #
    model.frames = make_model_frames(
        model, traps(capthist), mask, n_occasions(capthist),
        sessioncov(capthist), timecov(capthist))
    smooth.setup = make_smooth_setup(model, model.frames)
    design.matrices = make_design_matrices(model, model.frames, smooth.setup)
    par.labels = make_par_labels(design.matrices, fixed)
    parindx = make_parindx(par.labels[,"unique"])
    inv.link = get_inv_link(model, fixed, model.options)
    detected = get_captures(capthist, summarise = "occasions")
    data = prepare_data(capthist, model.options)
    mask.info = prepare_mask_info(mask, capthist, model.options)
    usage = usage(traps(capthist))
    start = check_start_values(
        start, capthist, mask, model.options, fixed, S, K, M, a, usage,
        design.matrices, par.labels, parindx, inv.link, mask.info)
    fitting.options = replace(
        x = list(hessian = TRUE, iterlim = 1000, LLonly = FALSE, fit = TRUE),
        list = names(fitting.options), values = fitting.options)

    # fitting function ------------------------------------------------------- #
    # can use if statement to choose between likelihood functions if locations
    # known is implemented
    nll.function = negloglik_wrapper

    # Fitting function arguments list ---------------------------------------- #
    nll.args = list(
        beta = start, data = data, mask.info = mask.info,
        design.matrices = design.matrices, parindx = parindx, fixed = fixed,
        inv.link = inv.link, model.options = model.options, usage = usage,
        detected = detected, R = R, n = n, S = S, K = K, M = M, a = a,
        trace = trace)

    # fit = FALSE ------------------------------------------------------------ #
    if(!fitting.options$fit) return(nll.args)

    # LLonly = TRUE ---------------------------------------------------------- #
    if(fitting.options$LLonly){
        nll = try(do.call(nll.function, nll.args))
        if(inherits(nll,"try-error")){
            stop("Unable to evaluate loglikelihood using start values")
        }else{
            return(-nll)
        }
    }

    # fit using nlm ---------------------------------------------------------- #
    message("- fitting model...")
    nll.args$beta = NULL
    nll.args = c(nll.args, list(
        f = nll.function, p = start, hessian = fitting.options$hessian,
        iterlim = fitting.options$iterlim
    ))
    nlm.results = try(do.call(nlm, nll.args))

    # process nlm results  --------------------------------------------------- #
    if(nlm.results$code > 1) warning("nlm code = ", nlm.results$code)
    if(nlm.results$iterations <= 1)
        warning("nlm iterations = ", nlm.results$iterations)
    names(nlm.results$estimate) = par.labels[,"unique"]
    if(!is.null(nlm.results$hessian)){
        rownames(nlm.results$hessian) = par.labels[,"unique"]
        colnames(nlm.results$hessian) = rownames(nlm.results$hessian)
    }

    # return gsecr object ------------------------------------------- #
    fit = list(
        capthist = capthist, mask = mask, model = model,
        model.options = model.options, fixed = fixed,
        model.frames = model.frames, design.matrices = design.matrices,
        inv.link = inv.link, start = start, smooth.setup = smooth.setup,
        mask.info = mask.info, nlm = nlm.results, parindx = parindx,
        par.labels = par.labels, run.time = difftime(Sys.time(), start.time)
    )
    class(fit) = c("gsecr", class(fit))
    return(fit)
}

# nlm input function ===========================================================

# evaluates the log-likelihood values separately for data from each array
# returns the sum of the loglik values

negloglik_wrapper = function(beta, parindx, fixed, design.matrices, inv.link, data, mask.info, model.options, usage, detected, R, n, S, K, M, a, trace = FALSE){
    # beta = start
    submodel.arrays = make_submodel_arrays(beta, parindx, fixed, design.matrices,
                                           inv.link, S, K)
    # lapply(submodel.arrays[[1]], head) ; lapply(submodel.arrays[[1]], dim)
    nll = sum(sapply(1:R, function(i){ # i=1
        negloglik_rcpp(
            data               = data[[i]],
            mask               = mask.info[[i]],
            pars               = submodel.arrays[[i]],
            detfunc_code      = model.options[["detfunc"]],
            bearings_pdf_code  = model.options[["bearings"]],
            distances_pdf_code = model.options[["distances"]],
            detected           = detected[[i]],
            usage              = usage[[i]],
            n                  = n[i],
            S                  = S[i],
            K                  = K[i],
            M                  = M[i],
            a                  = a
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

#==============================================================================#

calc_detprob = function(detfunc, g0, sigma, z, distances, usage, M, S, K){
    FUN = list(hn, hr)[[detfunc + 1]]
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

#==============================================================================#

calc_esa = function(detfunc, beta, parindx, fixed, design.matrices, distances,
                    usage, inv.link, S, K, M, a){
    submodel.arrays = make_submodel_arrays(beta, parindx, fixed, design.matrices,
                                           inv.link, S, K)
    session.names = names(design.matrices)
    esa = sapply(session.names, function(session){ # session = session.names[1] ; session
        pdot = calc_pdot(
            detfunc  = detfunc,
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
        esa = sum(pdot) * a
        return(esa)
    })
    return(esa)
}

#==============================================================================#

calc_pdot = function(detfunc, g0, sigma, z, pcall, distances, usage, M, S, K){
    one_minus_detprob = 1 - calc_detprob(detfunc, g0, sigma, z, distances,
                                         usage, M, S, K)
    # non-detection probabilities for each maskpoint-occasion combination
    prodK = apply(one_minus_detprob, c(1,2), prod)
    # non-detection probabilities for each maskpoint
    # see DK thesis (Sec 6.2, p104) for an explanation of the pcall bit
    prodSK = array(1, dim = M)
    for(s in 1:S){
        if(pcall[s] < 1){
            for(m in 1:M)
                prodSK[m] = prodSK[m] * { (1.0 - pcall[s]) + pcall[s] * prodK[m,s] }
        }else{
            for(m in 1:M)
                prodSK[m] = prodSK[m] * prodK[m,s]
        }
    }
    # detection probabilities for each maskpoint
    pdot = 1.0 - prodSK
    return(pdot)
}

#==============================================================================#

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

# distances distributions ======================================================

rgamma = function(n, par, EX){
    stats::rgamma(n = n, shape = par, scale = EX / par)
}

dgamma = function(x, par, EX){
    stats::dgamma(x = x, shape = par, scale = EX / par)
}

rlnorm = function(n, par, EX){
    stats::rlnorm(n = n, meanlog = log(EX) - par^2 / 2, sdlog = par)
}

dlnorm = function(x, par, EX){
    stats::dlnorm(x = x, meanlog = log(EX) - par^2 / 2, sdlog = par)
}


# bearings distributions =======================================================

rvm = function(n, par, EX){
    CircStats::rvm(n = n, mean = EX, k = par)
}

dvm = function(x, par, EX){
    CircStats::dvm(theta = x, mu = EX, kappa = par)
}

rwrpcauchy = function(n, par, EX){
    CircStats::rwrpcauchy(n = n, location = EX, rho = par)
}

dwrpcauchy = function(x, par, EX){
    CircStats::dwrpcauchy(theta = x, mu = EX, rho = par)
}

# design matrices etc. =========================================================

# return a 'smooth.setup' object (i.e 'G') for all models
# smooth.setup used inside make_model_matrix (which is inside make_design_matrices)

# function to make smooth setup for all models
# allows use of more complex formula syntax (e.g. as.numeric(x))

#' @importFrom dplyr bind_rows
make_smooth_setup = function(model, model.frames){
    sapply(names(model), function(submodel){ # submodel = names(model)[4] ; submodel
        # combine covariates for all sessions
        data = as.data.frame(do.call(
            "bind_rows",
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
        if(nrow(data) == 1){
            # gam throws an error if nrow = 1
            list(formula = formula)
        }else{
            mgcv::gam(formula, data = data, fit = FALSE)
        }
    }, simplify = FALSE)
}

#==============================================================================#

make_model_frames = function(model, traps, mask, n_occasions, sessioncov = NULL, timecov = NULL, sessions = NULL, submodels = NULL, debug = FALSE){
    # check inputs ----------------------------------------------------------- #
    if(!ms(traps)) stop("only works with multi-session traps")
    if(!ms(mask)) stop("only works with multi-session masks")
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
    # make model frames ------------------------------------------------------ #
    model.frames = sapply(sessions, function(session){
        temptraps = traps[[session]]
        tempmask  = subset(mask, session)
        S         = n_occasions[session]
        K         = nrow(temptraps)
        M         = size(tempmask)
        temptimecov = timecov[[session]]
        tempsessioncov = if(is.null(sessioncov)) NULL else{
            sessioncov[session, , drop = FALSE]
        }
        # trapcov ------------------------------------------------------------ #
        trapcov = trapcov(temptraps)
        # remove any timevaryingcovs
        # - these will be added in modified form in the next step
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
        # timevaryingcov ----------------------------------------------------- #
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
        # maskcov ------------------------------------------------------------ #
        maskcov = covariates(tempmask[[1]])
        # add sessioncov
        if(!is.null(tempsessioncov)){
            maskcov = cbind(
                maskcov,
                tempsessioncov[rep(1, M), , drop = FALSE]
            )
        }
        # timecov ------------------------------------------------------------ #
        timecov = if(is.null(temptimecov)){
            data.frame(row.names = 1:S)
        }else{
            temptimecov
        }
        # add sessioncov
        if(!is.null(sessioncov)){
            timecov = if(is.null(timecov)){
                tempsessioncov[rep(1, S), , drop = FALSE]
            }else{
                cbind(timecov, tempsessioncov[rep(1, S), , drop = FALSE])
            }
        }
        # get model frame for each submodel ---------------------------------- #
        sapply(submodels, function(submodel){ # submodel = "pcall"
            switch(submodel,
                   "D"     = maskcov,
                   "pcall" = timecov,
                   timevaryingcov
            )[, all.vars(model[[submodel]]), drop = FALSE]
        }, simplify = FALSE)
    }, simplify = FALSE)
    return(model.frames)
}

#==============================================================================#

# extract the design matrix component from the smooth.setup object
# need to identify which rows of X correspond to which session

make_design_matrices_old = function(smooth.setup, model.frames){
    # count the number of rows per session for each submodel model frame
    rows = sapply(names(smooth.setup), function(submodel){
        # get number of rows per model frame
        nrows = sapply(model.frames, function(x) nrow(x[[submodel]]))
        # convert to row numbers in smooth.setup X
        rows = sapply(nrows, function(i) 1:i, simplify = FALSE)
        if(length(rows) > 1)
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

#==============================================================================#

make_design_matrices = function(model, model.frames, smooth.setup, sessions = NULL, submodels = NULL){
    if(is.null(sessions)) sessions = names(model.frames)
    if(is.null(submodels)) submodels = names(model.frames[[1]])
    design.matrices = sapply(sessions, function(session){
        sapply(submodels, function(submodel){
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
    return(design.matrices)
}

#==============================================================================#

# called by make_design_matrices and predict_submodel
# if no smooth.setup then uses model.matrix
# if smooth.setup is provided then this is used to pursuade mgcv::predict.gam to
# provide a design matrix using an existing gam model and new covariate data

#' @importFrom stats model.matrix
make_model_matrix = function(data, smooth.setup){
    formula = smooth.setup$formula
    response = all.vars(formula)[1]
    # add response variable to data
    if(is.null(data[[response]])) data[[response]] = 1
    if(is.null(smooth.setup$terms)){
        X = model.matrix(formula, data)
        colnames(X) = "(Intercept)"
    }else{
        # get design matrix from predict.gam
        class(smooth.setup) = "gam"
        smooth.setup$coefficients = rep(NA, ncol(smooth.setup$X))
        X = mgcv::predict.gam(smooth.setup, newdata = data, type = 'lpmatrix')
        colnames(X) = smooth.setup$term.names
    }
    return(X)
}

#==============================================================================#

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

# detection functions ==========================================================

# half normal
hn = function(x, theta, ...){
    theta[1] * exp(-0.5 * x * x / theta[2] / theta[2])
}

# hazard rate
hr = function(x, theta, ...){
    theta[1] * ( 1 - exp(-(x / theta[2])^(-theta[3])) )
}

#==============================================================================#

# works in side the gfit function
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

make_parindx = function(beta.names){
    submodel.names = c("D", "g0", "sigma", "z", "pcall", "bearings", "distances")
    parindx = sapply(submodel.names, function(submodel){
        matches = grepl(paste0("^", submodel, "\\."), beta.names)
        if(any(matches)) return(which(matches)) else return(NULL)
    }, simplify = FALSE)
    parindx = parindx[!sapply(parindx, is.null)]
    return(parindx)
}

make_parlist = function(beta, parindx, fixed){
    c(lapply(parindx, function(x){
        beta[x]
    }), fixed)
}

#==============================================================================#

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

#==============================================================================#

# prepare data into a format that can be read by the negloglik wrapper function

prepare_data = function(capthist, model.options, locations = FALSE){
    captures  = get_captures(capthist)
    bearings  = get_bearings(capthist)
    distances = get_distances(capthist)
    data = lapply(session(capthist), function(i){
        list(capthist  = captures[[i]],
             bearings  = bearings[[i]],
             distances = distances[[i]])
    })
    return(data)
}

#==============================================================================#

# calculate distance and bearings to all mask points from all traps

prepare_mask_info = function(mask, capthist, model.options){
    bearings  = calc_bearings(traps(capthist), mask)
    distances = calc_distances(traps(capthist), mask)
    mask.info = sapply(session(capthist), function(i){
        list(bearings  = bearings[[i]],
             distances = distances[[i]])
    }, simplify = FALSE)
    return(mask.info)
}

