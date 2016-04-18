
# main function ----------------------------------------------------------------

# @title Delta method
# @description Function to estimate the variance of nonlinear functions of normal variables
# @details Uses the \link[stats]{numericDeriv} function.
# @param f Function of vector beta
# @param beta Parameter vector
# @param vcov Variance-covariance matrix for beta
# @param ... additional arguments to pass to \code{f}
# @author Darren Kidney \email{darrenkidney@@googlemail.com}
# @examples
# \dontrun{
# # Example:
# if(0){
# hn = function(beta, r){
#    g0 <- boot::inv.logit(beta[1])
#    sigma <- exp(beta[2])
#    g0 * exp(-r^2/2/sigma^2)
# }
# beta = c(boot::logit(0.5), log(1000))
# vcov = matrix(c(4.337749e-02,-1.869121e-03,-1.869121e-03,7.368978e-04), 2, 2) ; vcov
# r = seq(0, 3000, length = 25)
# delta = delta_method(hn, beta, vcov, r = r)
# plot(r, hn(beta, r), type = "l", xlim = c(0,3000), ylim = c(0,1))
# lines(r, delta$est, col = 2, lwd = 2)
# lines(r, delta$est + 1.96 * delta$se, lty = 2, col = 2, lwd = 2)
# lines(r, delta$est - 1.96 * delta$se, lty = 2, col = 2, lwd = 2)
# }
# }
# @export

# assumes that f is a function of beta vector
# makes a new function g which is the same as f but uses separate beta arguments
delta_method = function(f, beta, vcov, ...){
    # checks ----------------------------------------------------------------- #
    if(!"beta" %in% names(formals(f)))
        stop("f must be a function of vector 'beta'")
    if("betanames" %in% names(formals(f)))
        stop("f can't be a function of 'betanames'")
    # g args ----------------------------------------------------------------- #
    dots = list(...)
    # expand beta vector to a series of individual betas
    beta.args = paste("beta", 1:length(beta), sep = "")
    # add betanames and dots
    all.args = c(beta.args, "betanames")
    if(length(dots) > 0) all.args = c(all.args, names(dots))
    # collapse
    all.args.collapsed = paste(all.args, collapse = ", ") # args
    # evaluate function args within the current environment ------------------ #
    thisenv = environment()
    betanames = names(beta)
    for(i in 1:length(beta))
        eval(parse(text = paste0(beta.args[i]," = beta[",i,"]")))
    # checks
    if(!all(sapply(beta.args, exists, envir = thisenv)))
        stop("beta objects don't exist")
    if(length(dots) > 0){
        for(i in 1:length(dots))
            eval(parse(text = paste0(names(dots)[i], " = dots[[i]]")))
        if(!all(sapply(names(dots), exists, envir = thisenv)))
            stop("... objects don't exist")
    }
    # define function g ------------------------------------------------------ #
    # extract body from function f
    body = paste(as.character(body(f)), collapse = "\n")
    # remove leading/trailing braces/returns
    for(i in c("^\\{","^\n","\\}$","\n$"))
        body = gsub(i, "", body)
    # add new line reconstructing beta vector
    newline = paste0("beta = setNames(c(", paste(beta.args, collapse = ", "),
                     "), betanames)\n")
    body = paste0(newline, body)
    g = eval(parse(text = paste0("function(", all.args.collapsed, "){\n", body,
                                 "\n}")))
    # define a call expression for g ----------------------------------------- #
    calltext = paste0("g(", all.args.collapsed,")") # calltext
    expr = quote(eval(parse(text = calltext))) # expr
    # estimates and variances ------------------------------------------------ #
    est  = stats::numericDeriv(expr = expr, theta = beta.args, rho = thisenv)
    grad = attr(est, "gradient") # grad
    var  = sapply(1:nrow(grad), function(i) t(grad[i, ]) %*% vcov %*% grad[i, ])
    est  = as.numeric(est)
    list(estimate = est, se = sqrt(var))
}


# wrapper for linear models ----------------------------------------------------

# @title Delta method for linear models
# @description Function to estimate confidence intervals for functions of estimated parameters
# @details This is a wrapper function for \link[gibbonsecr]{delta_method}
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

# wrapper for delta_method for linear models
delta_method_Xbeta = function(X, beta, vcov){
    if(length(dim(X)) != 2)
        stop("X must be a matrix")
    if(length(beta) != ncol(X))
        stop("length(beta) must equal ncol(X)")
    if(!all(dim(vcov) == rep(ncol(X), 2)))
        stop("nrow(vcov) and ncol(vcov) must equal ncol(X)")
    npars = ncol(X)
    f.body = paste0("as.numeric(X %*% c(", paste("beta[", 1:npars, "]", sep = "",
                                                 collapse = ", "), "))")
    f.text = paste0("function(beta){", f.body, "}")
    f = eval(parse(text = f.text))
    delta_method(f, beta, vcov, X = X)
}
