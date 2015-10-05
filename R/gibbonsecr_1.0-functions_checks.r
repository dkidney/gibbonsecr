
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# checks the 'capthist' argument in the fit_gibbonsecr function

check_capthist = function(capthist){

    ##################################################
    ## check input

    # check class and detector type
    if(!inherits(capthist, "capthist"))
        stop("requires 'capthist' object", call. = FALSE)
    if(detector(traps(capthist)) != "proximity")
        stop("requires proximity detectors", call. = FALSE)
    # convert to multi-session
    # timecov = timecov(capthist)
    capthist = MS(capthist)

    ##################################################
    ## trapcov

    # add scaled version of x and y to covariates
    meanSD = traps_meanSD(traps(capthist))
    for(session in session(capthist)){
        coords = as.data.frame(mapply(function(x, y, z) ((x - y) / z),
                        x = traps(capthist[[session]]),
                        y = meanSD[1,],
                        z = meanSD[2,]))
        rownames(coords) = rownames(traps(capthist[[session]]))
        if(is.null(covariates(traps(capthist[[session]])))){
            covariates(traps(capthist[[session]])) = coords
        }else{
            covariates(traps(capthist[[session]])) = cbind(
                covariates(traps(capthist[[session]])),
                coords
            )
        }
    }

    ##################################################
    ## sessioncov

    sessioncov = sessioncov(capthist)
    if(!is.null(sessioncov)){
        if(!all(rownames(sessioncov) == session(capthist)))
            stop("!all(rownames(sessioncov) == session(capthist))", call. = FALSE)
    }

    ##################################################
    ## usage - if missing then assume full usage

    if(is.null(usage(traps(capthist)))){
        for(session in session(capthist)){
            usage(traps(capthist[[session]])) = matrix(
                1,
                nrow = n_groups(capthist[[session]]),
                ncol = n_occasions(capthist[[session]])
            )
        }
    }

    ##################################################
    ## bearings - if present, convert to radians

    if(!is.null(get_bearings(capthist))){
        for(session in session(capthist)){ # session = session(capthist)[1] ; session
            bearings = get_bearings(capthist[[session]])
            if(!is.null(bearings)){
                if(attr(bearings, "details")$units == "degrees"){
                    capthist[[session]] = add_bearings(capthist[[session]], bearings * pi / 180)
                }
            }
        }
    }

    ##################################################
    ## distances - if present, convert to metres

    if(!is.null(get_distances(capthist))){
        for(session in session(capthist)){ # session = session(capthist)[1] ; session
            distances = get_distances(capthist[[session]])
            if(!is.null(distances)){
                if(attr(distances, "details")$units == "km"){
                    capthist[[session]] = add_distances(capthist[[session]], distances * 1000)
                }
            }
        }
    }

    # attr(capthist, "sessioncov") = sessioncov
    # attr(capthist, "timecov") = timecov
    return(capthist)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

check_covariates = function(covariates, capthist){
    message("checking covariates data...")

    ##################################################
    ## check column names

    colnames(covariates) = tolower(colnames(covariates))
    obligatory = c("array", "post")
    check_names(covariates, obligatory, must.contain.all = FALSE)

    ##################################################
    ## check occasion column

    # if no occasion column then add one
    if(!any(colnames(covariates) == "occasion")){
        if(all(n_occasions(capthist) == 1)){
            covariates$occasion = 1
        }else{
            message("- adding occasion column (assuming covariates constant across occasions)")
            covariates = do.call(rbind, sapply(session(capthist), function(i){ # i = session(capthist)[1] ; i
                merge(
                    covariates[covariates$array == i,],
                    data.frame(occasion = 1:n_occasions(capthist[[i]]))
                )
            }, simplify = FALSE))
            rownames(covariates) = NULL
        }
    }

    ##################################################
    ## delete covariates for arrays with no captures

    i = !covariates$array %in% session(capthist)
    if(any(i)){
        message("- deleting covariate data for arrays with no detections: ", paste(unique(covariates$array[i]), collapse = ", "))
        covariates = covariates[!i, , drop = FALSE]
    }
    # check all arrays with data are present in covariates
    i = !session(capthist) %in% covariates$array
    if(any(i))
        stop("covariate data missing for the following arrays: ", paste(session(capthist)[i], collapse = ", "), call. = FALSE)
    # index of rows in covariates (array-post-occasion)
    actual = paste(covariates[["array"]], covariates[["post"]], covariates[["occasion"]], sep = "-")
    # check for duplicated rows
    i = duplicated(actual)
    if(any(i))
        stop("the following array-post-occasion combinations have more than one row in the covariates data: ", paste(unique(actual[i]), collapse = ", "), call. = FALSE)
    # index of rows that should be present (array-post-occasion)
    # need to take account of trap usage
    in.theory = apply(do.call(rbind, lapply(session(capthist), function(i){ # i = session(capthist)[1] ; i

        expand.grid(
            array = session(capthist[[i]]),
            post = rownames(traps(capthist[[i]])),
            occasion = 1:n_occasions(capthist[[i]]),
            stringsAsFactors = FALSE
        )[as.numeric(usage(traps(capthist[[i]]))) == 1,]

    })), 1, paste, collapse = "-")
    # check that all rows that should be there are actually there
    i = !in.theory %in% actual
    if(any(i))
        stop("the following array-post-occasion combinations have no covariates data: ", paste(in.theory[i], collapse = ", "), call. = FALSE)
    # check for any rows that shouldn't be there
    i = !actual %in% in.theory
    if(any(i)){
        message("- deleting array-post-occasion combinations with no detections data: ", paste(actual[i,], sep = ", "))
        covariates = covariates[!i,]
    }
    # check for NAs in essential columns
    j = colnames(covariates) %in% c("array", "post", "occasion")
    i = apply(covariates[,j], 2, function(x) any(is.na(x)))
    if(any(i))
        stop("the following compuldory columns in the covariates data contain missing values: ", paste(colnames(covariates)[j], collapse = ", "), call. = FALSE)
    # check for NAs in non-essential columns
    i = apply(covariates[,!j], 2, function(x) any(is.na(x)))
    if(any(i)){
        message("deleting covariates containing missing values: ", paste(colnames(covariates)[!j][i], collapse = ", "))
        for(k in colnames(covariates)[!j][i]) covariates[,k] = NULL
    }
    # check column classes
    covariates$array = factor(covariates$array, levels = session(capthist))
    if(inherits(covariates$occasion, c("integer","numeric"))){
        covariates$occasion = as.integer(covariates$occasion)
    }else{
        stop("'occasion' column in covariates data must be numeric", call. = FALSE)
    }
    for(cov in colnames(covariates)){ # cov = "habitat"
        if(is.character(covariates[[cov]]))
            covariates[[cov]] = as.factor(covariates[[cov]])
    }
    # sort by array, post, occasion
    covariates = covariates[order(covariates[["array"]], covariates[["post"]], covariates[["occasion"]]),]
    return(covariates)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# checks the 'details' argument in the import_data function

check_details = function(details = list()){
    # bearings
    if(is.null(details$bearings)) details$bearings = list()
    default = list(units = "degrees", type = "continuous")
    details$bearings = replace(default, names(details$bearings), details$bearings)
    if(!details$bearings$units %in% c("degrees", "radians"))
        stop("bearings units must be 'degrees' or 'radians'", call. = FALSE)
    if(!details$bearings$type %in% c("continuous", "interval"))
        stop("bearings type must be 'degrees' or 'interval'", call. = FALSE)
    # distances
    if(is.null(details$distances)) details$distances = list()
    default = list(units = "km", type = "continuous")
    details$distances = replace(default, names(details$distances), details$distances)
    if(!details$distances$units %in% c("km", "m"))
        stop("distances units must be 'km' or 'm'", call. = FALSE)
    if(!details$distances$type %in% c("continuous", "interval"))
        stop("distances type must be 'continuous' or 'interval'", call. = FALSE)
    return(details)
}


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# checks data read from the detections file within the import_data function

check_detections = function(detections, details){
    message("checking detections data...")
    # check columns
    colnames(detections) = tolower(colnames(detections))
    if(!all(c("array", "occasion", "post", "group") %in% colnames(detections))) stop("Detections data must contain the following columns: 'array', 'occasion', 'post', 'group' ('bearing' and 'distance' columns are optional)", call. = FALSE)
    # check bearings
    if(!is.null(detections[["bearing"]])){
        bearings = detections[["bearing"]][!is.na(detections[["bearing"]])]
        # convert to radians
        if(details$bearings$units == "degrees")
            bearings = bearings * pi / 180
        # check range
        if(min(bearings < 0) || max(bearings) > (2*pi))
            stop("check the range of bearings estimates", call. = FALSE)
    }
    # check distances
    if(!is.null(detections[["distance"]])){
        distances = detections[["distance"]][!is.na(detections[["distance"]])]
        # convert to metres
        if(details$distances$units == "km")
            distances = distances * 1000
        # check range
        if(min(distances) < 0)
            stop("estimated distances must be positive", call. = FALSE)
    }

    ##################################################
    ## double-counted groups

    # index of unique array-post-occasion combinations
    index = unique(detections[,c("array", "post", "occasion")])
    duplicate.counts = sapply(1:nrow(index), function(i){ # i=1
        rows = with(detections, {
            which(array == index$array[i] & post == index$post[i] & occasion == index$occasion[i])
        })
        groups = detections$group[rows]
        dups = duplicated(groups)
        if(any(dups)) return(sort(as.character(unique(groups[dups])))) else return(NULL)
    })

    # print errors
    errors = !sapply(duplicate.counts, is.null)
    if(any(errors)){
        error.message = "Double counted groups:\n"
        for(i in which(errors)){ # i=which(errors)[1] ; i
            error.message = c(error.message, paste("array ", index$array[i], ", post ", index$post[i], ", occasion ", index$occasion[i], ", groups: ", paste(duplicate.counts[[i]], collapse = ", "), "\n", sep = ""))
        }
        stop(error.message, call. = FALSE)
    }

    ##################################################
    ## column classes

    detections$array = as.character(detections$array)
    detections$post  = as.character(detections$post)
    detections$group = as.character(detections$group)
    if(inherits(detections$occasion, c("integer","numeric"))){
        detections$occasion = as.integer(detections$occasion)
    }else{
        stop("'occasion' column in detections data must be numeric", call. = FALSE)
    }
    return(detections)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

check_fixed = function(fixed, model.options, capthist, locations = FALSE){
    # convert to list of numeric values
    fixed = lapply(fixed, as.numeric)
    # check names
    check_names(fixed, c('D', 'g0', 'sigma', 'z', 'pcall', 'bearings', 'distances'))
    # if single occasion for all arrays:
    # - g0 should be fixed - warn if it isn't fixed at one
    # - pcall can't be estimated, so fix at one if no user value supplied
    if(all(n_occasions(capthist) == 1)){
        if(!"g0" %in% names(fixed)){
            fixed[["g0"]] = 1
            message("single survey occasion so fixed: g0 = 1")
        }else{
            if(fixed[["g0"]] != 1)
                warning("** single survey gibbon surveys should have g0 fixed at 1", call. = FALSE)
        }
        if(!"pcall" %in% names(fixed)){
            fixed[["pcall"]] = 1
            message("single survey occasion so fixed: pcall = 1")
        }
    }
    # if halfnormal detection function then dont need z
    if(model.options[["detectfn"]] == 0 && "z" %in% names(fixed)){
        fixed[["z"]] = NULL
        message("no need for z parameter since model.options[['detectfn']] = 0 (ie. half normal)")
    }
    # if no bearings/distances data then dont need fixed bearings/distances value
    for(i in c("bearings", "distances")){
        if(model.options[[i]] == 0){
            if(!is.null(fixed[[i]])){
                fixed[[i]] = NULL
                message("no need for fixed ",i," parameter (no ",i," model)")
            }
        }
    }
    #     if(locations){
    #         for(i in c("bearings", "distances", "pcall")){ # i = "bearings"
    #             if(!is.null(fixed[[i]])){
    #                 fixed[[i]] = NULL
    #                 message("locations = TRUE, so fixed$", i, " set to NULL")
    #             }
    #         }
    #     }

    # check range of fixed values
    for(i in names(fixed)){
        if(i %in% c("g0","pcall") || (i == "bearings" && model.options$bearings == 2)){
            if(fixed[[i]] <= 0 || fixed[[i]] > 1)
                stop("fixed value for ", i, " must be greater than 0 and less than or equal to 1", call. = FALSE)
        }else{
            if(fixed[[i]] <= 0)
                stop("fixed value for ", i, " must be positive", call. = FALSE)
        }
    }
    return(fixed)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

check_mask = function(mask, capthist, mask.options = list()){

    ##################################################
    ## make a mask if none supplied

    if(is.null(mask)){
        if(is.null(mask.options$buffer)){
            mask.options$buffer = 5000
            message("using default buffer width 5000 m")
        }
        if(is.null(mask.options$spacing)){
            mask.options$spacing = 250
            message("using default mask spacing 250 m")
        }
        mask = make.mask(traps   = traps(capthist),
                         buffer  = mask.options$buffer,
                         spacing = mask.options$spacing,
                         type    = "trapbuffer")
    }

    ##################################################
    ## check session names

    if(!inherits(mask, "mask"))
        stop("requires 'mask' object", call. = FALSE)
    # mask = MS(mask)
    # if more than one session then check that session names agree with capthist
    if(length(mask) > 1){
        # delete mask sessions that don't appear in capthist
        missing = !session(mask) %in% session(capthist)
        if(any(missing)){
            message("the following sessions are present in mask but not in capthist and will be deleted from mask: ", paste(session(mask)[missing], collapse = ", "))
            mask = subset_mask(mask, !missing)
        }
        # check for any capthist sessions that don't appear in mask
        missing = !session(capthist) %in% session(mask)
        if(any(missing))
            stop("the following sessions are present in capthist but not in mask; ", paste(session(capthist)[missing], collapse = ", "), call. = FALSE)
    }

    ##################################################
    ## remove mask points with missing covariates

    if(!is.null(covariates(mask[[1]]))){
        # if(is.null(mask.options$remove.missing))
            # mask.options$remove.missing = TRUE
        # if(mask.options$remove.missing){
            missing = sapply(covariates(mask), function(x){
                any(apply(x, 1, function(x){
                    any(is.na(x))
                }))
            })
            if(any(missing)){
                message("removing mask points with missing covariate values")
                for(session in session(mask)){ # session = "2"
                    keep = !apply(covariates(mask[[session]]), 1, function(x){
                        any(is.na(x))
                    })
                    mask[[session]] = subset_mask(mask[[session]], keep)
                }
            }
        # }
    }

    ##################################################
    ## add scaled x y covariates

    # add scaled version of x and y to covariates
    meanSD = traps_meanSD(traps(capthist))
    for(session in session(capthist)){
        coords = as.data.frame(mapply(function(x, y, z) ((x - y) / z),
                        x = mask[[session]],
                        y = meanSD[1,],
                        z = meanSD[2,]))
        rownames(coords) = rownames(mask[[session]])
        if(is.null(covariates(mask[[session]]))){
            covariates(mask[[session]]) = coords
        }else{
            covariates(mask[[session]]) = cbind(
                covariates(mask[[session]]),
                coords
            )
        }
    }

    return(mask)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

check_model = function(model, fixed, model.options, capthist, mask, locations = FALSE){

    ##################################################
    ## standardise format of model formulae

    if(!inherits(model, "list")) model = list(model)
    model = lapply(model, as.formula)
    model.components = lapply(model, as.character)
    submodel.names = if(is.null(names(model))){
        sapply(model.components, function(x) if(length(x) == 2) "" else x[2])
    }else names(model)
    model = lapply(model, function(x) if(length(x) == 3) as.formula(paste(x[c(1, 3)])) else x)
    names(model) = submodel.names

    ##################################################
    ## check submodel names

    submodels = c("D", "g0", "sigma", "z", "pcall", "bearings", "distances")
    check_names(model, submodels)
    # must have D, g0 and sigma model
    for(submodel in c("D", "g0", "sigma")){ # submodel = "sigma"
        if(is.null(model[[submodel]])){
            model[[submodel]] = ~1
            message("using default formula: ", submodel, " ~ 1")
        }
    }

    ##################################################
    # z

    if(!is.null(model[['z']]) && model.options$detectfn == 0){
        model[['z']] = NULL
        message("formula for z ignored as model.options[['detectfn']] = 0")
    }
    if(is.null(model[['z']]) && model.options[['detectfn']] == 1){
        model[['z']] = ~1
        message("using default formula: z ~ 1")
    }

    ##################################################
    ## bearings / distances

    for(submodel in c("bearings","distances")){
        if(!is.null(model[[submodel]]) && model.options[[submodel]] == 0){
            model[[submodel]] = NULL
            message("formula for ", submodel, " ignored as model.options[['", submodel, "']] = 0")
        }
        if(is.null(model[[submodel]]) && model.options[[submodel]] != 0){
            model[[submodel]] = ~1
            message("using default formula: ", submodel, " ~ 1")
        }
    }

    #     # use default model for pcall if locations unknown
    #     if(!is.null(model$pcall) && locations){
    #         model$pcall = NULL
    #         message("formula for pcall ignored as locations = TRUE")
    #     }
    #     if(is.null(model$pcall) && !locations){
    #         model$pcall = ~1
    #         message("using default formula: pcall ~ 1")
    #     }

    ##################################################
    ## pcall

    if(is.null(model$pcall)){
        model$pcall = ~1
        message("using default formula: pcall ~ 1")
    }

    ##################################################
    ## if submodel parameter is fixed, then check that model is ~1

    for(submodel in names(model)){ # i = "D"
        if(model[[submodel]] != ~ 1){
            if(!is.null(fixed[[submodel]])){
                model[[submodel]] = ~ 1
                message("formula for ", submodel, " changed to ~ 1 as parameter values has been fixed")
            }
        }
    }

    ##################################################
    # check formula syntax and covariate names

    covlevels = c(covlevels(capthist), covlevels(mask))
    for(submodel in names(model)){ # submodel = "D"
        # check for as.numeric and as.factor
        terms = attr(terms(model[[submodel]]), "term.labels")
        notallowed = grepl("^as.numeric\\(", terms) |
            grepl("^as.factor\\(", terms) |
            grepl("^as.character\\(", terms)
        if(any(notallowed))
            stop("formulas containing 'as.numeric', 'as.factor', etc. not currently implemented", call. = FALSE)
        # check for covs that don't exists
        covs = all.vars(model[[submodel]])
        allcovs = unique(unname(unlist(covlevels)))
        i = !covs %in% allcovs
        if(any(i)){
            stop("covariates in ", submodel, " formula not recognised: ", paste(covs[i], collapse = ", "), call. = FALSE)
        }
        # check for covs that aren't allowed
        allowed = unique(unname(unlist(covlevels[switch(
            submodel,
            "D"     = c("sessioncov","maskcov"),
            "pcall" = c("sessioncov","timecov"),
            c("sessioncov","timecov","trapcov","timevaryingcov","maskcov")
        )]
        )))
        i = !covs %in% allowed
        if(any(i)){
            stop("covariates in ", submodel, " formula not allowed: ", paste(covs[i], collapse = ", "), call. = FALSE)
        }
    }
    # can't have all pars fixed
    if(all(names(model) %in% names(fixed))) stop("all parameters fixed", call. = FALSE)
    # sort
    model = model[submodels]
    model = model[!sapply(model, is.null)]
    return(model)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

check_model_options = function(model.options = list(), capthist, locations = FALSE){

    # convert to list of integers
    model.options = lapply(model.options, as.integer)

    # check names
    check_names(model.options, c("detectfn", "bearings", "distances"))

    # use defaults for missing options
    default.model.options = list(detectfn = 0, bearings = 1, distances = 0)
    model.options = replace(default.model.options, names(model.options), model.options)

    #     # known locations
    #     if(locations){
    #         for(i in c("bearings", "distances")){ # i = "bearings"
    #             if(model.options[[i]] != 0){
    #                 model.options[[i]] = 0
    #                 message("locations = TRUE, so model.options$", i, " set to 0")
    #             }
    #         }
    #     }

    # check values
    if(!model.options$detectfn %in% 0:1)
        stop("model.options[['detectfn']] must be 0 (half normal) or 1 (hazard rate)", call. = FALSE)
    if(!model.options$bearings %in% 0:2)
        stop("model.options[['bearings']] must be 0 (no bearings model), 1 (von Mises) or 2 (wrapped Cauchy)", call. = FALSE)
    if(!model.options$distances %in% 0:2)
        stop("'model.options[['distances']] must be 0 (no distances model), 1 (gamma) or 2 (lognormal)", call. = FALSE)

    # check against capthist
    if(!ms(capthist)) capthist = list(capthist)
    # if bearings/distances is not zero, check bearings/distances data exists
    for(submodel in c("bearings", "distances")){ # i = "bearings"
        data.present = any(sapply(capthist, function(x) !is.null(attr(x, submodel))))
        if(model.options[[submodel]] != 0 && !data.present){
            model.options[[submodel]] = 0
            message("no estimated ", submodel, ", so model.options['", submodel, "']] set to 0")
        }
    }

    return(model.options)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# checks that a character string contains expected names

check_names = function(x, allowed, must.contain.all = TRUE){
    # check for missing names
    missing = length(names(x)) != length(x) || any(names(x) %in% "")
    if(any(missing)){
        stop("some names missing from ", paste(substitute(x)), " \nchoose from: '", paste(allowed, collapse = "', '"), "'", call. = FALSE)
    }
    # check for inadmissable names
    if(must.contain.all){
        wrong = !names(x) %in% allowed
        if(any(wrong)){
            stop("the following names for ", paste(substitute(x)), " are not recognised: '", paste(names(x)[wrong], collapse = "', '"), "' \nchoose from: '", paste(allowed, collapse = "', '"), "'", call. = FALSE)
        }
    }
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

check_shp = function(poly, capthist){
    if(!inherits(capthist, "capthist")) stop("capthist object required")
    regiontraps = if(ms(traps(capthist))){
        do.call(rbind, lapply(traps(capthist), as.data.frame))
    }else{
        traps(capthist)
    }
    inside = pointsInPolygon(regiontraps, poly)
    if(any(!inside))
        warning("some traps are outside the polygon - check units")
    invisible()
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# checks data read from the posts file within the import_data function

check_posts = function(posts, detections, details){
    message("checking posts data...")
    # check all essential column names exist
    colnames(posts) = tolower(colnames(posts))
    if(!all(c("array", "post", "x", "y", "usage") %in% colnames(posts)))
        stop("Posts data must contain the following columns: 'array', 'post', 'x', 'y' and 'usage'", call. = FALSE)
    # check all array names in detections data appear in posts data
    if(!all(detections[["array"]] %in% posts[["array"]]))
        stop("some array names in detections data not present in posts data", call. = FALSE)
    # check all posts names in detections data appear in posts data
    if(!all(detections[["post"]] %in% posts[["post"]]))
        stop("some post names in detections data not present in posts data", call. = FALSE)
    # delete posts in arrays without any detections
    # index = posts$array %in% detections$array
    # if(any(!index)){
        # message("- deleting posts for arrays with no detections: ", paste(unique(posts$array[!index]), collapse = ", "))
        # posts = posts[index,]
    # }
    # column classes
    posts$array = as.character(posts$array)
    posts$post  = as.character(posts$post)
    posts$usage = as.character(posts$usage)
    # check usage...
    return(posts)
}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# this needs some work
# allow some start values to be fixed

check_start_values = function(start, capthist, mask, model.options, fixed, S, K, M, a, usage, design.matrices, par.labels, parindx, inv.link, mask.info, CV = 0.3){

    # start = NULL
    # start = c(D = log(0.5))

    ##################################################
    ## check length and names

    default.start = setNames(rep(NA, nrow(par.labels)), par.labels[,"unique"])
    if(is.null(start)){
        start = default.start
    }else{
        start = unlist(start)
        if(is.null(names(start))){
            if(length(start) != nrow(par.labels)){
                stop("if start is an unnamed vector then it must contain values for all coefficients", call. = FALSE)
            }else{
                names(start) = par.labels[,"unique"]
            }
        }
        for(i in unique(par.labels[,"submodel"])){
            names(start)[names(start) == i] = paste0(i, ".(Intercept)")
        }
        start = replace(default.start, names(start), start)
    }

    ##################################################
    ## non-density pars

    for(submodel in unique(par.labels[,"submodel"])){ # submodel = "sigma"
        if(submodel == "D") next
        i = which(par.labels[,"submodel"] == submodel)
        if(is.na(start[i[1]])){
            start[i[1]] = switch(
                submodel,
                "g0"        = logit(0.5),
                "sigma"     = log(750),
                "z"         = log(3),
                "pcall"     = logit(0.5),
                "bearings"  = switch(model.options$bearings,
                                     log(10),
                                     logit(0.8)),
                "distances" = switch(model.options$distances,
                                     log(cv_to_pdfpar(CV, "gamma")),
                                     log(cv_to_pdfpar(CV, "lnorm")))
            )
        }
    }

    ##################################################
    ## density pars

    if(any(par.labels[,"submodel"] == "D")){
        start[is.na(start) & names(start) != "D.(Intercept)"] = 0
        if(any(names(start) == "D.(Intercept)" & is.na(start))){
            esa = calc_esa(
                detectfn        = model.options$detectfn,
                beta            = start,
                parindx         = parindx,
                fixed           = fixed,
                design.matrices = design.matrices,
                distances       = sapply(mask.info, function(x) x[["distances"]], simplify = FALSE),
                usage           = usage,
                inv.link        = inv.link,
                S               = S,
                K               = K,
                M               = M,
                a               = a
            )
            ngroups = sum(n_groups(capthist))
            i = which(par.labels[,"submodel"] == "D")[1]
            start[i] = log(ngroups / sum(esa))
        }
    }

    # esa = calc_esa(capthist, mask, model.options, detectpar)

    # calc_esa(model.options$detectfn, beta, par.labels, fixed, design.matrices, distances, usage, inv.link, S, K, M, a)


    #         start = do.call(c, lapply(1:nrow(par.labels), function(i){ # i=1
    #
    #             if(grepl("Intercept", par.labels[i,"term"])){
    #
    #                 switch(par.labels[i, "submodel"],
    #
    #                        "D"         = log(ngroups / esa),
    #                        "g0"        = logit(detectpar$g0),
    #                        "sigma"     = log(detectpar$sigma),
    #                        "z"         = log(detectpar$z),
    #                        "bearings"  = switch(model.options$bearings, log(70), logit(0.95)),
    #                        "distances" = switch(model.options$distances, log(CV^(-2)), log(sqrt(log(1+CV^2)))),
    #                        "pcall"     = logit(0.5)
    #
    #                 )
    #
    #             }else{
    #
    #                 0
    #
    #             }
    #
    #         }))

    # start = start[names(start) %in% names(fixed)]

    return(start)

}

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# four types of covariates used in secr package
# 1. sessioncov     - constant for all traps and occasions within the session
#                     (e.g. array, possibly habitat and season)
# 2. timecov        - constant for all traps within a given occasion and session
#                     (e.g. array, season, possibly temperature)
# 3. trapcov        - constant for all occasions within a given post and session
#                     (e.g. array, habitat)
# 4. timevaryingcov - can vary between occasions and posts
#                     (e.g. observer id, observer experience, temperature)

# get_covariates_by_level = function(covariates){
#
#     out = sapply(c("sessioncov", "timecov", "trapcov", "timevaryingcov"), function(level){ # level = "sessioncov"
#
#         # make an id variable to group covariate data
#         ids = with(covariates, switch(level,
#                                       "sessioncov"     = paste(array),
#                                       "timecov"        = paste(array, occasion, sep = "_"),
#                                       "trapcov"        = paste(array, post, sep = "_"),
#                                       "timevaryingcov" = paste(array, post, occasion, sep = "_")
#         ))
#
#         covnames = apply(do.call(rbind, sapply(unique(ids), function(id){ # id = session_id[1] ; id
#
#             apply(covariates[ids == id, , drop = FALSE], 2, function(x) all(x == x[1]))
#
#         }, simplify = FALSE)), 2, all)
#
#         covnames = colnames(covariates)[covnames]
#
#         # extract the relvant data from covariates
#         sub = covariates[!duplicated(ids), covnames]
#         rownames(sub) = NULL
#
#         return(sub)
#
#     }, simplify = FALSE)
#
#     return(out)
#
# }

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

