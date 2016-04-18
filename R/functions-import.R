
# survey data ==================================================================

#' @title Import survey data
#' @description The main arguments to this function are \code{detections},
#'   \code{posts} and (optionally) \code{covariates}. The values of these
#'   arguments can be either (i) a string giving the the file path to a
#'   \code{.csv} file, or (ii) a data frame (see Details for a full description
#'   of the required format in each case).
#'
#'   The function returns a \link[secr]{capthist} object that can be used
#'   as an input to the \link[gibbonsecr]{gfit} fitting function. The
#'   capthist object returned differs from the standard format used by the
#'   \pkg{secr} package in that arrays of estimated bearings and distances are
#'   attached as attributes (if present in the detections data).
#' @param detections recapture data and (optionally) estimated bearings and
#'   distances to detected groups
#' @param posts listening post coordinates (in metric units)
#' @param covariates covariate values (optional)
#' @param details list containing the type and units of the estimated bearings
#'   and distances (see Details)
#' @details The format of the \code{.csv} files (or data frames) and the
#'   \code{details} argument are described below:
#' \describe{
#'  \item{\strong{detections}}{This should contain one row per detection. For
#'  example, if group X was detected at array 1 on occasion 1 at posts A and B,
#'  and on occasion 2 at post C, then this would represent three detections.
#'  This data should contain the folowing columns:
#'   \tabular{ll}{
#'    \code{array}    \tab array ID \cr
#'    \code{occasion} \tab sampling occasion (must be an integer) \cr
#'    \code{post}     \tab listening post ID \cr
#'    \code{group}    \tab group ID \cr
#'    \code{bearing}  \tab estimated bearing (optional) \cr
#'    \code{distance} \tab estimated distance (optional) \cr
#'    \tab\cr\tab\cr
#'   }
#'  }
#'  \item{\strong{posts}}{This should contain one row per listening post and
#'  contain the folowing columns:
#'   \tabular{ll}{
#'    \code{array} \tab array ID \cr
#'    \code{post}  \tab listening post ID \cr
#'    \code{x}     \tab listening post x-coordinate (in metric units) \cr
#'    \code{y}     \tab listening post y-coordinate (in metric units) \cr
#'    \code{usage} \tab listening post usage (must be a string of 1s and 0s) \cr
#'   }
#'   The values in the usage column should reflect the number of occasions. For
#'   single-occasion surveys all entries in the usage column should be
#'   \code{"1"}. For multi-occasion surveys the length of each entry in the
#'   usage column should be equal to the number of occasions for that array. For
#'   example, in a three-occasion survey if post X was used on all occasions
#'   then the entry in the usage column should be \code{"111"}, but if it was
#'   used on occasions 1 and 3 but not on occasion 2 then the entry would be
#'   \code{"101"}.
#'  }
#'  \item{\strong{covariates}}{This should contain one row per listening
#'  post-occasion combination. For example, if there are 3 listening posts at
#'  array X and 2 sampling occasions, the covariates data should contain 6 rows
#'  for that array (irrespective of the number of detections). This data should
#'  contain the folowing columns:
#'   \tabular{ll}{
#'    \code{array}        \tab the array ID \cr
#'    \code{occasion}     \tab the sampling occasion (must be an integer) \cr
#'    \code{post}         \tab the listening post ID \cr
#'    \code{<covariate1>} \tab values for covariate 1 \cr
#'    \code{<covariate1>} \tab values for covariate 2 \cr
#'    \code{...}          \tab ... \cr
#'   }
#'   For advice on model specification using covariates see
#'   \link[gibbonsecr]{gibbonsecr-covariates}.
#'  }
#'  \item{\strong{details}}{ The value of the \code{details} argument should be
#'  a list. If there are no estimated bearings and no estimated distances then
#'  this should be an empty list (the default).
#'
#'  If the detections data contains estimated bearings, then one element of the
#'  \code{details} list should be named \code{"bearings"} and the value of this
#'  element should be a list with element names \code{"units"} and
#'  \code{"type"}. The \code{"units"} element can be set to either
#'  \code{"degrees"} (the default) or \code{"radians"}, and the \code{"type"}
#'  element should be set to \code{"continuous"} (currently the only available
#'  option). If bearings data exist and the \code{"bearings"} element is
#'  \code{NULL} then the default values are assumed.
#'
#'  If the detections data contains estimated bearings, then one element of the
#'  \code{details} list should be named \code{"distances"} and the value of this
#'  element should be a list with element names \code{"units"} and
#'  \code{"type"}. The \code{"units"} element can be set to either \code{"km"}
#'  (the default) or \code{"m"}, and the \code{"type"} element should be set to
#'  \code{"continuous"} (currently the only available option). If distances data
#'  exist and the \code{"distances"} element is \code{NULL} then the default
#'  values are assumed.
#'  }
#' }
#' @seealso \link[gibbonsecr]{plot.gcapthist}, \link[gibbonsecr]{import_shp}
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/example-import_data.r
#' @export

import_data = function(detections, posts, covariates = NULL, details = list()){
    message("importing survey data...")

    # details ---------------------------------------------------------------- #
    details = check_details(details)

    # detections and posts --------------------------------------------------- #
    # if not data.frame, assume file path to csv file
    if(!inherits(detections, "data.frame"))
        detections = read.csv(detections, stringsAsFactors = FALSE)
    if(!inherits(posts, "data.frame"))
        posts = read.csv(posts, stringsAsFactors = FALSE)
    detections = check_detections(detections, details)
    posts = check_posts(posts, detections, details)

    # capthist --------------------------------------------------------------- #
    # construct a multi-session capthist object
    array.names = unique(posts$array)
    capthist = sapply(array.names, function(array){ # array = array.names[2] ; array
        # traps
        i = posts$array == array
        traps = with(posts[i,], data.frame(x = x, y = y, row.names = post))
        traps = read.traps(data = traps, detector = "proximity")
        usage(traps) = do.call(rbind, lapply(strsplit(posts$usage[i], NULL),
                                             as.numeric))
        # captures
        i = detections$array == array
        captures = with(detections[i,], {
            data.frame(
                session  = array,
                ID       = group,
                occasion = occasion,
                detector = post
            )
        })
        # capthist
        if(nrow(captures) == 0){
            capthist = array(dim = c(0,ncol(usage(traps)),nrow(traps)),
                             dimnames = list(NULL,
                                             as.character(1:ncol(usage(traps))),
                                             rownames(traps)))
            class(capthist) = "capthist"
            session(capthist) = array
            traps(capthist) = traps
            covariates(capthist) = data.frame()
        }else{
            capthist = make.capthist(captures, traps,
                                     noccasions = ncol(usage(traps)))
            dimnames(capthist)[[3]] = rownames(traps)
        }

        return(capthist)
    }, simplify = FALSE)
    # convert to multi-session by default
    capthist = MS(capthist, session.names = array.names)
    class(capthist) = c("gcapthist", class(capthist))

    # bearings and distances ------------------------------------------------- #
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

    # covariates ------------------------------------------------------------- #
    if(is.null(covariates)){
        # make a set of null covariates
        occasions = n_occasions(capthist)
        covariates = do.call(rbind, lapply(session(capthist), function(session){
            expand.grid(
                array = session,
                post = rownames(traps(capthist[[session]])),
                occasion = 1:occasions[session],
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
    # covariates = dplyr::tbl_df(covariates)
    # run checks
    covariates = check_covariates(covariates, capthist)
    # add covariates to capthist
    for(level in c("sessioncov", "timecov", "trapcov", "timevaryingcov")){
        # level = "sessioncov"
        # level = "sessioncov"
        # level = "sessioncov"
        # level = "sessioncov"
        # make an id variable to group covariate data
        ids = with(covariates, switch(
            level,
            "sessioncov"     = paste(array),
            "timecov"        = paste(array, occasion, sep = "_"),
            "trapcov"        = paste(array, post, sep = "_"),
            "timevaryingcov" = paste(array, post, occasion, sep = "_")
        ))
        # determine which covariates are constant within each id
        covnames = apply(do.call(rbind, sapply(unique(ids), function(id){
            # id = session_id[1] ; id
            apply(covariates[ids == id, , drop = FALSE], 2, function(x){
                all(x == x[1])
            })
        }, simplify = FALSE)), 2, all)
        covnames = colnames(covariates)[covnames]
        # extract the relvant columns from the covariates
        tempcov = covariates[!duplicated(ids), covnames, drop = FALSE]
        temparray = tempcov$array

        # groupcov - not currently implemented
        for(session in session(capthist)){
            covariates(capthist[[session]]) = NULL
        }

        # sessioncov
        # save as a single dataframe (n_rows = n_sessions)
        # append as an attribute
        # note that this is is different to how Murray deals with sessioncov in
        # the secr package: secr.fit allows the inclusion of sessioncov
        # and timecov via function arguments
        if(level == "sessioncov"){
            # dont use occasion
            tempcov = tempcov[, colnames(tempcov) != "occasion", drop = FALSE]
            rownames(tempcov) = session(capthist)
            # attr(capthist, "sessioncov") = tempcov
            sessioncov(capthist) = tempcov
        }else{
            # for all remaining levels, don't use sessioncov
            cols = !colnames(tempcov) %in% colnames(attr(capthist, "sessioncov"))
            tempcov = tempcov[, cols, drop = FALSE]
        }

        # timecov
        # a list of dataframes (n_elements = n_sessions)
        if(level == "timecov"){
            # if single-occasion then there can be no timecovs
            if(all(n_occasions(capthist) == 1)) next
            # dont use post
            tempcov = tempcov[, colnames(tempcov) != "post", drop = FALSE]
            if(ncol(tempcov) == 0) next
            timecov(capthist) = sapply(session(capthist), function(session){
                sub = tempcov[temparray == session, , drop = FALSE]
                rownames(sub) = NULL
                return(sub)
            }, simplify = FALSE)
        }else{
            # for all remaining levels, don't use timecov
            cols = !colnames(tempcov) %in% colnames(attr(capthist, "timecov"))
            tempcov = tempcov[, cols, drop = FALSE]
            # if no remaining covariates, then go to next loop
            if(ncol(tempcov) == 0) next
        }

        # trapcov
        # is a trap attribute and is accessed via secr::covariates(traps)
        # traps for separate sessions have their own data.frames
        if(level == "trapcov"){
            # dont use post or occasion
            cols = !colnames(tempcov) %in% c("post","occasion")
            tempcov = tempcov[, cols, drop = FALSE]
            if(ncol(tempcov) == 0) next
            # add tempcovs to each session
            for(session in session(capthist)){
                sub = tempcov[temparray == session, , drop = FALSE]
                rownames(sub) = NULL
                covariates(traps(capthist[[session]])) = sub
            }
            # for all remaining levels, don't use trapcov
            cols = !colnames(tempcov) %in% colnames(covariates(traps(capthist[[1]])))
            tempcov = tempcov[, cols, drop = FALSE]
            # if no remaining covariates, then go to next loop
            if(ncol(tempcov) == 0) next
        }

        # timevaryingcov
        # accessed via secr::timevaryingcov(traps)
        # but there is no useful description on any of the help pages
        # will have to work it out by trial and error
        if(level == "timevaryingcov"){
            # dont use occasion or post
            cols = !colnames(tempcov) %in% c("occasion","post")
            tempcov = tempcov[, cols, drop = FALSE]
            if(ncol(tempcov) == 0) next
            # add timevaryingcovs to each session
            for(session in session(capthist)){
                sub = tempcov[temparray == session, , drop = FALSE]
                rownames(sub) = NULL
                # timevaryingcov(traps(capthist[[session]])) = sub
                timevaryingcov(traps(capthist[[session]])) = NULL
            }
        }
    }

    return(capthist)
}


# shapefiles ===================================================================

#' @title Import ESRI shapefiles
#' @description This is a wrapper for the \link[raster]{shapefile} function
#'   (from the \pkg{raster} package). It performs additonal checks on data
#'   covariates (converting all logical and character variables to factors) and
#'   prepares a 'fortified' version of \link[sp]{SpatialPolygons} objects data
#'   to allow \pkg{ggplot}-style plotting.
#' @details A list with two elements:
#' \tabular{ll}{
#'   \code{sp} \tab an object of class \link[sp]{SpatialPolygons} or
#'   \link[sp]{SpatialPoints} \cr
#'   \code{fortified} \tab for polygon shapefiles, this will be the result of
#'   applying the \link[ggplot2]{fortify} function to the \code{sp} element \cr
#' }
#' @param x file path of the shapefile including the file name (the file
#'   extension is not required since it is assumed to be \code{.shp}), or a
#'   \link[sp]{SpatialPolygons} or \link[sp]{SpatialPoints} object
#' @param fortify if \code{TRUE}, a 'fortified' version of the spatial object
#'   will be returned (see Details)
#' @param ... additional arguments to pass to \link[raster]{shapefile}
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @seealso \link[gibbonsecr]{plot.gshp}, \link[gibbonsecr]{import_data}
#' @example inst/examples/example-import_shp.r
#' @importFrom raster shapefile
#' @importFrom tools file_ext
#' @importFrom ggplot2 fortify
#' @export

import_shp = function(x, fortify = TRUE, ...){

    # import shapefile ------------------------------------------------------- #
    # if x is character, assume it's a file path, otherwise check that it's a
    # SpatialPolygons or SpatialPoints object
    if(inherits(x, "character")){
        message("reading shapefile...")
        # file extension
        if(file_ext(x) == "") x = paste0(x, ".shp")
        if(file_ext(x) != "shp")
            stop("file extension not recognised", call. = FALSE)
        # file exists
        if(!file.exists(x)) x = file.path(getwd(), x)
        if(!file.exists(x))
            stop("file path does not exist:\n ", x, call. = FALSE)
        # read shp file
        x = try(shapefile(x, ...), TRUE)
        if(inherits(x, "try-error"))
            stop("couldn't import shapefile", call. = FALSE)
    }

    # check class ------------------------------------------------------------ #
    classes = c("SpatialPolygons", "SpatialPolygonsDataFrame",
                "SpatialPointsDataFrame")
    if(!inherits(x, classes))
        stop("expecting one of: '", paste(classes, collapse = "', '"), "'")

    # check data ------------------------------------------------------------- #
    # convert character and logical variables to factors
    if("data" %in% slotNames(x)){
        for(j in 1:ncol(x@data)){
            if(inherits(x@data[[j]], c("character","logical"))){
                x@data[[j]] = as.factor(x@data[[j]])
            }
        }
    }

    # fortify ---------------------------------------------------------------- #
    if(fortify && inherits(x, "SpatialPolygonsDataFrame")){
        # add temporary id column to sp data
        x@data$id = sapply(x@polygons, function(x) x@ID)
        message("fortifying polygons...")
        fortified = fortify(x, region = "id")
        x@data$id = NULL
    }else{
        fortified = NULL
    }

    # return ----------------------------------------------------------------- #
    if(fortify){
        # gshp class
        shp = list(sp = x, fortified = fortified)
        class(shp) = c("gshp", class(shp))
    }else{
        # sp class
        shp = x
    }
    return(shp)
}

# covariates ===================================================================

#' @title Add spatial covariates to mask or traps
#' @description Based on the \link[secr]{addCovariates} function from \pkg{secr}
#'   but which also accommodates \link[sp]{SpatialPointsDataFrame} data objects.
#' @param x a \link[gibbonsecr]{gmask} or
#'   \link[gibbonsecr]{gcapthist} object.
#' @param data a \link[gibbonsecr]{gshp} object
#' @example inst/examples/example-add_covariates.r
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export

add_covariates = function(x, data){
    message(paste0("adding covariates from ", substitute(data), " to ", substitute(x), "..."))

    # check inputs ----------------------------------------------------------- #
    is.mask     = inherits(x, "gmask")
    is.capthist = inherits(x, "gcapthist")
    if(!(is.mask | is.capthist))
        stop("expecting a gmask or gcapthist object")
    if(inherits(data, "gshp")) data = data$sp
    is.poly     = inherits(data, "SpatialPolygonsDataFrame")
    is.points   = inherits(data, "SpatialPointsDataFrame")
    if(!(is.poly | is.points))
        stop("expecting a SpatialPolygonsDataFrame or SpatialPointsDataFrame object")

    # add covariates --------------------------------------------------------- #
    new = lapply(x, function(x){
        message("- session ", session(x))
        tmp = x
        # if capthist, extract the traps object
        x = if(is.capthist) traps(x) else x
        # check x covariates for covnames
        if(!is.null(covariates(x))){
            for(j in colnames(data@data)){
                if(j %in% colnames(covariates(x))){
                    warning(paste0("overwriting existsing covariate '", j))
                    covariates(x)[[j]] = NULL
                }
            }
        }
        if(is.poly){
            x = addCovariates(x, data)
        }else{
            # use nearest point method for assigning covariates
            i = nearest_rcpp(as.matrix(x), as.matrix(data@coords))
            covs = data@data[i, , drop = FALSE]
            if(is.null(covariates(x))){
                covariates(x) = covs
                rownames(covariates(x)) = rownames(x)
            }else{
                covariates(x) = cbind(covariates(x), covs)
            }
        }
        # if capthist, update traps element
        if(is.capthist){
            traps(tmp) = x
            x = tmp
        }
        return(x)
    })
    # reset attributes ------------------------------------------------------- #
    attributes(new) = attributes(x)
    return(new)
}




