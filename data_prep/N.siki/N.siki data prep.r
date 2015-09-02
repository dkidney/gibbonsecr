

# setup -------------------------------------------------------------------

require(secr)
main.folder = "~/Dropbox/packages/gibbonsecr/gibbonsecr_1.0/gibbonsecr"
wd = setwd(main.folder)
op = options(stringsAsFactors = FALSE)

# try(detach("package:gibbonsSECR", unload = TRUE), TRUE)
# require(secr)
# setwd("~/Packages/gibbonsSECR")
# source("r/gibbonsSECR-functions.r")


# load raw data -----------------------------------------------------------

data.folder = file.path(main.folder, "data_raw/N.siki/data")
    
# there are three data files:

## SECR_gibbons.csv
## - this is the the first version of the data
## - it contains eastings and northings for all the listening posts
## - the repeat column identifies which occasion

SECR_gibbons = read.csv(file.path(data.folder, "SECR_gibbons.csv"))
head(SECR_gibbons) 
# str(SECR_gibbons)

## SECR_detections4_V2.csv
## - this is an updated version of the capture data
## - it doesn't contain temperature or listening post coordinates
## - bearings are in degrees
## - distances are in metres
## - Elevation.m. looks like a post-level covariate 

SECR_detections4_V2 = read.csv(file.path(data.folder, "SECR_detections4_V2.csv"))
head(SECR_detections4_V2) 
# str(SECR_detections4_V2)

## Covariates4.csv
## - this contains occasion-level covariates
## - ElevationLP - post-level (i.e. trapcov) - same as Elevation.m. in SECR_gibbons.csv
## - Elev        - array-level (i.e. sessioncov)
## - Rugged      - array-level (i.e. sessioncov)
## - distwater   - array-level (i.e. sessioncov)
## - TempAM      - occasion-level (i.e. timecov)

Covariates4 = read.csv(file.path(data.folder, "Covariates4.csv"))
head(Covariates4) 
# str(Covariates4)



# detections --------------------------------------------------------------

head(SECR_detections4_V2) ; dim(SECR_detections4_V2)

# fix the record in SECR_detections4_V2 with bearing = 371 - change it to 360 
SECR_detections4_V2$bearing[SECR_detections4_V2$bearing == 371] = 360
head(SECR_detections4_V2) ; dim(SECR_detections4_V2)

# delete duplicates
detections = unique(SECR_detections4_V2)
head(detections) ; dim(detections)

# get vector of ids giving unique array-post-occasion-group combinations

# check for duplicated array-post-occasion-group combinations
ids = paste(
    detections[["array"]],
    detections[["post"]],
    detections[["occasion"]],
    detections[["group"]],
    sep = "_"
)
head(ids)
any(duplicated(ids))

# some detections appear to be duplicates but with different bearing and distance data
head(detections[ids %in% ids[duplicated(ids)],])

# use average bearing and distance for duplicated ids
mean.bearing = function(x){
    (x[1] + min(abs(diff(x)), 360 - abs(diff(x))) / 2) %% 360
}
mean.bearing(x = c(10, 20)) # should be 15
mean.bearing(x = c(355, 05)) # should be 0
mean.bearing(x = c(345, 05)) # should be 355

detections = do.call(rbind, lapply(unique(ids), function(id){ # id = unique(ids)[1] ; id
    
    rows = which(ids == id)
    if(length(rows) == 0 || length(rows) > 2)
        stop("something when wrong...")
    temp = detections[rows,] 
    if(length(rows) == 2){
        temp$bearing = mean.bearing(temp$bearing)
        temp$distance = mean(temp$distance)
        temp = temp[1,,drop = FALSE]
    }
    rownames(temp) = NULL
    return(temp)
    
}))

# recalculate ids to check no more duplicates
ids = paste(
    detections[["array"]],
    detections[["post"]],
    detections[["occasion"]],
    detections[["group"]],
    sep = "_"
)
any(duplicated(ids))

head(detections) ; dim(detections)



# posts ----------------------------------------------------------

head(SECR_gibbons) ; dim(SECR_gibbons)
j = colnames(SECR_gibbons) %in% c("Array", "Listening.post", "Easting", "Northing", "Elevation.m.", "repeat.")
posts = SECR_gibbons[,j]
colnames(posts) = c("array", "post", "x", "y", "elevation", "occasion")
head(posts) ; dim(posts)

# delete duplicates
posts = unique(posts)
head(posts) ; dim(posts)

# delete array 28 (no detections)
# posts = posts[posts$array != "28",]
# head(posts) ; dim(posts)

# check for duplicated array-post combinations
ids = paste(
    posts[["array"]],
    posts[["post"]],
    sep = "_"
)
head(ids)
any(duplicated(ids))

# duplicates differ by occasion - one row per occasion
head(posts[ids %in% ids[duplicated(ids)],], 12)

# need to collapse data to one row per unique array-post and add a usage column
posts = do.call(rbind, lapply(unique(ids), function(id){ # id = unique(ids)[1] ; id
    
    rows = which(ids == id) 
    temp = posts[rows,]
    usage = c(0,0,0,0)
    usage[temp$occasion] = 1
    usage = paste(usage, collapse = "") ; usage
    temp = temp[1, c("array", "post", "x", "y", "elevation"), drop = FALSE]
    temp$usage = usage
    rownames(temp) = NULL
    return(temp)
    
}))

# recalculate ids to check no more duplicates
ids = paste(
    posts[["array"]],
    posts[["post"]],
    sep = "_"
)
any(duplicated(ids))

head(posts) ; dim(posts)



# covariates --------------------------------------------------------------

head(Covariates4) ; dim(Covariates4)

# delete duplicates
covariates = unique(Covariates4)
head(covariates) ; dim(covariates)

# check for duplicated array-post-occasion combinations
ids = paste(
    covariates[["array"]],
    covariates[["post"]],
    covariates[["occasion"]],
    sep = "_"
)
head(ids)
any(duplicated(ids))

head(covariates) ; dim(covariates)

# elevation
## ElevationLP is the same as elevation in posts, so can be deleted
## Elev looks like a array-level version of elevation - delete to avoid confusion
covariates[["ElevationLP"]] = NULL
covariates[["Elev"]] = NULL

# reorder
covariates = covariates[, c("array", "post", "occasion", "Rugged", "distWater", "TempAM")]
covariates = covariates[order(covariates[["array"]], covariates[["post"]], covariates[["occasion"]]), ]
rownames(covariates) = NULL
head(covariates) ; dim(covariates)

# check all array-occasion-post combinations present
index = do.call(rbind, lapply(1:nrow(posts), function(i){ # i = 1
    temp = posts[i,]
    data.frame(
        array = temp$array,
        post = temp$post,
        occasion = (1:4)[as.numeric(strsplit(temp$usage, "")[[1]]) == 1]
    )
}))
head(index)

covariates = do.call(rbind, lapply(1:nrow(index), function(i){ # i=5
    
    # store this row of the index matrix
    temp = index[i,]
    
    # identify matching rows in covariates
    j = which(covariates$array == temp$array &
            covariates$post == temp$post &
            covariates$occasion == temp$occasion
    ) # covariates[j,]
    
    if(length(j) == 1){
        
        # if one matching row then return the original row in the covariates
        temp = covariates[j,]
        
    }else if(length(j) > 1){
        
        # if more than one matching row then we have unidentified duplicate rows
        stop("check for duplicates", call. = FALSE)
        
    }else{

        # if no matching rows then need to try and impute covariate values
        
        # array-level
        # find matching rows for this array
        k = which(covariates$array == temp$array) # covariates[k,]
        
        if(length(k) == 0){
            temp[["Rugged"]]    = NA
            temp[["distWater"]] = NA
            temp[["TempAM"]]    = NA
        }else{
            temp[["Rugged"]]    = covariates[["Rugged"]][k[1]]
            temp[["distWater"]] = covariates[["distWater"]][k[1]]
            
            # occasion-level
            # find rows for this array and occasion
            k = which(covariates$array == temp$array &
                          covariates$occasion == temp$occasion
            ) # covariates[k,]
            
            if(length(k) == 0){
                # if length k is zero then there are no available covariates for this array-occasion
                temp[["TempAM"]] = NA
            }else{
                # impute the missing occasion-level covariates
                temp[["TempAM"]] = covariates[["TempAM"]][k[1]]
            }
            
        }
        
    }
    
    rownames(temp) = NULL
    
    return(temp)
    
}))
head(covariates, 20) ; dim(covariates)


# missing temperature data

# deal with missing TempAM data
rows = which(is.na(covariates$TempAM))
for(i in rows){ # i = 316

    # identify the array-post-occasion 
    array    = covariates[["array"]][i]    ; array
    post     = covariates[["post"]][i]     ; post
    occasion = covariates[["occasion"]][i] ; occasion

    # delete any detections for this array-post-occasion
    j = detections[["array"]]    == array &
        detections[["post"]]     == post &
        detections[["occasion"]] == occasion ; which(j)
    detections = detections[!j,]
    
    # set trap usage for this array-post-occasion 0
    k = posts[["array"]] == array &
        posts[["post"]]  == post
    usage = strsplit(posts[["usage"]][k], "")[[1]]
    usage[occasion] = "0"
    posts[["usage"]][k] = paste(usage, collapse = "")
    
} 

# delete the row in the covariates
covariates = covariates[-rows,]

head(covariates, 20) ; dim(covariates)








# save as rda -----------------------------------------------------------

details = list(
    bearings = list(
        units = "degrees",
        type  = "continuous"
    ),
    distances = list(
        units = "m",
        type  = "continuous"
    )
)

N.siki = import_data(
    detections = detections, 
    posts      = posts, 
    covariates = covariates, 
    region     = NULL,
    habitat    = NULL,
    details    = details
)

# N.siki[["27"]]
# usage(traps(N.siki))[["27"]]
# get_captures(N.siki, "occasions")

# verify(capthist)

save(N.siki, file = file.path(main.folder, "data/N.siki.rda"))



# clean up ----------------------------------------------------------------

setwd(wd)
options(op)

## ************************************************************************** ## 
## ************************************************************************** ## 
