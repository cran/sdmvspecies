#!/usr/bin/env Rscript


pickMean <- function(env.stack, subset=NULL, stack=FALSE) {
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }

    env.names <- unlist(names(env.stack))

    if (length(subset)) {
        check.result <- subset %in% env.names
        if (!all(check.result)) {
            mis.name <- subset[!check.result]
            message <- paste("Follow variable:", paste(mis.name, collapse=","), "are missing!\nAlready auto removed!", sep="")
            warning(message)
        }
        env.names <- subset[check.result]
    }
    #print(env.names)

    # TODO:here used mclapply but not given core.number
    species.list <- mclapply(X=env.names, FUN=.pickMean, env.stack)
    #print(length(species.list))

    if (!stack) {
        species.stack <- stack(species.list)
        species.layer <- stackApply(species.stack, c(1), sum, na.rm=FALSE)
        threshold <- length(env.names)
        species.layer <- species.layer >= threshold
        return(species.layer)
    } else {        
        species.layer <- env.stack[[env.names[1]]]
        col.number <- length(species.list)
        species.stack <- stack()
        for (col.index in 1:col.number) {
            species.raster <- setValues(species.layer, as.vector(species.list[[col.index]]))
            species.stack <- stack(species.stack, species.raster)
        }
        names(species.stack) <- env.names
        return(species.stack)
    }
}


.pickMean <- function(env.name, env.stack) {
    env.layer <- env.stack[[env.name]]
    factors.mean <- cellStats(env.layer, stat='mean', na.rm=TRUE)
    factors.sd <- cellStats(env.layer, stat='sd', na.rm=TRUE)
    factors.min.end <- factors.mean-factors.sd
    factors.max.end <- factors.mean+factors.sd
    result.layer <- ((env.layer >= factors.min.end) & (env.layer <= factors.max.end))
    return(result.layer)
}


# files <- list.files(path="../../test/env/", pattern="*.bil$", full.names=TRUE)
# subset <- c("bio1", "bio12", "bio7", "bio5")
# env.stack <- stack(files)
# species.raster <- pickMean(env.stack)
# species.raster <- pickMean(env.stack, subset=subset, stack=TRUE)