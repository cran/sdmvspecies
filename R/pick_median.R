#!/usr/bin/env Rscript


pickMedian <- function(env.stack, subset=NULL, stack=FALSE) {
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

    # TODO:here used mclapply but not given core.number
    species.list <- mclapply(X=env.names, FUN=.pickMedian, env.stack)
    
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

.pickMedian <- function(env.name, env.stack) {
    env.layer <- env.stack[[env.name]]
    env.matrix <- getValues(env.layer)
    env.quantile <- quantile(env.matrix, na.rm=TRUE)
    result.layer <- ((env.layer >= env.quantile[2]) & (env.layer <= env.quantile[4]))
    return(result.layer)
}

# files <- list.files(path="../../test/env/", pattern="*.bil$", full.names=TRUE)
# subset <- c("bio1", "bio12", "bio7", "bio5")
# env.stack <- stack(files)
# species.raster <- pickMedian(env.stack, subset=subset)
# species.raster <- pickMedian(env.stack, subset=subset, stack=TRUE)