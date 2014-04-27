#!/usr/bin/env Rscript

library("raster")
library("parallel")

pickMedian <- function(env.stack) {
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }
    env.names <- unlist(names(env.stack))
    #print(env.names)

    # TODO:here used mclapply but not given core.number
    species.list <- lapply(X=env.names, FUN=.pickMedian, env.stack)

    species.stack <- stack(species.list)
    species.layer <- stackApply(species.stack, c(1), sum, na.rm=FALSE)
    threshold <- nlayers(env.stack)
    species.layer <- species.layer >= threshold
    return(species.layer)
}
.pickMedian <- function(env.name, env.stack) {
    env.layer <- env.stack[[env.name]]
    env.matrix <- getValues(env.layer)
    env.quantile <- quantile(env.matrix, na.rm=TRUE)
    result.layer <- ((env.layer >= env.quantile[2]) & (env.layer <= env.quantile[4]))
    return(result.layer)
}

#files <- list.files(path="./env.cn.synthese", pattern="*.bil$", full.names=TRUE)
#env.stack <- stack(files)
#species.raster <- pickQuantile(env.stack)
