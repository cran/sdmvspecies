#!/usr/bin/env Rscript

library("raster")
library("parallel")

pickMean <- function(env.stack) {
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }
    env.names <- unlist(names(env.stack))
    #print(env.names)

    # TODO:here used mclapply but not given core.number
    species.list <- lapply(X=env.names, FUN=.pickMean, env.stack)

    species.stack <- stack(species.list)
    species.layer <- stackApply(species.stack, c(1), sum, na.rm=FALSE)
    threshold <- nlayers(env.stack)
    species.layer <- species.layer >= threshold
    return(species.layer)
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

#files <- list.files(path="./env.cn.synthese", pattern="*.bil$", full.names=TRUE)
#files <- c("./env/bio1.bil", "./env/bio12.bil", "./env/bio7.bil", "./env/bio5.bil")
#env.stack <- stack(files)
#species.raster <- pickMean(env.stack)
#writeRaster(species.raster, "pickmean_distribution.img", "HFA")