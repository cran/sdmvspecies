#!/usr/bin/env Rscript

library("raster")
library("parallel")

.artificialLimitationTranslate <- function(factor, normal.mean, normal.sd) {
    result <- (factor > normal.mean-normal.sd) & (factor < normal.mean+normal.sd)
    return(result)
}

artificialBellLimitation <- function(env.stack, config) {
    #library("raster")
    
    # check env.stack first
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }
    # TODO:here used mclapply but not given core.number
    species.list <- lapply(X=config, FUN=.artificialBellLimitationMain, env.stack)

    species.matrix <- matrix(unlist(species.list), ncol=length(config), byrow=FALSE)
    species <- apply(species.matrix, 1, sum)
    
    species.layer <- env.stack[[config[[1]][1]]]

    species.raster <- setValues(species.layer, as.vector(species))
    #print(species.raster)
    threshold <- length(config)
    species.raster <- species.raster >= threshold
    return(species.raster)
}

.artificialBellLimitationMain <- function(var, env.stack) {
    #print(var)
    #print(env.stack)
    predictor.name <- var[1]
    normal.mean <- var[2]
    normal.mean <- as.double(normal.mean)
    normal.sd <- var[3]
    normal.sd <- as.double(normal.sd)
    layer.threshold <- var[4]
    layer.threshold <- as.double(layer.threshold)

    env.layer <- env.stack[[predictor.name]]
    #print(env.layer)

    factor <- getValues(env.layer)
    #print(factor)

    #result <- .gaussianTranslate(factor, factors.range, factors.min, factors.mean)
    result <- .artificialLimitationTranslate(factor, normal.mean, normal.sd)
#     result[result >= layer.threshold] <- 1
#     result[result < layer.threshold] <- 0
    #print(result)
    return(result)
}

# files <- list.files(path="../../test/env", pattern="*.bil$", full.names=TRUE)
# env.stack <- stack(files)
# # config <- list(c("bio1",150, 50, 0.9), c("bio12", 2000, 500, 0.9), c("bio7", 400, 100, 0.9), c("bio5", 300, 100, 0.9))
# config <- list(c("bio1",150, 50), c("bio12", 2000, 500), c("bio7", 400, 200), c("bio5", 300, 100))
# species.raster <- artificialBellLimitation(env.stack, config)