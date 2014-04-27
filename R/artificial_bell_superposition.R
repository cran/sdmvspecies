#!/usr/bin/env Rscript

library("raster")
library("parallel")

.artificialGaussianTranslate <- function(factor, normal.mean, normal.sd) {
    result <- dnorm(factor, normal.mean, normal.sd) 
    result <- sqrt(2*pi)*normal.sd*result
    return(result)
}

artificialBellSuperposition <- function(env.stack, config) {
    #library("raster")
    
    # check env.stack first
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }
    # TODO:here used mclapply but not given core.number
    species.list <- lapply(X=config, FUN=.artificialBellSuperpositionMain, env.stack)

    species.matrix <- matrix(unlist(species.list), ncol=length(config), byrow=FALSE)
    species <- apply(species.matrix, 1, sum)
    #print(species)
    #stop()
    species.layer <- env.stack[[config[[1]][1]]]

    species.raster <- setValues(species.layer, as.vector(species))
    return(species.raster)
}

.artificialBellSuperpositionMain <- function(var, env.stack) {
    #print(var)
    #print(env.stack)
    predictor.name <- var[1]
    normal.mean <- var[2]
    normal.mean <- as.integer(normal.mean)
    normal.sd <- var[3]
    normal.sd <- as.integer(normal.sd)

    env.layer <- env.stack[[predictor.name]]
    #print(env.layer)

    factor <- getValues(env.layer)
    #print(factor)

    #result <- .gaussianTranslate(factor, factors.range, factors.min, factors.mean)
    result <- .artificialGaussianTranslate(factor, normal.mean, normal.sd)
    return(result)
}

# files <- list.files(path="../../test/env", pattern="*.bil$", full.names=TRUE)
# env.stack <- stack(files)
# config <- list(c("bio1",150, 50), c("bio12", 2000, 500), c("bio7", 400, 100), c("bio5", 300, 100))
# species.raster <- artificialBellSuperposition(env.stack, config)