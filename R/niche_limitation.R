#!/usr/bin/env Rscript


.limitMinMax <- function(factor, limit.min, limit.max) {
    result <- (factor >=limit.min) & (factor <= limit.max)
    return(result)
}


.limitMin <- function(factor, limit.min) {
    result <- factor >= limit.min
    return(result)
}


.limitMax <- function(factor, limit.max) {
    result <- factor <= limit.max
    return(result)
}


nicheLimitation <- function(env.stack, config, stack=FALSE) {
    RESPONSE_METHOD = seq(1,5)
    
    # check env.stack first
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }
    # check params
    for (config.item in config) {
        if (!(config.item[1] %in% names(env.stack))) {
            stop("params must match with env.stack in layer names")
        }
        if (!(config.item[2] %in% RESPONSE_METHOD)) {
            stop("response method is wrong")
        }
    }
    # TODO:here used mclapply but not given core.number
    species.list <- mclapply(X=config, FUN=.nicheLimitationMain, env.stack)
    #print(species.list)

    species.matrix <- matrix(unlist(species.list), ncol=length(config), byrow=FALSE)
    if (!stack) {
        species <- apply(species.matrix, 1, sum)
        species.layer <- env.stack[[config[[1]][1]]]
        species.raster <- setValues(species.layer, as.vector(species))
        threshold <- length(config)
        species.raster <- species.raster >= threshold
        return(species.raster)
    } else {
        species.layer <- env.stack[[config[[1]][1]]]
        col.number <- ncol(species.matrix)
        species.stack <- stack()
        for (col.index in 1:col.number) {
            species.raster <- setValues(species.layer, as.vector(species.matrix[,col.index]))
            species.stack <- stack(species.stack, species.raster)
        }
        ncol <- length(config[[1]])
        config.matrix <- matrix(unlist(config), byrow=TRUE, ncol=ncol)
        layer.names <- config.matrix[,1]
        names(species.stack) <- layer.names

        return(species.stack)
    }
}


.nicheLimitationMain <- function(var, env.stack) {
    predictor.name <- var[1]
    niche.function <- var[2]
    limit.min <- var[3]
    limit.min <- as.double(limit.min)
    limit.max <- var[4]
    limit.max <- as.double(limit.max)

    env.layer <- env.stack[[predictor.name]]

    factor <- getValues(env.layer)

    if (niche.function == "1") {
        result <- .limitMinMax(factor, limit.min, limit.max)
    } else if (niche.function == "2") {
        result <- .limitMin(factor, limit.min)
    } else if (niche.function == "3") {
        result <- .limitMax(factor, limit.max)
    }
    return(result)
}


# files <- list.files(path="../../test/env/", pattern="*.bil$", full.names=TRUE)
# env.stack <- stack(files)
# config <- list(c("bio1", "1", 0, 100), c("bio2", "2", 70, 100))
# species.raster <- nicheLimitation(env.stack, config)
# species.raster <- nicheLimitation(env.stack, config, stack=TRUE)