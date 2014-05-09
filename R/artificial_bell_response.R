#!/usr/bin/env Rscript


.artificialGaussianTranslate <- function(factor, normal.mean, normal.sd) {
    result <- dnorm(factor, normal.mean, normal.sd) 
    result <- sqrt(2*pi)*normal.sd*result
    return(result)
}


artificialBellResponse <- function(env.stack, config, stack=FALSE, compose="product") {
    # check env.stack first
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }
    # TODO:here used mclapply but not given core.number
    species.list <- mclapply(X=config, FUN=.artificialBellResponseMain, env.stack)

    species.matrix <- matrix(unlist(species.list), ncol=length(config), byrow=FALSE)
    if (!stack) {
        if (compose == "product") {
            species <- apply(species.matrix, 1, prod)
        } else if (compose == "sum") {
            species <- apply(species.matrix, 1, sum)
        }
        species.layer <- env.stack[[config[[1]][1]]]
        species.raster <- setValues(species.layer, as.vector(species))
        return(species.raster)
    } else {
        species.layer <- env.stack[[config[[1]][1]]]
        col.number <- ncol(species.matrix)
        species.stack <- stack()
        for (col.index in 1:col.number) {
            species.raster <- setValues(species.layer, as.vector(species.matrix[,col.index]))
            species.stack <- stack(species.stack, species.raster)
        }
        config.matrix <- matrix(unlist(config), byrow=TRUE, ncol=3)
        layer.names <- config.matrix[,1]
        names(species.stack) <- layer.names
        return(species.stack)
    }
}


.artificialBellResponseMain <- function(var, env.stack) {
    predictor.name <- var[1]
    normal.mean <- var[2]
    normal.mean <- as.integer(normal.mean)
    normal.sd <- var[3]
    normal.sd <- as.integer(normal.sd)

    env.layer <- env.stack[[predictor.name]]

    factor <- getValues(env.layer)

    result <- .artificialGaussianTranslate(factor, normal.mean, normal.sd)
    return(result)
}

# files <- list.files(path="../../test/env", pattern="*.bil$", full.names=TRUE)
# env.stack <- stack(files)
# config <- list(c("bio1",100, 200), c("bio12", 2800, 1500), c("bio7", 200, 300), c("bio5", 300, 100))
# #config <- list(c("bio1",1, 300), c("bio14", 100, 160), c("bio5", 200, 200), c("bio11", 50, 190), c("bio16", 500, 100))
# species.raster <- artificialBellResponse(env.stack, config, compose="sum")
# species.raster.pro <- artificialBellResponse(env.stack, config, compose="product")
# species.raster.stack <- artificialBellResponse(env.stack, config, stack=TRUE)