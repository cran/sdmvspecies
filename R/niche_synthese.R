#!/usr/bin/env Rscript


.gaussianTranslate <- function(factor, range, min, mean) {
    normal.sd <- range/6
    result <- dnorm(factor, mean, normal.sd) 

    result <- sqrt(2*pi)*normal.sd*result
    return(result)
}


.linearIncreaseTranslate <- function(factor, range, min, mean) {
    result <- 1/range*(factor-min)
}


.linearDecreaseTranslate <- function(factor, range, min, mean) {
    result <- 1/range*(min+range-factor)
}


.truncatedLinearIncreaseTranslate <- function(factor, range, min, mean) {
    critical.point <- range*2/3+min
    
    original.factor <- factor
    factor.na <- is.na(original.factor)
    indictor <- original.factor >= critical.point
    indictor[factor.na] <- FALSE
    
    factor[indictor]<- 1
    indictor <- original.factor < critical.point
    indictor[factor.na] <- FALSE
    new.value <- 1/critical.point*(original.factor-min)
    factor[indictor] <- new.value[indictor]

    return(factor)
}


.truncatedLinearDecreaseTranslate <- function(factor, range, min, mean) {
    critical.point <- range*1/3+min
    
    original.factor <- factor
    factor.na <- is.na(original.factor)
    indictor <- original.factor <= critical.point
    indictor[factor.na] <- FALSE
    
    factor[indictor]<- 1
    indictor <- original.factor > critical.point
    indictor[factor.na] <- FALSE
    new.value <- 1/(range-critical.point)*(range+min-original.factor)
    factor[indictor] <- new.value[indictor]
    
    return(factor)
}

nicheSynthese <- function(env.stack, config, stack=FALSE) {
    RESPONSE_METHOD = seq(1,5)
    
    # check env.stack first
    if (!(class(env.stack) %in% "RasterStack")) {
        stop("env.stack is not a RasterStack object!")
    }
    # check params
    for (config.item in config) {
        #print(param.item)
        if (!(config.item[1] %in% names(env.stack))) {
            stop("params must match with env.stack in layer names")
        }
        if (!(config.item[2] %in% RESPONSE_METHOD)) {
            stop("response method is wrong")
        }
    }
    # TODO:here used mclapply but not given core.number
    species.list <- mclapply(X=config, FUN=.nicheSyntheseMain, env.stack)
    
    species.matrix <- matrix(unlist(species.list), ncol=length(config), byrow=FALSE)
    if (!stack) {
        species <- apply(species.matrix, 1, sum)
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
        ncol <- length(config[[1]])
        config.matrix <- matrix(unlist(config), byrow=TRUE, ncol=ncol)
        layer.names <- config.matrix[,1]
        names(species.stack) <- layer.names
        return(species.stack)
    }
}

.nicheSyntheseMain <- function(var, env.stack) {
    predictor.name <- var[1]
    niche.function <- var[2]
    weight <- var[3]
    weight <- as.integer(weight)

    env.layer <- env.stack[[predictor.name]]

    factors.max <- cellStats(env.layer, stat='max', na.rm=TRUE)
    factors.min <- cellStats(env.layer, stat='min', na.rm=TRUE)
    factors.range <- factors.max - factors.min
    factors.mean <- factors.range*0.5 + factors.min

    factor <- getValues(env.layer)

    if (niche.function == "1") {
        result <- .gaussianTranslate(factor, factors.range, factors.min, factors.mean)
    } else if (niche.function == "2") {
        result <- .linearIncreaseTranslate(factor, factors.range, factors.min, factors.mean)
    } else if (niche.function == "3") {
        result <- .linearDecreaseTranslate(factor, factors.range, factors.min, factors.mean)
    } else if (niche.function == "4") {
        result <- .truncatedLinearIncreaseTranslate(factor, factors.range, factors.min, factors.mean)
    } else if (niche.function == "5") {
        result <- .truncatedLinearDecreaseTranslate(factor, factors.range, factors.min, factors.mean)
    }
    result <- result*weight
    return(result)
}


# files <- list.files(path="../../test/env/", pattern="*.bil$", full.names=TRUE)
# env.stack <- stack(files)
# config <- list(c("bio1","1",1), c("bio2", "2", 2), c("bio3", "4", 1), c("bio4", "4", 1), c("bio5", "5", 2))
# species.raster <- nicheSynthese(env.stack, config)
# species.raster <- nicheSynthese(env.stack, config, stack=TRUE)