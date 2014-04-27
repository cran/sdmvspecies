#!/usr/bin/env Rscript

library("raster")
library("parallel")

.gaussianTranslate <- function(factor, range, min, mean) {
    normal.sd <- range/6
    result <- dnorm(factor, mean, normal.sd) 
    #print(result)
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
    #print(len(factor))
    
    original.factor <- factor
    factor.na <- is.na(original.factor)
    indictor <- original.factor <= critical.point
    indictor[factor.na] <- FALSE
    
    factor[indictor]<- 1
    #print(len(factor))
    indictor <- original.factor > critical.point
    indictor[factor.na] <- FALSE
    new.value <- 1/(range-critical.point)*(range+min-original.factor)
    factor[indictor] <- new.value[indictor]
    
    #print(len(factor))
    
    return(factor)
}

nicheSynthese <- function(env.stack, config) {
    #library("raster")
    
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
        #print(param.item[3])
        #print(is.numeric(param.item[3]))
        #if (!(is.numeric(param.item[3]))) {
        #    stop("weight must be numeric")
        #}
    }
    # TODO:here used mclapply but not given core.number
    species.list <- mclapply(X=config, FUN=.nicheSyntheseMain, env.stack)

    species.matrix <- matrix(unlist(species.list), ncol=length(config), byrow=FALSE)
    species <- apply(species.matrix, 1, sum)
    species.layer <- env.stack[[config[[1]][1]]]
    species.raster <- setValues(species.layer, as.vector(species))
    return(species.raster)
}

.nicheSyntheseMain <- function(var, env.stack) {
    #print(var)
    #print(env.stack)
    predictor.name <- var[1]
    niche.function <- var[2]
    weight <- var[3]
    weight <- as.integer(weight)

    env.layer <- env.stack[[predictor.name]]
    #print(env.layer)

    factors.max <- cellStats(env.layer, stat='max', na.rm=TRUE)
    factors.min <- cellStats(env.layer, stat='min', na.rm=TRUE)
    factors.range <- factors.max - factors.min
    factors.mean <- factors.range*0.5 + factors.min

    factor <- getValues(env.layer)
    #print(factor)

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



#files <- list.files(path="./env.cn.synthese", pattern="*.bil$", full.names=TRUE)
#env.stack <- stack(files)
#config <- list(c("bio1","1",1), c("bio2", "2", 2), c("bio3", "4", 1), c("bio4", "4", 1), c("bio5", "5", 2))
#species.raster <- nicheSynthsis(env.stack, config)
#writeRaster(species.raster, "synthese_v3.img", "HFA")
