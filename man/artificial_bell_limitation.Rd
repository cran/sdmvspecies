\name{artificialBellLimitation}

\alias{artificialBellLimitation}

\title{
artificial bell-shaped Limitation method
}

\description{ 
use artificial bell-shaped limitation method to create virtual species.
}

\usage{
artificialBellLimitation(env.stack, config)
}

\arguments{
  \item{env.stack}{A RasterStack object}
  \item{config}{list. Contain the configure information for this method)} 
}

\value{
A RasterLayer object that is the virtual species distribution map
}

\author{
Howl Anderson
}

\examples{
# load the sdmvspecies library
library("sdmvspecies")
# load parrllel to use mutilcore CPU
#library("parallel")
# find package's location
package.dir <- system.file(package="sdmvspecies")
# let see where is our sdmvspecies is installed in
package.dir
# find env dir under the package's location
env.dir <- paste(package.dir, "/external/env/", sep="")
# let see env dir
env.dir
# get the environment raster file
file.name <- files <- c("bio1.bil", "bio12.bil", "bio7.bil", "bio5.bil")
files <- paste(env.dir, file.name, sep="")
# make raster stack
env.stack <- stack(files)
# config
config <- list(c("bio1",150, 50), c("bio12", 2000, 500), c("bio7", 400, 100), c("bio5", 300, 100))
# run pick mean
species.raster <- artificialBellLimitation(env.stack, config)
# plot map
plot(species.raster)
}

\keyword{artificial bell-shaped Limitation method}