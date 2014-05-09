\name{nicheLimitation}

\alias{nicheLimitation}

\title{
niche limitation method
}

\description{ 
use niche limitation method to create virtual species.
}

\usage{
nicheLimitation(env.stack, config, stack=FALSE)
}
 
\arguments{
  \item{env.stack}{A RasterStack object}
  \item{config}{list. Contain the configure information for this method)}
  \item{stack}{logical, New in version 0.2. if you want return a RasterStack object rather than combine them together?}
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
# find package's location
package.dir <- system.file(package="sdmvspecies")
# let see where is our sdmvspecies is installed in
package.dir
# find env dir under the package's location
env.dir <- paste(package.dir, "/external/env/", sep="")
# let see env dir
env.dir
# get the environment raster file
env.files <- list.files(env.dir, pattern="*.bil$", full.names=TRUE)
# see the file list
env.files
# put the environment file in a raster stack, 
# which require all the environment should have same resolution and extend
env.stack <- stack(env.files)
# let see the env.stack var
env.stack
# here let's configure the environment response function and weight
config <- list(c("bio1", "1", 0, 100), c("bio11", "2", -100, NULL))

species.raster <- nicheLimitation(env.stack, config)
# plot
plot(species.raster)
}

\keyword{niche limitation method}