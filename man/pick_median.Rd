\name{pickMedian}

\alias{pickMedian}

\title{
pick median method
}

\description{ 
use pick median method to create virtual species.
}

\usage{
pickMedian(env.stack, subset=NULL, stack=FALSE)
}
 
\arguments{
  \item{env.stack}{A RasterStack object}
  \item{subset}{string vector, New in version 0.2. using a subset of env.stack to create vritual speices. NULL means using all env.stack}
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
files <- list.files(path=env.dir, pattern="*.bil$", full.names=TRUE)
# make raster stack
env.stack <- stack(files)
# run pick mean
species.raster <- pickMedian(env.stack)
# plot map
plot(species.raster)
}

\keyword{pick median method}
