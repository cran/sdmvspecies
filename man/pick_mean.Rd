\name{pickMean}

\alias{pickMean}

\title{
pick mean method
}

\description{ 
use pick mean method to create virtual species.
}

\usage{
pickMean(env.stack)
}
 
\arguments{
  \item{env.stack}{A RasterStack object}
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
species.raster <- pickMean(env.stack)
# plot map
plot(species.raster)
}

\keyword{pick mean method}