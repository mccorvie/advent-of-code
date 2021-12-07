library( tidyverse )
library( bit64)
dir <- "~/Desktop/Advent-Code-2021/Dec 07"
ff <- "input"

lines    <- readLines( file.path( dir, ff))
crab_pos <- strtoi(str_split( lines, ",", simplify=T))

# L1 minimizer is the median
mm <- median(crab_pos)
sum(abs(crab_pos-mm))

# L2 minimizer is the mean
target <- round(mean( crab_pos))
dist <- abs( crab_pos-target)
sum(dist*(dist+1)/2)


fuel_cost <- function( target )
{
  dist <- abs(crab_pos-target)
  sum(dist*(dist+1)/2)
}

for( target in 475:495)
  cat( target, fuel_cost( target ) - 94862000, "\n")

