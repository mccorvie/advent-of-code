library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 07"
ff <- "input"

crab_pos <- scan( file.path( dir, ff), sep=",")

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

crab_pos = c(rep(1,100),rep(100,20))

x_pos <- min(crab_pos):max(crab_pos)
fuel_costs <- sapply( x_pos, fuel_cost)
min(fuel_costs)
x_pos[which( fuel_costs==min(fuel_costs))]

# but the actual min is 1 away because its not quite the L2 minimizer
optimize( fuel_cost, c( min(crab_pos), max( crab_pos)))
mean(crab_pos)
median(crab_pos)


