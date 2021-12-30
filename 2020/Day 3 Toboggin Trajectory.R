library( tidyverse)
dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day03"
toboggin <- str_split( readLines( file.path( dir, ff)), "", simplify=T ) == "#"

# answer1
drs <- 1
dcs <- 3

# ansewr2
# drs <- c( 1,1,1,1,2)
# dcs <- c( 1,3,5,7,1)
pp  <- 1

for( idx in 1:length( dcs ))
{
  dr <- drs[idx]
  dc <- dcs[idx]
  r <- seq( 1, nrow(toboggin), by=dr)

  r <- r[r<=nrow(toboggin)]
  c <- seq( 1, nrow(toboggin)*(dc+1), by=dc )
  c <- head(c, length( r )) %% ncol(toboggin)
  c[c==0] = ncol(toboggin)
  rc <- matrix( c(r,c), ncol=2)
  pp <- pp * sum(toboggin[rc])
}

pp
