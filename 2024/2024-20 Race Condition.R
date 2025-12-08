#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 20
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "a" ))

use_test = T
input = if( use_test ) test else raw

racetrack <- input |> map( \(x) str_split_1( x,"")) |> reduce( rbind)


dist_one_step <- \(mm)
{
  mm_up   <- rbind( mm[2:dim,], pad)
  mm_down <- rbind( pad, mm[1:(dim-1),] )
  mm_left  <- cbind( mm[,2:dim], pad)
  mm_right <- cbind( pad, mm[,1:(dim-1)] )
  
  mm <- pmin( mm, mm_up+1, mm_down+1, mm_left+1, mm_right+1,na.rm=T)
  mm  
}


dim <- dim(racetrack)[1]
mm <- matrix( Inf, nrow = dim, ncol=dim )
mm0 <- mm
mm[ racetrack =="S" ] <- 0
mm[ racetrack == "#" ] <- NA
pad <- rep( NA, dim)


while( any(mm0!=mm,na.rm=T ))
{
  mm0 <- mm
  mm[racetrack=="#"] <- NA
}

barriers <- which( is.na( mm ), arr.ind = T)
gap      <- numeric()
for( entry in 1:nrow( barriers ))
{
  bcoords <- barriers[entry,,drop=F]
  for( dir in list( c(1,0), c(0,1)))
  {
    if( any( bcoords-dir < 1 | bcoords+dir > dim )) next
    gap <- c( gap, abs(mm[bcoords + dir] - mm[bcoords-dir])-2)
  }
}

gap0 <- gap[ !is.na(gap)] 
table(gap0)
sum( gap0 >= 100)

which( racetrack != "#", arr.ind=T)

entry <- matrix( c(4,2), nrow=1)
walls <- matrix( Inf, nrow = dim, ncol=dim )
walls[ racetrack != "#" ] = NA



