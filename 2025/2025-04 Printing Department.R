#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 4
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

ats <- which( str_split( input, "", simplify = T) == "@", arr.ind=T)

# pad edges with .
mm  <- matrix( ".", nrow=max(ats)+2, ncol = max(ats)+2)
ats <- ats+1  # shift rows and cols by 1
mm[ ats ] = "@"

mask = matrix( c( -1,-1,-1,0,0,1,1,1, -1,0,1, -1,1, -1,0,1), ncol=2)
accessible <- \( coords ) sum( mm[ mask + rep( coords,each=8) ] == "@" ) < 4

keep( 1:nrow(ats), \(nn) accessible( ats[nn,])) |> length() # part 1

n_ats0 <- nrow( ats )
old <- n_ats0+1
while( nrow( ats )< old)
{
  old <- nrow( ats )
  kk  <- keep( 1:nrow( ats ), \(nn) !accessible( ats[nn,]) )
  ats <- ats[kk,]
  mm  <- matrix( ".", nrow=max(ats)+2, ncol = max(ats)+2)
  mm[ ats ] = "@"
}

n_ats0 - nrow( ats  ) # part 2
