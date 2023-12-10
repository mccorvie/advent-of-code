# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( pracma)
library( unglue)

options(digits=20)

day <- 9
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

library( pracma)

xx <- 1 # part 1
xx <- 0 # part 2

str_extract_all( input, "(\\-?\\d+)") |> map( as.numeric ) |> 
  map_dbl( \( seq ) newtonInterp( seq_along( seq) / (length(seq)+1), seq, xx )) |> 
  sum() |> round()

##
## the thing I actually used to solve the problem below
##

extrap_seq <- \(seq,back=F)
{
  ends <- integer()
  while( any( seq!=0 ))
  {
    ll   <- length( seq )
    ends <- c( ends, seq[ll])
    starts <- c( starts, seq[1])
    seq  <- seq[2:ll] - seq[1:(ll-1)]
    seq
  }
  
  if( back )
  {
    signs <- -1*(-1)^(1:length(starts))
    return( sum( starts*signs))
  }
  
  sum(ends)
}

seqs <-str_extract_all( input, "(\\-?\\d+)") |> 
  map( as.numeric ) 

# part 1
seqs |> 
  map_dbl( extrap_seq) |> 
  sum()


seqs |> 
  map_dbl( \(v) extrap_seq( v,T)) |> 
  sum()

