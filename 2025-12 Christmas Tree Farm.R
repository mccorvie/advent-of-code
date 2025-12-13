#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 12
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

block_sizes = c( 6,7,7,7,6,7) # real
block_sizes = c( 7,7,7,7,7,7) # example

tt<-tail( input, -30) |> 
  unglue_data( "{w}x{h}: {n1} {n2} {n3} {n4} {n5} {n6}", convert=T) |> 
  as_tibble() |> 
  mutate( 
    area = w*h,
    occluded = 7*(n1+n2+n3+n4+n5+n6),
    gap1 = area - occluded,
    n_block3x3 = ( w%/%3) * (h%/%3),
    n_block  = n1 + n2 +  n3 + n4 + n5 + n6,
    gap2 = n_block3x3 - n_block
  ) 

tt |> filter( gap2>=0) |> nrow()


