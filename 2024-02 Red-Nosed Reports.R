#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 2
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw


input <- input |> map( \(x) as.numeric( str_split_1( x, " ")))

safe <- \(row)
{
  dd <- diff( row)
  monotonic <- all( sign( dd)==1)||all( sign( dd)==-1)
  small_inc <- all( abs(dd) <=3 ) && all( abs(dd) >= 1)
  monotonic && small_inc  
}

#part 1
input |> map_dbl( safe ) |> sum()


safe_damp <- \(row)
{
  for( omit_i in (-1):(-length(row)))
    if( safe( row[omit_i])) return( T )

  F
}


input |> map_dbl( safe_damp ) |> sum()
