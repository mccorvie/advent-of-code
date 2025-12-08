#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 1
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "a" ))

use_test = F
input = if( use_test ) test else raw

turns <- input |> str_replace( "L","-") |> str_replace( "R","+") |> as.numeric()
sum( cumsum( c( 50, turns))%% 100==0 )


zero_passes <- \( start, change )
{
  whole_spins <- change %/% 100
  change  <- change %% 100
}

-920 %/% 100
