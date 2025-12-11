#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 3
raw <- read_advent(day = day, year=2025) |> head(-1)

# raw  <- readLines( paste0( "input", day ))
test <- readLines( sprintf("test%02da", day))


use_test = F
input = if( use_test ) test else raw

max_joltage2 <- \(line)
{
  js   <- str_split_1( line, "" )
  idx1 <- first( which( js == max(head(js,-1))))
  js2  <- tail( js, -idx1)
  idx2 <- first( which( js2 == max(js2)))+idx1
  as.numeric( paste0( js[idx1], js[idx2]))
}
map_dbl( input, max_joltage2) |> sum()


n_battery = 2  # part 1
n_battery = 12 # part 2

max_joltage_n <- \(line,nn=n_battery)
{
  js <- str_split_1( line, "" )
  js0 <- js
  idx <- numeric()
  for( remaining in nn:1)
  {
    ll <- length( js ) - remaining+1
    idx <- c( idx, first( which( js == max( js[1:ll] ))))
    js  <- tail( js, -last( idx ))
  }
  as.numeric( paste0( js0[cumsum(idx)], collapse=""))
}

map_dbl( input, max_joltage_n) |> sum()


