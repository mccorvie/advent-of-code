#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 5
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

split = which( input == "")
ranges = input[1:(split-1)] |> unglue_data( "{low}-{high}", convert=T) |> as_tibble()

expand_grid( ranges, x = as.numeric( input[(split+1):length(input)])) |> 
  mutate( fresh = low <= x & x <= high) |> 
  group_by( x ) |> 
  summarize( fresh = any( fresh )) |> 
  summarize( sum( fresh ))


ranges = input[1:(split-1)] |> unglue_data( "{low}-{high}", convert=T) |> as_tibble()
ranges <- arrange( ranges, low)

consolidated <- tibble()
while( nrow( ranges ) > 0 )
{
  target <- ranges[1,]
  while( nrow( ranges )> 0 && min(ranges$low) <= target$high )
  {
    new_high    <- filter( ranges, low <= target$high) |> pull( high ) |> max()
    ranges      <- filter( ranges, low > target$high)
    target$high <- max( target$high, new_high )
  }
  consolidated <- consolidated |> bind_rows(target)
}

consolidated |> mutate( range = high-low+1) |> pull( range ) |> sum()
