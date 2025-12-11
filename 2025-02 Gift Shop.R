#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 2
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

## part 1
maxn <- 99999
invalid_id <- paste0( 1:maxn, 1:maxn) |> as.numeric()

## part 2
invalid_id <- character()
for( times in 2:10)
{
  maxn = 10^(floor(10/times))-1
  invalid_id <-c( invalid_id, map_chr( 1:maxn, \(x) paste0(rep( x, times ), collapse="")))
}
invalid_id <- invalid_id |> as.numeric() |> unique()


mm <- input |> str_split_1( ",") |> str_split( "-", simplify=T) |> apply( 2, as.numeric)
colnames( mm) = c("low", "high")
as_tibble( mm ) |> 
  rowwise() |> 
  mutate( ss = sum(invalid_id[ (low <= invalid_id) & (invalid_id <=high) ] )) |> 
  ungroup() |> 
  summarize( sum(ss))

