#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 11
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

pebble_vec <- str_split_1( input, " ") 

inc_pebble <- \( pebbles, pebble, val=1 )
{
  if( is.null( pebbles[[pebble]]))
    pebbles[[pebble]] = 0
  pebbles[[pebble]] = pebbles[[pebble]]+val
  pebbles  
}

pebbles = list()
for( pebble in pebble_vec)
  pebbles = inc_pebble( pebbles, pebble )

pebbles
blinks = 25 #part 1
blinks = 75 #part 2

for( blink in 1:blinks)
{
  pebbles2 = list()
  for( pebble in names( pebbles ))
  {
    num = pebbles[[pebble]]
    if( pebble == "0") {
      pebbles2 = inc_pebble( pebbles2, "1", num )
    } else if( str_length(pebble) %% 2 == 0 ) {
      left  = str_sub( pebble, 1, str_length( pebble) %/% 2 )
      right = str_sub( pebble, str_length( pebble) %/% 2+1, str_length( pebble) ) |> as.numeric() |> as.character()
      pebbles2 = inc_pebble( pebbles2, left, num )
      pebbles2 = inc_pebble( pebbles2, right, num )
    } else {
      pebbles2 = inc_pebble( pebbles2, as.character( as.numeric( pebble) * 2024 ), num )
    }
  }
  pebbles2
  pebbles = pebbles2
  cat( blink, "->", sum(unlist(pebbles)), "\n")
}

