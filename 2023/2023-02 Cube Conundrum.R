# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 2
raw <- read_advent(day = day, year=2023) |> head(-1)

# raw  <- readLines( paste0( "input", day ))
# test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

games <- raw |> str_split_i( ":", 2 )
max_allowed <- c( red = 12, green=13, blue=14)

highest_seen <- \(game_seq0 )
{
  game_seq <- game_seq0 |> str_split_1( ";") |> str_split( ",") 
  quant <- game_seq |>  map( \(x) str_extract( x,"(\\d)+" ) |> as.numeric())
  type  <- game_seq |>  map( \(x) str_extract( x,"([a-z]+)" ) )
  
  highest_seen <- c( red = 0, green = 0, blue= 0)
  for( i in seq_along( game_seq))
    highest_seen[ type[[i]]] <- pmax( quant[[i]], highest_seen[ type[[i]] ] )

  highest_seen
}

# part 1
allowed <- map( games, highest_seen) |> map_lgl( \(x) all( x <= max_allowed))
(seq_along( games ) * allowed) |>  sum()

# part 2
map( games, highest_seen) |>map_dbl( prod ) |> sum()

