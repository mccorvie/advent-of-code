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

tt <- tibble( input = input ) |> 
  mutate( 
    turn = input |> str_replace( "L","-") |> str_replace( "R","+") |> as.numeric(),
    cur = cumsum( c( 50, head(turn,-1 ))),
    shift = (turn < 0) * sign(turn),
    span   = abs( (cur+shift) %/% 100 - (cur+turn+shift)%/%100 ),
    touch  = (cur+turn) %% 100 == 0 
  ) 

tt |> pull( touch ) |> sum() # part1
tt |> pull(span) |> sum() #part2
