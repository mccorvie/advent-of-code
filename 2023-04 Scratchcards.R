# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 4
raw <- read_advent(day = day, year=2023) |> head(-1)

# raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw


nums <- str_split_i( input, ":",2)
win   <- str_split_i( nums, "\\|",1) |> 
  str_extract_all( "(\\d)+") |> map( \(x) as.numeric(x))
mine  <- str_split_i( nums, "\\|",2) |> 
  str_extract_all( "(\\d)+") |> map( \(x) as.numeric(x))

num_match <- map2_dbl( win, mine, \(ww, mm) length( intersect( ww,mm ))) 

# part 1
num_match |> 
  keep( \(mm) mm > 0 ) |> 
  map_dbl( \(mm) 2^(mm-1)) |> 
  sum()

# part 2
ll <- length(input)
num_cards <- rep( 1, ll )
for( idx in 1:(ll-1))
{
  if( num_match[idx]==0) next
  
  prize_start <- idx+1
  prize_end   <- min( idx+num_match[idx], ll )
  num_cards[ prize_start:prize_end] <- num_cards[ prize_start:prize_end] + num_cards[idx]
}
sum(num_cards)

