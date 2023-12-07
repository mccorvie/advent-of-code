# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 7
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input0 = if( use_test ) test else raw


#substitution mapping so that regular lex sort on strings works
card_sub <- c( "A"="E", "K"="D", "Q"="C", "J"="B", "T"="A") 
# uncomment for part 2 to make "J" mean "Joker"
#card_sub[ "J"] = "0"  

hand_type <- \( hh )
{
  if( hh == "00000") return( "5")
  
  hh_v <- hh |> str_split_1( "" ) 
  numj <- coalesce( table( hh_v )["0"], 0 ) # how many Jokers?

  chr <- hh_v |> 
    keep( \(c) c != "0") |> 
    table() |> 
    sort( decreasing= T) 

  chr[1] <- chr[1] + numj # add Jokers to largest card category
  
  paste0( chr, collapse= "" )
}

input2 <- str_split( input, " ", simplify=T )
tibble( hand = input2[,1], bid=input2[,2]) |> 
  mutate( 
    bid = as.numeric( bid),
    hand_std  = str_replace_all( hand, card_sub ),
    hand_char = map_chr( hand_std, hand_type  ) 
  ) |> 
  arrange( hand_char, hand_std) |> 
  mutate( rank = 1:n(), win = rank*bid ) |> 
  summarize(sum(win))





