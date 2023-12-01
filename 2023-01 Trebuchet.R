if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <-1
raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw




ff <- \(x) x^2


# part 1

map_chr( input, \(x) str_remove_all( x,"[a-z]")) |> 
  map_chr( \(x) paste0(  str_sub(x, 1,1), str_sub(x, -1,-1))) |> 
  as.numeric() |> 
  sum()


# part 2
calib_value <- function( num_str )
{
  digits <- c( "one", "two", "three", "four", "five", 
             "six", "seven","eight", "nine", "###", as.character(1:9) )

  xx <- num_str |> str_locate_all( digits )
  
  first_loc <- map_dbl( xx, \(ll) min( ll[,"start"]) ) 
  last_loc  <- map_dbl( xx, \(ll) max( ll[,"start"]) ) 
  
  dig1 <- match( min(first_loc), first_loc ) %% 10
  dig2 <- match( max(last_loc), last_loc ) %% 10
  return( 10*dig1+dig2)
}  

map_dbl( input, calib_value ) |> sum()
  