#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 7
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

pp      <- input |> map( \(ss) str_split_1( ss, " ") )
testvals <- pp |> map_dbl( \(xx) xx[1] |> str_sub( end=-2) |> as.numeric())
term_lists <- pp |> map( \(xx) xx[-1] |> as.numeric()) 



find_expr <- \( testval, term_list )
{
#  cat( testval, "?=", term_list, "\n")
  if( length( term_list) ==1 )
    return( testval == term_list )
  
  last_term  <- tail( term_list, 1 )
  terms_left <- head( term_list,-1)
  dig_len    <- str_length( as.character(last_term))
  
  if( part2 && 
      str_ends( as.character( testval), as.character(last_term)) &&
      find_expr( testval %/% ( 10^dig_len ), terms_left))
    return( T )
  
  if( (testval %% last_term == 0) && 
      find_expr( testval %/% last_term, terms_left ))
    return( T )
      
  testval > last_term && find_expr( testval - last_term, terms_left )

}


# testval = testvals[5]
# term_list = term_lists[[5]]
# find_expr( testval, term_list)

part2 <<- F
found <- map2_lgl( testvals, term_lists, find_expr ) 
sum( found * testvals) #part 1

part2 <<- T
found <- map2_lgl( testvals, term_lists, find_expr ) 
sum( found * testvals) #part 1


