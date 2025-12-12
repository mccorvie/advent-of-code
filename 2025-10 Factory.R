#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 10
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

fewest_presses <- \(line)
{
  indicator <- str_extract( line, "\\[(.*)\\] (.*) \\{(.*)\\}", group=1) |> str_split_1("") 
  indicator <- (indicator == "#" ) |> as.numeric()
  nl = length( indicator )
  
  buttons0 <- str_extract( line, "\\[(.*)\\] (.*) \\{(.*)\\}", group=2) |> str_split_1( " ") 
  nb = length( buttons0 )
  
  button_coord <- buttons0 |> 
    map( \(t) eval(parse(text = paste0( "c", t, "+1" )))) |> 
    map2( 1:nb, \(r,c) matrix( c( r,rep( c, length(r))), ncol=2)) 
  button_coord <- do.call( rbind, button_coord )
  
  buttons <- matrix( 0, nrow = nl, ncol = nb )
  buttons[ button_coord ] = 1
  
  joltage   <- str_extract( line, "\\[(.*)\\] (.*) \\{(.*)\\}", group=3) |>  str_split_1(",") |> as.numeric() 
  
  
  for( npress in 1:nb )
  {
    idx_design <- combn( nb, npress ) 
    pressed <- matrix( 0, ncol=ncol( idx_design), nrow = nb )
    for( c in 1:ncol( idx_design ))
      pressed[idx_design[,c], c ] = 1
    
    result <- apply( buttons %*% pressed %% 2 -indicator, 2, \(x) sum( abs(x)))
    if( any( result < 1e-3 )) 
      return( npress )
    
  }
  NA 
}

map_dbl( input, fewest_presses) |> sum()

