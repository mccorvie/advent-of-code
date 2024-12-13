#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

printarg <- \(ff)
{
  \(xx)
  {
    cat( xx, "\n")
    ff(xx)
  }
}


day <- 12
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day,"e" ))

use_test = F
input = if( use_test ) test else raw

garden <- input |> map( \(x) str_split_1( x,"")) |> reduce( rbind)

dim <- dim(garden)[1]
inc <- list( c(1,0), c(-1,0), c( 0,1), c(0,-1))

shift_diff <- \( coord_set, shift_dir )
{
  coord_set |> map( \(xx) round(xx+shift_dir)) |> setdiff( coord_set ) 
}

coords <- map( 1:nrow(garden), \(r) map( 1:ncol(garden), \(c) round(c(r,c)))) |> reduce( c)
price1  <- 0 
price2  <- 0 
while( length( coords ) > 0)
{
  region  <- list( coords[[1]] )
  label   <- garden[ region[[1]][1], region[[1]][2] ]
  oldarea <- 0
  while( length( region ) > oldarea )
  {
    oldarea <- length( region )
    region <- inc |> map( \(dd) map( region, \(rr) round(rr + dd) )) |> 
      reduce( c ) |> intersect( coords ) |> 
      keep( \(xx) garden[xx[1],xx[2]]==label ) |> union(region)
  }
  
  perims <- inc |> map( \(dd) shift_diff( region, dd))
  sides  <- map2( perims, rev( inc ), shift_diff )

  area   <- length( region )
  perim  <- perims |> map_dbl( length ) |> sum()
  sides  <- sides  |> map_dbl( length ) |> sum()
  price1 <- price1 + area * perim
  price2 <- price2 + area * sides
  
  coords <- setdiff( coords, region)
}
price1
price2
