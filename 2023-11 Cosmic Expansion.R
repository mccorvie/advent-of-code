# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 11
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw
image <- str_split( input, "", simplify = T) == "#"

empty_x <- !apply( image, 1, any )
empty_y <- !apply( image, 2, any )
coords  <- which( image, arr.ind=T) 

scale <- 2 # part 1
scale <- 1000000 # part 2

crossing( g1= 1:nrow( coords), g2=1:nrow(coords)) |> 
  filter( g1<g2) |> 
  rowwise() |> 
  mutate( 
    manhattan = sum(abs(coords[g1,]-coords[g2,])), 
    extra_x   = (scale-1)*sum( empty_x[seq(coords[g1,1],coords[g2,1])]),
    extra_y   = (scale-1)*sum( empty_y[seq(coords[g1,2],coords[g2,2])]),
    dist      = manhattan + extra_x + extra_y
  ) |> 
  ungroup() |> 
  summarize( sum = sum( dist ))
    


