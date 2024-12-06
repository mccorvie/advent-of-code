#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 4
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw



puzzle <- input |> str_split( "", simplify=T)
puzzle_size <- nrow( puzzle)

count <- 0
for( r in 1:puzzle_size)
  for( c in 1:puzzle_size)
    for( dr in (-1):1)
      for( dc in (-1):1)
      {
        if( dr == 0 && dc == 0 ) next
        indices <- map(0:3, \(n) c(r,c) + n*c(dr,dc)) |> reduce( rbind)
        if( any( indices < 1 ) || any( indices > puzzle_size )) next
        if( "XMAS" == paste0(puzzle[indices], collapse=""))
          count <- count+1
      }
count  # part1 


stroke1 <- matrix( c( -1,-1, 0,0,1,1), ncol=2, byrow=T)
stroke2 <- matrix( c( -1,1, 0,0,1,-1), ncol=2, byrow=T)
strokes <- list( stroke1, stroke2 )

stroke_word <- \( puzzle, r, c, stroke )
{
  indices <- matrix( c(r,c), nrow=3, ncol=2, byrow=T) + stroke 
  if( any( indices < 1 ) || any( indices > puzzle_size )) return("XXX")
  paste0(puzzle[indices], collapse="")
}

count<-0
for( r in 1:puzzle_size)
  for( c in 1:puzzle_size)
  {
    words <- map( strokes, \(ss) stroke_word(puzzle,r,c,ss))
    if( all(words == "MAS" | words == "SAM"))
      count <- count+1
  }
count # part2



