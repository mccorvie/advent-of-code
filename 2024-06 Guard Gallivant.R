#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 6
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = T
input = if( use_test ) test else raw


# 
# tt <- input |> map( \(ss) tibble( map = str_split_1( ss, ""), r=1:str_length(ss) )) |> 
#   map2( as.list( 1:10), \(tt, idx) mutate( tt, c=idx)) |> 
#   reduce( add_row)


rotate_90 <- \(mat) t(mat)[nrow(mat):1,]

trace_route <- \(mm)
{
  visited0 <- matrix( F, nrow=nrow(mm), ncol=ncol(mm))
  visited <- mm == "^"
  mm[visited] <- "." 
  pos         <- which( visited, arr.ind=T)[1,] |> as.list()
  cnt<-0
  while( T )
  {
    col     <- mm[,pos$col]
    segment <- cumsum( col !=".")
    trip    <- (1:nrow( mm))<=pos$row & ( col == "." ) & (segment == segment[pos$row])
    pos$row <- first( (1:nrow(mm))[trip])

    if( visited0[ pos$row, pos$col ] )
      return( list( visted = visited, visted0=visited0, mm = mm ) )

    visited[ trip, pos$col] <- T
    visited0[ pos$row, pos$col ] <- T
    
    if( pos$row ==1)
      return( sum( visited ))
    
    
    mm       <- rotate_90( mm )
    visited  <- rotate_90( visited )
    visited0 <- rotate_90( visited0 )
    pos      <- list( row = nrow(mm)+1-pos$col, col = pos$row )
  }
  visited
}

mm0 <- input |> map( \(ss) str_split_1( ss, "") ) |> reduce( rbind )

part1 <- trace_route(mm0)
part1

part2 <- 0
for( r in 1:nrow(mm))
  for( c in 1:ncol(mm))
  {
    if( mm0[r,c] != ".")
      next
    mm <- mm0
    mm[r,c] <- "O"
    if( trace_route(mm) < 0 )
    {
      cat( r, ",", c, "\n")
      part2 <- part2+1
    }
  }


mm <- mm0
mm[2,6] <- "O"
tt <- trace_route(mm) 
tt  

part2
