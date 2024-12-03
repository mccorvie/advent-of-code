# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 10
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "c" ))

use_test = F
input = if( use_test ) test else raw


pipes = str_split( input, "", simplify=T )
id_pipes <- \(template) pipes %in% template |> matrix( nrow = nrow( pipes ), ncol=ncol( pipes)) 

# from each square, what does it look like in a given direction?
peek_dir <- \( dir, mm, fill=F )
{
  d <- dim( mm)
  rows <- 1:d[1]
  cols <- 1:d[2]
  
  if( dir == "u" )      return( rbind( rep( fill, d[2] ), mm[head(rows,-1),]))
  else if( dir == "d" ) return( rbind( mm[tail(rows,-1),], rep( fill, d[2])))
  else if( dir == "r" ) return(  cbind( mm[,tail(cols,-1)], rep( fill, d[1])))
  else if( dir == "l" ) return( cbind( rep( fill, d[1]), mm[,head(cols,-1)]))
}

# Unwind all non-loops and replace them with empty ground
start <- pipes=="S"
while( T )
{
  pipe_u = id_pipes( c( "|", "L", "J" )  )
  pipe_d = id_pipes( c( "|", "F", "7" ) ) 
  pipe_r = id_pipes( c( "-", "L", "F" )  )
  pipe_l = id_pipes( c( "-", "J", "7" )  )
  
  not_loop <- pipe_u & !peek_dir( "u", pipe_d | start ) | 
    pipe_d & !peek_dir( "d", pipe_u | start ) | 
    pipe_l & !peek_dir( "l", pipe_r | start ) | 
    pipe_r & !peek_dir( "r", pipe_l | start ) 
  
  if( !any( not_loop )) break
  pipes[ not_loop] <- "."
}


## Part 1
dist <- matrix( Inf, nrow = nrow(pipes), ncol=ncol(pipes))
old  <- dist
dist[ start ] =0
while( any( old != dist ))
{
  old <- dist  

  dist[pipe_d] <- pmin( dist, peek_dir( "d", dist+1, Inf))[pipe_d]
  dist[pipe_u] <- pmin( dist, peek_dir( "u", dist+1, Inf))[pipe_u]
  dist[pipe_l] <- pmin( dist, peek_dir( "l", dist+1, Inf))[pipe_l]
  dist[pipe_r] <- pmin( dist, peek_dir( "r", dist+1, Inf))[pipe_r]

}

max( dist[ !is.infinite(dist)] )

## Part 2

pipes[start] = "F" # yours might be different
int_points <- function( row )
{
  # count the number of wall crosings from the exterior
  # odd crossings = interior, even crossings = exterior
  sep <- paste0( row, collapse="") |> 
    str_remove_all( "\\-") |> 
    str_remove_all( "(F7|LJ)") |> 
    str_replace_all( "(FJ|L7)", "|") |> 
    str_split_1( "")
  
  sum( cumsum( sep=="|")[ sep=="."]%%2)
}

1:nrow( pipes ) |> 
  map_dbl( \(idx) int_points( pipes[idx,])) |> sum()



##
## calculate start shape
##

shape <- list(
  "1100" = "|",
  "1010" = "J",
  "1001" = "L",
  "0110" = "7",
  "0101" = "F",
  "0011" = "-"  
)

key <- c( 
  any( start & peek_dir( pipe_d, "u")),
  any( start & peek_dir( pipe_u, "d")),
  any( start & peek_dir( pipe_r, "l")),
  any( start & peek_dir( pipe_l, "r"))
) |> as.numeric() |> paste0( collapse="")

start_shape = shape[[key]]
start_shape

