#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

raw <- read_advent(day = 16, year=2022) |> head(-1)
test <- readLines( "test16" )

input<- raw
input <- test

patterns <- c( "Valve {valve} has flow rate={rate}; {nv} to {vv} {tunnels}" )

plan <- unglue::unglue_data( input, patterns, convert=T)
rate <- plan |> pull( rate , name= valve )
good_valves <- names( rate )[rate >0] |> sort()
adj <- plan |> 
  mutate( tunnel = str_split( tunnels, ", ")) |> 
  unnest( tunnel ) |> 
  select( valve, tunnel) |> 
  as.matrix()

# make shortest distance matrix between good valves
dd <- matrix( Inf, nrow=nrow( plan), ncol=nrow( plan), dimnames= list( pull( plan, valve),pull( plan, valve)))
dd[adj] <-1
dd[ diag( T, nrow( dd)) ] = 0
while( any( dd == Inf))
  walk( 1:nrow( dd), \(i) walk( 1:ncol(dd), \(j) dd[i,j] <<- min( dd[i,] + dd[,j])))

evaluate_path <- function( path )
{  
  if( runif(1) < 1/1000 ) cat( path, "\n")
  time_opened <- cumsum(map2_dbl( head(path,-1), tail(path, -1), ~ dd[.x,.y])+1)
  sum( (max_t-time_opened) * rate[path[-1]])
}

get_length <- \( path ) sum( map2_dbl( head(path,-1), tail(path, -1), ~ dd[.x,.y]) ) + length( path )-1

follow_path <- function( path )
{
  if( get_length( path) >=max_t ) return( 0 )
  pressure <- evaluate_path( path )
  if( part2 ) cache_path( path, pressure ) 
  max( pressure, map_dbl( setdiff( good_valves, path ), ~ follow_path( c( path, .))))
}
part2 = F
max_t <- 30
follow_path( "AA" ) # part1


cache_path <- function( path, pressure )
{
  key = paste0( sort( path ), collapse= "-")
  valveset_best[ key ] <<- max( pressure, coalesce( valveset_best[key],0))
}

evaluate_combo <- \(idx1, idx2)
{
  if( length( intersect( valveset[[idx1]], valveset[[idx2]]))>1)
    return(0)
  valveset_best[idx1] + valveset_best[idx2]
}

part2 <- T
max_t <- 26
valveset_best = c( "AA" = 0 )
follow_path( "AA" )

valveset <- str_split( names( valveset_best), "-")
combo_best <- 0
for( idx1 in 1:(length( valveset )-1))
  for( idx2 in (idx1+1):length( valveset ))
    combo_best <- max( combo_best, evaluate_combo( idx1, idx2 ))
combo_best # part2 

