library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day12"
directions <-  str_match( readLines( file.path( dir, ff)), regex( "([A-Z])(\\d+)"))
magnitude  <- strtoi(directions[,3])
direction  <- directions[,2]

rotation_matrix <- function( dir, degrees )
{
  mm  <- matrix( c( 0,-1,1,0 ), nrow=2)
  if( dir=="R")
    mm <- mm * -1
  rot <- degrees / 90
  out <- diag(1,2) 
  if( rot >0)
    for( rot in 1:rot)
      out <- out %*% mm
  out
}

dir <- list( "N" = c( 0,1), "S" = c(0,-1), "E" = c(1,0), "W"=c(-1,0))

cur_dir <- c( 1,0)
cur_loc <- c(0,0)
for( idx in 1:length( direction))
{
  if( direction[idx]== "R" || direction[idx]=="L")
  {
    cur_dir <- cur_dir %*% rotation_matrix(  direction[idx], magnitude[idx]) 
  }
  else if( direction[idx]=="F")
  {
    cur_loc <- cur_loc + cur_dir * magnitude[idx]
  }
  else
  {
    cur_loc = cur_loc + dir[[direction[idx]]] * magnitude[ idx]
  }
  
  cat( directions[idx,1], cur_loc, "\n")
}

answer1<-sum( abs( cur_loc))
answer1

waypoint <- c(10,1)
cur_loc <- c(0,0)
for( idx in 1:length( direction))
{
  if( direction[idx]== "R" || direction[idx]=="L")
  {
    waypoint <- waypoint %*% rotation_matrix(  direction[idx], magnitude[idx]) 
  }
  else if( direction[idx]=="F")
  {
    cur_loc <- cur_loc + waypoint * magnitude[idx]
  }
  else
  {
    waypoint = waypoint + dir[[direction[idx]]] * magnitude[ idx]
  }
  
  cat( directions[idx,1], cur_loc, "\n")
}
answer2<-sum( abs( cur_loc))
answer2
waypoint
