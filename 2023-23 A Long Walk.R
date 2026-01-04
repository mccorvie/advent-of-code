#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 23
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = T
input = if( use_test ) test else raw

MM <- input |> str_split( "", simplify=T)
dir <- list( "<" = c(0,-1), ">" = c(0,1), "v" = c(1,0),"^"= c(-1,0))
search <- \( pos, last_pos, depth=1 )
{
  cat( depth, " ")
  
  #cat( pos[1], ",", pos[2],  "->", MM[pos[1],pos[2]],"\n")
  if( pos[1]==nrow( MM)) 
    return( 1 )

  neighbors <- map( dir, \(dd) pos + dd) |> 
    keep( \(x) any(x != last_pos)) |> 
    keep( \(x) MM[x[1],x[2]] != "#")

  slopes <- neighbors |> 
    keep( \(x) MM[x[1],x[2]] != "." ) |> 
    keep( \(x) any( pos-x != dir[[MM[x[1],x[2]]]])) |> 
    map( \(nn) search( nn, pos, depth+1)) |> 
    unlist( recursive = F )

  paths <- neighbors |> keep( \(x) MM[x[1],x[2]] == "." ) |> 
    map( \(nn) search( nn, pos, depth+1 )) |> 
    unlist( recursive = F )

  names( paths)  <- NA
  names( slopes) <- NA
  c( slopes, paths )+1
}

pos <- c(2,2)
last_pos <- c( 1,2)

out <- search( pos, last_pos)

((names( out) |> map_dbl( str_length))+1)/2
out
names( out)<- NULL
out
sort(out)

nrow(MM)
ncol(MM)



endcol = which( MM_str[nrow(MM_str),] == ".")
offsets <- matrix( c( 1,-1,0,0, 0,0,1,-1), ncol=2)


maze <- expand_grid( row = 1:nrow( MM), col = 1:ncol( MM)) |> 
  rowwise() |> 
  mutate( entry = MM[row,col] ) |> 
  ungroup() |> 
  mutate(
    dist = if_else( entry == "#", -Inf, NA ),
    dist = if_else( row == max(col) & entry == ".", 0, dist )
  ) 


search0 <- \( pos, last_pos )
{
  cat( pos[1], ",", pos[2],  "->", MM[pos[1],pos[2]],"\n")
  neighbors <- map( dir, \(dd) pos + dd) |> 
    keep( \(x) any(x != last_pos)) |> 
    keep( \(x) MM[x[1],x[2]] != "#")
  
  out       <- neighbors |> keep( \(x) MM[x[1],x[2]] != "." ) 
  remainder <- neighbors |> keep( \(x) MM[x[1],x[2]] == "." ) |> map( \(nn) search( nn, pos )) |> unlist( recursive=F)
  
  c( out, remainder )
}



l1<-list( "a"=1,"b"=2)
l2<-list( "a"=3,"b"=4)
c( l1, l2)

offset <- tibble( 
  match = c( ".",".",".",".",">","<","v","^"),
  dr    = c( 1,-1,0,0,0,0,1,-1 ),
  dc    = c( 0,0,1,-1,1,-1,0,0),
)

xx <- left_join( maze, offset, by = join_by(entry==match),relationship = "many-to-many") |> 
  mutate( newcol = col + dc, newrow = row+dr) |> 
  left_join( select( maze, row, col, ndist = dist ), by = join_by( newcol == col, newrow == row )) |> 
  group_by( row, col, dist, entry ) |> 
  summarize( ndist = max(ndist )+1)

filter( xx, dist != ndist)  

max( NA, na.rm = T)
max( c( 4,-Inf, 5) )

maze

MM <- matrix( NA, nrow = nrow( MM_str), ncol = ncol( MM_str))
MM[ MM_str == "#"] = Inf
endcol = which( MM_str[nrow(MM_str),] == ".")
MM[nrow( MM), endcol] = 0

MM_old <- MM
offsets <- matrix( c( 1,1,-1,-1,1,-1,1,-1), ncol=2)
offsets + rep(c(5,6),each=4) |> matrix( ncol=2)

while( MM != MM_old)
{
  
  
  
}