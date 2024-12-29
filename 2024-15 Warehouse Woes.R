#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 15
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "b" ))

use_test = F
input = if( use_test ) test else raw

print_warehouse <- \(warehouse, pos)
{
  warehouse[pos] <- "X"
  warehouse |> apply(  1, \(x) paste0( x, collapse="") ) |> paste( collapse="\n") |> cat()
  cat("\n\n")
}

input_gap <- which( input == "")
warehouse <- input[1:(input_gap-1)] |> str_split( "", simplify = T)
dim       <- dim(warehouse)[1]
pos       <- which( warehouse== "@", arr.ind=T) 
warehouse[pos] <- "."

moves     <- input[(input_gap+1):length(input)] |> str_split( "") |> unlist()
moves
dpos_map  <- list( "^" = c( -1,0), ">" = c(0,1), "v"= c(1, 0), "<" = c(0,-1))

push_boxes <- \(warehouse, pos, move)
{
  dpos   <- dpos_map[[move]]
  
  coords <- map( 1:dim, \(n) pos+n*dpos ) |> keep( \(v) all( v <= dim & v >=1)) |> reduce( rbind)
  path_to_edge <- warehouse[coords]
  gap    <- which( path_to_edge == "." ) |> first()
  if( is.na( gap )) return( NA )
  if( gap == 1)     return( warehouse )

  box_stack <- path_to_edge[1:(gap-1)]
  if( any( box_stack == "#" )) return( NA )
  warehouse[ coords[1:gap,,drop=F]] <- c( ".", box_stack )
  return( warehouse )
}


for( move in moves )
{
#  print_warehouse( warehouse, pos )
  warehouse0 <- push_boxes(warehouse,pos,move)
  if( any(is.na( warehouse0 ))) next
  warehouse <- warehouse0
  pos <- pos + dpos_map[[move]]  
}

print_warehouse( warehouse, pos )

box_loc <- which( warehouse == "O", arr.ind=T) 
sum(100*box_loc[,1] + box_loc[,2]-101)

other_half_dpos <- list( "[" = c(0,1), "]"= c(0,-1))
other_half      <- list( "[" = "]", "]" = "[")

push_boxes_big <- \( warehouse, pos, move, first=F)
{
  #first<-T
  dpos   <- dpos_map[[move]]
  coords <- map( 1:max(dim),\(x) pos+x*dpos) |> 
    keep( \(v) all(all(v<=dim) & all(v >= c(1,1)))) |> 
    reduce( rbind)
  coords
  path_to_edge <- warehouse[coords]
  gap    <- which( path_to_edge == "." ) |> first()
  if( is.na( gap )) return( NA )
  if( gap == 1)     return( warehouse )
  
  box_stack <- path_to_edge[1:(gap-1)]
  if( any( box_stack == "#" )) return( NA )

  
  if( move == "^" || move =="v")
  {
    first_box <- box_stack[1]
    first_box
    if( first )
    {
      warehouse <- push_boxes_big( warehouse, pos + other_half_dpos[[first_box]], move )
      if( any( is.na(warehouse))) return( NA )
    }
    
    half_boxes <- which( box_stack != lag(box_stack, default = box_stack[1]))
    for( half_box in half_boxes )
    {
      push_pos  <- pos + other_half_dpos[[ box_stack[half_box] ]] + (half_box-1)*dpos
      warehouse <- push_boxes_big( warehouse, push_pos, move )
      if( any( is.na(warehouse))) return( NA )
    }
  }
  
  warehouse[ coords[1:gap,,drop=F]] <- c( ".", box_stack )
  
  return( warehouse )
}


sanity <- \(warehouse)
{
  box_loc <- which( warehouse == "[", arr.ind=T) 
  box_loc[,2] = box_loc[,2]+1
  if( any( warehouse[ box_loc] != "]"))
    return( F )

  box_loc <- which( warehouse == "]", arr.ind=T) 
  box_loc[,2] = box_loc[,2]-1
  if( any( warehouse[ box_loc] != "["))
    return( F )

  T  
}

input_gap <- which( input == "")
warehouse <- input[1:(input_gap-1)] 
warehouse <- warehouse |> str_replace_all( "#","##") |> str_replace_all( "O", "[]")|> 
  str_replace_all( "\\.", "\\.\\.") |> str_replace_all( "@", "@\\.") |> str_split( "", simplify = T)
dim       <- dim(warehouse)
pos       <- which( warehouse== "@", arr.ind=T) 
warehouse[pos] <- "."
moves     <- input[(input_gap+1):length(input)] |> str_split( "") |> unlist()
length(moves)
moves


for( move in moves )
{
   cat( "move ", move, "\n")
   print_warehouse( warehouse, pos )
   
   if( !sanity(warehouse)) error("not sane")
   
#  move <- moves[6]
  
  warehouse0 <- push_boxes_big(warehouse,pos,move,T)
  if( any(is.na( warehouse0 ))) next
  warehouse <- warehouse0
  pos <- pos +  dpos_map[[move]] 
}
print_warehouse( warehouse, pos )


box_loc <- which( warehouse == "[", arr.ind=T) 
sum(100*box_loc[,1] + box_loc[,2]-101)
