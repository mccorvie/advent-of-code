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

test <- readLines( paste0( "test", day, "a" ))

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



push_boxes_big <- \( warehouse, pos, dpos )
{
  
  warehouse0 <- 666
    
  
}

input_gap <- which( input == "")
warehouse <- input[1:(input_gap-1)] 
warehouse |> str_replace_all( "#","##") |> str_replace_all( "O", "[]")|> 
  str_replace_all( "\\.", "\\.\\.") |> str_replace_all( "@", "@\\.") |> str_split( "", simplify = T)
dim       <- dim(warehouse)[1]
pos       <- which( warehouse== "@", arr.ind=T) 
warehouse[pos] <- "."

for( move in moves )
{
  warehouse0 <- push_boxes_big(warehouse,pos,dpos)
  if( any(is.na( warehouse0 ))) next
  warehouse <- warehouse0
  pos <- pos + dpos  
}

