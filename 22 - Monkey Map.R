#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day = 22
# adventr requests are being blocked
#raw  <- read_advent(day = day, year=2022) |> head(-1)
raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

mmap0 <- str_split( input[1:(length(input)-2)], "")
mmap <- map_dfr( 1:length( mmap0 ), \(row) tibble( row = row,  entry = mmap0[[row]] ) |> mutate( col = row_number() ) ) 
mmap <- mmap |> filter( entry != " ")
cmin <- mmap |> group_by( row ) |> summarize( cmin = min(col)-1 ) |> pull( cmin )
rmin <- mmap |> group_by( col ) |> summarize( rmin = min(row)-1 ) |> pull( rmin )
rmin
compass = c( ">", "v", "<", "^" )
compass_val = 0:3
names( compass_val ) = compass
dir = c( "<" = -1, ">" = 1, "v"  =1, "^" = -1, "L" = -1, "R" = 1, "X" = 0 )

idx_cycle = \( idx, len ) (idx-1) %% len +1
idx_cycle( 6:56, 15)


bb1 <- mmap |> group_by( row ) |> summarize( cmin = min(col), cmax = max( col )) 
bb2 <- mmap |> group_by( col ) |> summarize( rmin = min(row), rmax = max( row) ) 

input[199]
currow = 1
curcol = 1 + cmin[currow]
curdir = ">"
moves = str_extract_all( last( input ), "[:digit:]+")[[1]] |> as.numeric()
turns = str_extract_all( last( input ), "[RL]")[[1]] |> c( "X" )
curdir


cat( "(", currow, ", ", curcol, ") ", curdir, "\n")
last( moves)
first( turns)
for( movenum in 1:length( moves ))
{
  nmove = moves[movenum]

  cat( "move = ", nmove, "turn = ", turns[movenum])
  
  if( curdir == ">" || curdir == "<")
  {
    entries = mmap |> filter( row == currow ) |> pull( entry )
    path = curcol - cmin[ currow ] + dir[ curdir ] * (1:nmove)
    path = idx_cycle( path, length( entries ) )

    stopmove = match(  "#", entries[path], nomatch = length(path)+1) -1
    if( stopmove > 0 )
      curcol = path[ stopmove ] + cmin[ currow ]
  } else {
    entries = mmap |> filter( col == curcol ) |> pull( entry )
    path = currow - rmin[ curcol ] + dir[ curdir ] * (1:nmove)
    path = idx_cycle( path, length( entries ) )
    
    stopmove = match(  "#", entries[path], nomatch = length(path)+1) -1
    if( stopmove > 0 )
      currow = path[ stopmove ] + rmin[ curcol ]
  }
  
  turn <- turns[movenum]
  curdir <- compass[ idx_cycle( match( curdir, compass ) + dir[ turn ], 4 ) ]
  cat( " (", currow, ", ", curcol, ") ", curdir, "\n")
}
currow
curcol
1000 * currow + 4 * curcol + compass_val[curdir]
curdir

compass_val
# 125192