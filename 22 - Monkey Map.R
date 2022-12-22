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

mmap0 <- str_split( input[1:(length(input)-3)], "")
mmap <- map_dfr( 1:length( mmap0 ), \(row) tibble( row = row,  entry = mmap0[[row]] ) |> mutate( col = row_number() ) ) 
mmap <- mmap |> filter( entry != " ")
# cboundary <- mmap |> group_by( row ) |> summarize( cmin = min(col), cmax = max( col ))
# rboundary <- mmap |> group_by( col ) |> summarize( rmin = min(row), rmax = max( row ))
mmap
mmap |> filter( row == 1) |> summarize( cmin = min(col)-1) |> pull( cmin)
cmin <- mmap |> group_by( row ) |> summarize( cmin = min(col)-1 ) |> pull( cmin )
cmin
rmin <- mmap |> group_by( col ) |> summarize( rmin = min(row)-1 ) |> pull( rmin )
compass = c( ">", "v", "<", "^" )
compass_val = 0:3
names( compass_val ) = compass
dir = c( "<" = -1, ">" = 1, "v"  =1, "^" = -1, "L" = -1, "R" = 1, "X" = 0 )

idx_cycle = \( idx, len ) (idx-1) %% len +1

currow = 1
curcol = cmin[currow] + 1
curdir = ">"
curdir

moves = str_extract_all( last( input ), "[:digit:]+")[[1]] |> as.numeric()
turns  = str_extract_all( last( input ), "[RL]")[[1]] |> c( "X" )

cat( "(", currow, ", ", curcol, ") ", curdir, "\n")

for( movenum in 1:length( moves ))
{
  nmove = moves[movenum]
  nmove
  
  cat( "move = ", nmove, "turn = ", turns[movenum])
  
  if( curdir == ">" || curdir == "<")
  {
    entries = mmap |> filter( row == currow ) |> pull( entry )
    entries
    path = curcol - cmin[ currow ] + dir[ curdir ] * (1:nmove)
    path = idx_cycle( path, length( entries ) )
    path
    curcol
    currow
    
    stopmove = match(  "#", entries[path], nomatch = length(path)+1) -1
    stopmove
    nmove
    path[stopmove]
    path[1]
    path
    if( stopmove > 0 )
      curcol = path[ stopmove ] + cmin[ currow ]
  } else {
    entries = mmap |> filter( col == curcol ) |> pull( entry )
    path = currow - rmin[ curcol ] + dir[ curdir ] * (1:nmove)
    path
    path = idx_cycle( path, length( entries ) )
    entries[path]
    stopmove = match(  "#", entries[path], nomatch = length(path)+1) -1
    path[stopmove]
    stopmove
    if( stopmove > 0 )
      currow = path[ stopmove ] + rmin[ curcol ]
  }
  
  turn <- turns[movenum]
  turn
  curdir
  curdir <- compass[ idx_cycle( match( curdir, compass ) + dir[ turn ], 4 ) ]
  cat( " (", currow, ", ", curcol, ") ", curdir, "\n")
  curcol
}

1000 * currow + 4 * curcol + compass_val[curdir]

curdir
curdir = ">"
dir[turn]
match( ">", compass)
turn = "R"
compass[ match( curdir, compass) + dir[turn]]

moves = str_extract_all( last( input ), "[:digit:]+")
turns = str_extract_all( last( input ), "[RL]")
