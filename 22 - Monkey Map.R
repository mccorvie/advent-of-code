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

use_test = T
sidedim = 4

use_test= F
sidedim = 50

input = if( use_test ) test else raw

mmap0 <- str_split( input[1:(length(input)-2)], "")
mmap <- map_dfr( 1:length( mmap0 ), \(row) tibble( row = row,  tile = mmap0[[row]] ) |> mutate( col = row_number() ) ) 
mmap <- mmap |> filter( tile != " ") |> mutate( name = paste0( (row -1)%/% sidedim+1, (col-1) %/% sidedim+1 ) )

dist = str_extract_all( last( input ), "[:digit:]+")[[1]] |> as.numeric()
turn = str_extract_all( last( input ), "[RL]")[[1]] |> c( "X" )


atlas_test <- tribble(
  ~name, ~boxr, ~boxc, ~`>`, ~`>dir`, ~`v`, ~`vdir`,~`<`, ~`<dir`,~`^`,~`^dir`,
  "13", 1,3, "34", "<", "23", "v", "22", "v", "21", "v",
  "21", 2,1, "22", ">", "33", "^", "34", "^", "13", "v",
  "22", 2,2, "23", ">", "33", ">", "21", "<", "13", ">",
  "23", 2,3, "34", "v", "33", "v", "22", "<", "13", "^",
  "33", 3,3, "34", ">", "21", "^", "22", "^", "23", "^",
  "34", 3,4, "13", "<", "21", ">", "33",  "<", "23", "<"
)

atlas_prod <- tribble(
  ~name, ~boxr, ~boxc, ~`>`, ~`>dir`, ~`v`, ~`vdir`,~`<`, ~`<dir`,~`^`,~`^dir`,
  "12", 1,2, "13", ">", "22", "v", "31", ">", "41", ">",
  "13", 1,3, "32", "<", "22", "<", "12", "<", "41", "^",
  "22", 3,3, "13", "^", "32", "v", "31", "v", "12", "^",
  "31", 3,1, "32", ">", "41", "v", "12", ">", "22", ">",
  "32", 3,2, "13", "<", "41", "<", "31", "<", "22", "^",
  "41", 4,1, "32", "^", "13", "v", "12", "v", "31", "^",
)

atlas <- atlas_prod

rotR = c( ">" = "v", "v" = "<", "<" = "^", "^" = ">")
dir_code  <- c( ">" = 0, "v"=1, "<"=2, "^"=3)

get_face <- \( name, dir )
{
  rot_func <- list( `>` = \(m) m, `v` = \(m) t( m[,sidedim:1]), `<` = \(m) m[sidedim:1, sidedim:1], `^` = \(m) t(m[sidedim:1,]))
  face <- mmap |> filter( name == !!name ) |> arrange( col,row ) |> pull( tile ) |>  matrix( nrow = sidedim)
  rot_func[[dir]]( face )
}
unique( mmap$name)

curface <- min( mmap$name)
curcol <- currow <- 1
curdir <- ">"

for( nn in 1:length( dist))
{
  cat( curface, " ", curdir, "\n")
  
  adjface <- atlas |> filter( name == curface ) |> pull( curdir  )
  adjdir  <- atlas |> filter( name == curface ) |> pull( paste0( curdir, "dir")  )
  
  chart <- cbind( get_face( curface, curdir ), get_face( adjface, adjdir ))
  path <- chart[currow, (curcol+1):min(curcol+dist[nn]-1, ncol( chart))]
  stopmove <- match(  "#", path, nomatch = length(path)+1) -1
  
  curcol <- curcol + stopmove
  if( curcol > sidedim )
  {
    curface = adjface
    curdir  = adjdir
    curcol  = curcol - sidedim
  }

  times = c( R=1, L=3, X=0)[ turn[nn] ]
  times=3
  for( t in rep( 0, times) )
  {
    curdir  = rotR[ curdir ]
    curcol0 = currow
    currow  = sidedim +1 - curcol
    curcol = curcol0
  }
}

# currow0 <- currow
# curcol0 <- curcol
# curdir0 <- curdir


currow
curcol
curdir

currow <- currow0
curcol <- curcol0
curdir <- curdir0

curface
curdir0


(3*50 + currow)*1000 + (curcol)*4 + 3

nn
rep( 1, 0)
1:0
length( turn)
turn[nn]
code = dir_code[curdir ]
while( curdir != ">")
{
  
}

cat( curface, " ", curdir, "\n")

turns[2]
get_face( "13", ">")
mm <- matrix( 1:9, nrow =3)
mm[3:1,3:1] # rot 180
t( mm)[,3:1] # rot clockwise 90
t( mm[,3:1]) # rot counterclockwise 90

currow
curcol
curdir

rotL
dirs

rotR
##
## Part 2
##






cmin <- mmap |> group_by( row ) |> summarize( cmin = min(col)-1 ) |> pull( cmin )
rmin <- mmap |> group_by( col ) |> summarize( rmin = min(row)-1 ) |> pull( rmin )

mmap |> group_by( row ) |> summarize( cmin = min(col), cmax = max(col) ) 
mmap |> group_by( col ) |> summarize( rmin = min(row), cmin = max(row) ) 


compass = c( ">", "v", "<", "^" )
compass_val = 0:3
names( compass_val ) = compass
dir = c( "<" = -1, ">" = 1, "v"  =1, "^" = -1, "L" = -1, "R" = 1, "X" = 0 )

idx_cycle = \( idx, len ) (idx-1) %% len +1

currow = 1
curcol = 1 + cmin[currow]
curdir = ">"
dist = str_extract_all( last( input ), "[:digit:]+")[[1]] |> as.numeric()
turns = str_extract_all( last( input ), "[RL]")[[1]] |> c( "X" )

#cat( "(", currow, ", ", curcol, ") ", curdir, "\n")
for( movenum in 1:length( dist ))
{
  nmove = dist[movenum]
  
  #cat( "move = ", nmove, "turn = ", turns[movenum])
  
  if( curdir == ">" || curdir == "<")
  {
    entries = mmap |> filter( row == currow ) |> pull( tile )
    path = curcol - cmin[ currow ] + dir[ curdir ] * (1:nmove)
    path = idx_cycle( path, length( entries ) )
    
    stopmove = match(  "#", entries[path], nomatch = length(path)+1) -1
    if( stopmove > 0 )
      curcol = path[ stopmove ] + cmin[ currow ]
  } else {
    entries = mmap |> filter( col == curcol ) |> pull( tile )
    path = currow - rmin[ curcol ] + dir[ curdir ] * (1:nmove)
    path = idx_cycle( path, length( entries ) )
    
    stopmove = match(  "#", entries[path], nomatch = length(path)+1) -1
    if( stopmove > 0 )
      currow = path[ stopmove ] + rmin[ curcol ]
  }
  
  turn <- turns[movenum]
  curdir <- compass[ idx_cycle( match( curdir, compass ) + dir[ turn ], 4 ) ]
  #cat( " (", currow, ", ", curcol, ") ", curdir, "\n")
}

1000 * currow + 4 * curcol + compass_val[curdir]




