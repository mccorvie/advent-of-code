#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = 14, year=2022) |> head(-1)
test <- c(
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9"
)

draw_rocks <- function( segments )
{
  for( idx in 1:(length( segments)-1))
  {
    xs <- segments[[idx]][1]:segments[[idx+1]][1]-xoffset
    ys <- segments[[idx]][2]:segments[[idx+1]][2]
    mm[ys,xs] <<- "#"
  }    
}

drop_sand <- function()
{
  coord = matrix(c(0,500-xoffset),ncol=2,byrow=T)
  move_dir <- matrix( c( 1,1,1,0,0,-1,1,0), ncol=2)

  while( T )
  {
    moves  <- coord[ rep(1,nrow(move_dir)),] + move_dir
    ncoord <- moves[match( ".", mm[moves] ),,drop=F]
    if( ncoord[1] == nrow(mm) || any( is.na(ncoord)))
      return(F)
    if( all( ncoord == coord ))
    {
      mm[ ncoord ] <<- "O"
      return(T)
    }
    coord <- ncoord
  }
}

input <- raw
pp <- str_split( input,  " -> ") |> map( ~ map( str_split( ., ","  ),  strtoi ))
pp.flat <- unlist( pp )
xs <- seq(1,length(pp.flat), by=2)
xoffset <- pp.flat[xs] |> min()-2
xmax <- pp.flat[xs]  |> max()+1
ymax <- pp.flat[xs+1] |> max()+1

mm <- matrix( ".", nrow = ymax, ncol = xmax-xoffset)
walk( pp, draw_rocks )

cnt <- 0
while( drop_sand() )
  cnt <- cnt+1

cnt # part 1

ymax2 <- ymax + 2 
xoffset <- 500 - ymax2 -1
xmax2 <- 500 + ymax2

mm <- matrix( ".", nrow = ymax2, ncol = xmax2-xoffset)
walk( pp, draw_rocks )
draw_rocks( list( c(xoffset+1, ymax2-1,1), c(xmax2, ymax2-1 )))

cnt <- 0
while( drop_sand() )
  cnt <- cnt+1
cnt+1 # part 2

