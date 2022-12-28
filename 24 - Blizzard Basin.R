#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day = 24
# adventr requests are being blocked
#raw  <- read_advent(day = day, year=2022) |> head(-1)
raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))
test <- c(
  "#.#####",
  "#.....#",
  "#>....#",
  "#.....#",
  "#...v.#",
  "#.....#",
  "#####.#"  
)

use_test = F
input = if( use_test ) test else raw

blizzard <- str_split( input, "", simplify = T )
nr <- nrow( blizzard)
nc <- ncol( blizzard)

idx_wrap <- \(xx,width) (xx-1)%%(width) + 1

blizzN <- blizzard[2:(nr-1),2:(nc-1)] == "^"
blizzE <- blizzard[2:(nr-1),2:(nc-1)] == ">"
blizzS <- blizzard[2:(nr-1),2:(nc-1)] == "v"
blizzW <- blizzard[2:(nr-1),2:(nc-1)] == "<"

get_blizzard <- \(tt)
{
  nr2 <- nr-2
  nc2 <- nc-2
  bb <- list(
    blizzN[ idx_wrap( 1:nr2 + tt, nr2 ), ],
    blizzS[ idx_wrap( 1:nr2 - tt, nr2 ), ],
    blizzE[ ,idx_wrap( 1:nc2 - tt, nc2 ) ],
    blizzW[ ,idx_wrap( 1:nc2 + tt, nc2 ) ]
  ) |> reduce( `|`)
  
  border <- c( F, rep( T, nc2-1))
  bb <- rbind( border, bb, rev(border) )
  bb <- cbind( rep( T, nr ), bb, rep(T,nr))
  bb
}


get_adjacent <- \(adj)
{
  adj0 <- adj
  c.low  <- 1:(ncol(adj)-1)
  c.high <- c.low+1
  r.low  <- 1:(nrow(adj)-1)
  r.high <- r.low+1mna
  adj[r.low,]  <- adj[r.low,]  | adj0[r.high,] # up
  adj[r.high,] <- adj[r.high,] | adj0[r.low,]  # down
  adj[,c.low]  <- adj[,c.low]  | adj0[,c.high] # left
  adj[,c.high] <- adj[,c.high] | adj0[,c.low]  # right
  adj  
}


make_journey <- \( r.start, c.start, r.end, c.end, t.start )
{
  feasible <- matrix( F, nrow = nr, ncol = nc)
  feasible[r.start, c.start] <- T
  tt <- t.start
  repeat
  {
    tt <- tt+1
    feasible <- get_adjacent( feasible ) & !get_blizzard( tt )
    if( feasible[r.end, c.end])
      return( tt )
  }
}                   


t1 <- make_journey( 1,2,nr, nc-1, 0 )
t2 <- make_journey( nr, nc-1,1,2, t1 )
t3 <- make_journey( 1,2,nr, nc-1, t2 )
t1 # part 1
t3 # part 2


##
## Debugging
##


mm<-get_blizzard(9)
mdisp <- matrix(".", nrow=nr, ncol=nc)
mdisp[ mm ] = "#"
walk( 1:nrow( mdisp ), \(rr) cat( paste0( mdisp[rr,], collapse="" ),"\n" ))



##
## other stuff
##

print_tt <- \( tt)
{
  mtot <- matrix( 0, nrow = nrow( blizzard), ncol = ncol( blizzard))
  mdisp <- matrix( ".", nrow = nrow( blizzard), ncol = ncol( blizzard))
  for( dir in c( ">", "<", "^", "v"))
  {
    mdir <- map( 1:nrow( blizzard ), \(rr) map_lgl( 1:ncol( blizzard), \(cc) is_blizzard( rr, cc, tt, dir ))) 
    mdir <- do.call( rbind, mdir)
    mdisp[ mdir ] <- dir
    mtot <- mtot + mdir 
  }
  mtot <- if_else( mtot == 4, "#", if_else( mtot > 1, paste0( mtot), "X" )) |> matrix( nrow = nrow(mtot))
  mdisp[ mtot != "X"] = mtot[mtot!="X"]
  walk( 1:nrow( mdisp ), \(rr) cat( paste0( mdisp[rr,], collapse="" ),"\n" ))
}


blizzard_wrapx <- \(xx,width) (xx-2)%%(width-2) + 2

is_blizzard <- \( rr, cc, tt, dir )
{
  if( cc == 1 || cc == ncol( blizzard ))
    return( T )
  if( rr == 1)
    return( cc != 2 )
  if( rr == nrow( blizzard ))
    return( cc != ncol( blizzard)-1)
  if( dir == "^" )
    rr <- blizzard_wrapx( rr+tt, nr)
  if( dir == "v" )
    rr <- blizzard_wrapx( rr-tt, nr)
  if( dir == "<" )
    cc <- blizzard_wrapx( cc+tt, nc)
  if( dir == ">" )
    cc <- blizzard_wrapx( cc-tt, nc)
  
  (blizzard == dir)[rr,cc]
}

