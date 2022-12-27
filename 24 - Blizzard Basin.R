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
blizzard0 <- str_split( input, "", simplify = T )


blizzard_wrap <- \(xx,width) (xx-2)%%(width-2) + 2

is_blizzard <- \( rr, cc, tt, dir )
{
  if( cc == 1 || cc == ncol( blizzard0 ))
    return( T )
  if( rr == 1)
    return( cc != 2 )
  if( rr == nrow( blizzard0 ))
    return( cc != ncol( blizzard0)-1)
  if( dir == "^" )
    rr <- blizzard_wrap( rr+tt, nrow(blizzard0))
  if( dir == "v" )
    rr <- blizzard_wrap( rr-tt, nrow(blizzard0))
  if( dir == "<" )
    cc <- blizzard_wrap( cc+tt, ncol(blizzard0))
  if( dir == ">" )
    cc <- blizzard_wrap( cc-tt, ncol(blizzard0))
  
  (blizzard0 == dir)[rr,cc]
}

print_tt <- \( tt)
{
  mtot <- matrix( 0, nrow = nrow( blizzard0), ncol = ncol( blizzard0))
  mdisp <- matrix( ".", nrow = nrow( blizzard0), ncol = ncol( blizzard0))
  for( dir in c( ">", "<", "^", "v"))
  {
    mdir <- map( 1:nrow( blizzard0 ), \(rr) map_lgl( 1:ncol( blizzard0), \(cc) is_blizzard0( rr, cc, tt, dir ))) 
    mdir <- do.call( rbind, mdir)
    mdisp[ mdir ] <- dir
    mtot <- mtot + mdir 
  }
  mtot <- if_else( mtot == 4, "#", if_else( mtot > 1, paste0( mtot), "X" )) |> matrix( nrow = nrow(mtot))
  mdisp[ mtot != "X"] = mtot[mtot!="X"]
  walk( 1:nrow( mdisp ), \(rr) cat( paste0( mdisp[rr,], collapse="" ),"\n" ))
}


get_adj <- \(adj)
{
  adj0 <- adj
  c.low  <- 1:(ncol(adj)-1)
  c.high <- c.low+1
  r.low  <- 1:(nrow(adj)-1)
  r.high <- r.low+1
  superpos <-  adj0[r.low,] | adj0[r.high,] 
  adj[r.low,]  <- adj[r.low,] | superpos 
  adj[r.high,] <- adj[r.high,] | superpos 
  superpos <-  adj0[,c.low] | adj0[,c.high]
  adj[,c.low]  <- adj[,c.low] | superpos 
  adj[,c.high] <- adj[,c.high] | superpos 
  adj  
}


make_journey <- \( r.start, c.start, r.end, c.end, t.start )
{
  feasible <- matrix( F, nrow = nrow(blizzard0), ncol = ncol(blizzard0))
  feasible[r.start, c.start] <- T
  tt <- t.start
  repeat
  {
    tt <- tt+1
    feasible <- get_adj( feasible )
    blizzard <- map( c( ">", "<", "^", "v"), \(dir) map( 1:nrow( blizzard0 ), \(rr) map_lgl( 1:ncol( blizzard0), \(cc) is_blizzard( rr, cc, tt, dir )))) |> 
      map( \(ll) do.call( rbind, ll)) |> 
      reduce( `|`)
    feasible <- feasible & !blizzard
    if( feasible[r.end, c.end])
      break
    
    cat( "to ", tt, " ", sum( feasible), "\n")
  }
  tt
}                   


t1 <- make_journey( 1,2,nrow(blizzard0), ncol( blizzard0)-1, 0 )
t2 <- make_journey( nrow(blizzard0), ncol( blizzard0)-1,1,2, t1 )
t3 <- make_journey( 1,2,nrow(blizzard0), ncol( blizzard0)-1, t2 )
t1 # part 1
t3 # part 2


