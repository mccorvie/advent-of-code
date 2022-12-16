#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( unglue)
library( tidyverse )
library( adventr)

raw <- read_advent(day = 15, year=2022) |> head(-1)
test <- readLines( "test15" )

input<- raw
y1 = 2000000
minc = 0
maxc = 4000000

input <- test
y1 = 10
yy= 0
minc = 0
maxc = 20

pattern <- "Sensor at x={sx}, y={sy}: closest beacon is at x={bx}, y={by}"

sensor_data <- unglue_data( input, pattern, convert=T) 
beacons <- sensor_data |> distinct( bx, by )

get_y_slice <-function( yy ) 
{
  sensor_data |> 
    mutate(
      dist = abs( bx - sx ) + abs( by - sy ),
      ygap  =  dist - abs( sy-yy),
      x1 = sx - ygap, 
      x2 = sx + ygap, 
    )|> 
    filter( ygap >= 0) 
}

overlaps <- function( y_slice )
{
  y_slice |> 
    arrange( x1 ) |> 
    mutate( 
      sup_x2 = cummax( x2 ),
      overlap = coalesce( lag(sup_x2) - x1 +1, Inf )
    )  
}  

y_slice <- get_y_slice( y1 )
min_x1 <- y_slice |> pull( x1 ) |> min()
max_x2 <- y_slice |> pull( x2 ) |> max()
# as it turns out there are no gaps
gaps <- overlaps(y_slice) |> filter( overlap < 0 ) |> pull( overlap ) |> sum()
nb <- beacons |> filter( by==y1, bx >= min_x1, bx <=max_x2 ) |> nrow()
max_x2 - min_x1 +1 + gaps - nb # part 1

yy = minc
while( yy <= maxc )
{
  y_slice <- get_y_slice( yy )
  gaps <- y_slice |> overlaps() |> filter( overlap < 0 ) 
  if( nrow( gaps ) > 0)
  {
    xx <- gaps |> pull( x1 ) |> first() -1
    break
  }
  # the next time waning cones don't overlap
  waning_cones <- y_slice |> filter( sy < yy ) |> overlaps() |> filter( overlap >= 0 )
  receding_overlap <- min( waning_cones$overlap, Inf)
  dy <- receding_overlap %/% 2 + 1
  # the next time cones switch from waxing to waning
  next_center <- y_slice |> filter( sy > yy ) |> pull( sy ) |> min()
  # the next time an interval disappears
  next_top <- y_slice |> mutate( top = sy + dist ) |> filter( top > yy ) |> pull(top ) |> min()
  cat( yy, " ", dy, "\n")
  yy <- min( yy + dy, next_center + 1, next_top + 1)
}

print( 4000000 * xx + yy , digits=20) # part 2


y_slice |> 
  arrange( aa ) |> 
  mutate( 
    sup_bb = cummax( bb ),
    overlap = lag(sup_bb) - aa +1
  )
yy
xx <- 2895970

intervals$sy+1 == yy


sprintf( "%f", 4000000 * xx + yy )

##
## part 1 actually used
##

intervals <- unglue_data( input, pattern, convert=T) |> 
  mutate(
    dist = abs( bx - sx ) + abs( by - sy ),
    gap  = dist - abs( sy-v),
    la = sx - gap,
    lb = sx + gap,
    intersect_old = F,
    intersect = F
  )|> 
  filter( gap >= 0)


beacons <- intervals |> distinct( bx, by )
total_length <- 0
while( nrow( intervals) >0 )
{
  cur_int <- intervals |> head(1)
  cur_int
  repeat
  {
    intervals <- intervals |> 
      mutate( 
        intersect_old = intersect,
        inta = pmax( la, cur_int$la ), 
        intb = pmin( lb, cur_int$lb ), 
        intersect = inta <=intb
      ) 
    
    if( all( intervals$intersect_old == intervals$intersect ))
      break
    cur_int <- intervals |> filter( intersect ) |> summarize( la=min(la), lb=max(lb) )
  }
  cat( cur_int$la, " ", cur_int$lb, "\n" )
  nb <- beacons |> filter( by==v, bx >= cur_int$la, bx <=cur_int$lb) |> nrow()
  total_length <- total_length + cur_int$lb - cur_int$la +1 - nb
  intervals <- intervals |> filter( !intersect )
}
intervals

