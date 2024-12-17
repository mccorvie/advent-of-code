#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 14
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

data <- input |> str_extract_all("-?\\d+", simplify = T) 
colnames(data) <- c( "x0", "y0", "vx", "vy")
data <- data |> as_tibble() |> mutate( across(colnames(data), as.numeric))

#test
roomw <- 11
roomh <- 7

#real
roomw <- 101
roomh <- 103

cutx <- (roomw-1)/2
cuty <- (roomh-1)/2

calc_pos <- \(t) 
{
  data |> mutate( x = (x0+t*vx) %% roomw, y=(y0+t*vy)%%roomh) |> 
    mutate( quadx = sign(x-cutx), quady = sign(y-cuty))
}

final <- calc_pos( 100 )
final |> filter( quadx!=0, quady!=0 ) |> group_by( quadx, quady ) |> summarize(n=n()) |> pull(n) |> prod()


singles <- \(t)
{
  calc_pos(t) |> group_by( x,y) |> summarize( n=n(), .groups = "drop") |>ungroup() |> 
    mutate( sing = n==1)  |>  summarize( all=all(sing)) |> pull( all )
}




for( t in 1:(roomh*roomw))
{
  if( (t %% 100)==0) cat( t,"\n")
  if( singles(t)) cat( "-->", t, "\n")
  
}

pos <- calc_pos(7572)

ggplot( pos, aes(x,y)) + geom_point( )



##
## based on an incorrect theory of symmetry of picture
##

skew2x <- \(t)
{
  calc_pos(t) |> group_by(quady) |> summarize( ss = sum(quadx)) |> ungroup() |> 
    summarize(sk2 =  sum(abs(ss))) |> pull( sk2)
}

skew2y <- \(t)
{
  calc_pos(t) |> group_by(quadx) |> summarize( ss = sum(quady)) |> ungroup() |> 
    summarize(sk2 =  sum(abs(ss))) |> pull( sk2)
}

xskew <- map_dbl( 1:100, \(t) calc_pos(t) |> pull(quadx) |> sum() )
which(xskew == 0)

final <- calc_pos( 57 )
final |> filter( quadx!=0, quady!=0 ) |> group_by( quadx, quady ) |> summarize(n=n())

# 1 57 68
which( map_dbl( 1:102, \(t) skew2x( t*roomw + 57)) == 0)
t = 49*roomw +1
t = 21*roomw+57
t = 45*roomw+68


yskew <- map_dbl( 1:102, \(t) calc_pos(t) |> pull(quady) |> sum() )
which(yskew == 0)

which( map_dbl( 1:100, \(t) skew2y( t*roomh + 78)) == 0)


t = 49*roomw +1
t = 21*roomw+57
t = 45*roomw+68
t = 80*roomh+78

calc_pos(t) |> group_by(quadx) |> summarize( ss = sum(quady)) |> ungroup() 

pos <- calc_pos(t)


doubled(100)
ggplot( pos, aes(x,y)) + geom_point( )
