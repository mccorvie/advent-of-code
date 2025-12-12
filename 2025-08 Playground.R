#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 8
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

n_box <- length( input)

boxes <- input |> unglue_data( "{x},{y},{z}", convert = T) |>  mutate( box = 1:n())

dist <- expand_grid( boxa=1:n_box, boxb=1:n_box) |> filter( boxa>boxb) |> 
  left_join( boxes, by = join_by( boxa== box)) |> 
  rename( xa=x,ya=y,za=z ) |> 
  left_join( boxes, by = join_by( boxb== box)) |> 
  rename( xb=x,yb=y,zb=z ) |> 
  mutate( dist = (xa-xb)^2+(ya-yb)^2+(za-zb)^2) |> 
  arrange( dist )

circuit <- 1:n_box
for( idx in 1:1000)
{
  connect = dist[idx,]
  if( circuit[connect$boxa] == circuit[connect$boxb] ) next
  circuit[ circuit==circuit[connect$boxa]] = n_box + idx
  circuit[ circuit==circuit[connect$boxb]] = n_box + idx
}

table(circuit) |> sort() |> tail( 3) |> prod()


circuit <- 1:n_box

idx <-0
while( max(circuit)-min(circuit) > 0 )
{
  idx <- idx + 1
  connect = dist[idx,]
  if( circuit[connect$boxa] == circuit[connect$boxb] ) next
  circuit[ circuit==circuit[connect$boxa]] = n_box + idx
  circuit[ circuit==circuit[connect$boxb]] = n_box + idx
}

xa = filter( boxes, box == connect$boxa) |> pull( x ) |> as.numeric()
xb = filter( boxes, box == connect$boxb) |> pull( x ) |> as.numeric()
xa * xb
