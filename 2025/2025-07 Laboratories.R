#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 7
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

manifold <- str_split( input, "", simplify = T)
beams = which( manifold[1,]=="S")
manifold <- manifold[ seq( 3, nrow( manifold), by=2), ]

splits <- 0
for( step in 1:nrow( manifold ))
{
  split_loc <- beams[ which( manifold[step, beams ]=="^" )]
  cat( "step ", step, " -> split ", length( split_loc) )
  splits <- splits + length( split_loc )
  beams <- unique( c( split_loc+1, split_loc-1, beams[which( manifold[step, beams ]==".")] ))
  cat( " -> beams ", length( beams ), "\n")
}
splits


manifold <- str_split( input, "", simplify = T)
path_cnt <- as.numeric( manifold[1,]=="S")
manifold <- manifold[ seq( 3, nrow( manifold), by=2), ]

for( step in 1:nrow(manifold))
{
  split_loc <- manifold[step, ]=="^"
  split_l   <- c( tail( path_cnt * split_loc,-1), 0)
  split_r   <- c( 0, head( path_cnt * split_loc,-1))
  unsplit   <- path_cnt * !split_loc
  path_cnt  <- unsplit + split_l + split_r
}
sum(path_cnt)




