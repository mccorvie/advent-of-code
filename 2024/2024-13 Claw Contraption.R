#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 13
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = T
input = if( use_test ) test else raw

mm_all <- str_extract_all( input, "\\d+", simplify=T) |> as.numeric() |> matrix(nrow=length(input))
cost   <- 0
part2  <- T
for( idx in seq(1,nrow(mm_all), by=4) )
{
  mm <- t(mm_all[ idx:(idx+1),])
  vv <- mm_all[ idx+2, ] + 10000000000000 * part2
  ss <- solve( mm, vv)
  if( sum(abs(ss-round(ss)))<1e-2)
    cost <- cost + ss[1]*3 + ss[2]
}
cost
