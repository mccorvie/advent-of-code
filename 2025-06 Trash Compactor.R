#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 6
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

mm <- str_split( str_trim( input ), " +", simplify=T)
ops <- mm[nrow(mm),]
mm <- mm[1:(nrow(mm)-1),] |>  apply( 2, as.numeric)
ss <- mm[, ops=="+"] |> apply( 2, sum) |> sum()
pp <- mm[, ops=="*"] |> apply( 2, prod) |>  sum()
ss+pp


