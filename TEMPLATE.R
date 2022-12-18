#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

day = XX
raw  <- read_advent(day = day, year=2022) |> head(-1)
test <- readLines( paste0( "test", day ))

input<- raw
input <- test

