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

robo0 = map( 1:4, ~ diag(1,4)[.,] )
names( robo0) = c( "ore", "clay", "obs", "geo" )
buildcost <- list( 
  "+ore" = c(-4,0,0,0),
  "+clay" = c( 0,-2,0,0),
  "+obs" = c( -3,0,-14,0),
  "+geo" = c( -3,0,-14,0)
)

robo = c( robo0, buildlist )
robo


buildlist<-rep(list(NULL), 26)
buildlist[3] <- 

pairlist( 1:10)
