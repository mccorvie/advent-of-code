#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 18
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "a" ))

use_test = F
input = if( use_test ) test else raw

coords <- input |> str_split( ",", simplify = T) |> as.numeric() |> matrix(ncol=2)
coords <- coords +1
dim  <- max(coords)


flood <- \(coords)
{
  mm <- matrix( Inf, nrow = dim, ncol=dim)
  mm0 <- mm
  pad <- rep( Inf, dim)
  mm[1,1] <- 0
  mm[coords] <- NA
  
  while( any(mm0!=mm,na.rm=T ))
  {
    mm0 <- mm
    mm[coords] <- NA
    mm_up   <- rbind( mm[2:dim,], pad)
    mm_down <- rbind( pad, mm[1:(dim-1),] )
    mm_left  <- cbind( mm[,2:dim], pad)
    mm_right <- cbind( pad, mm[,1:(dim-1)] )
    
    mm <- pmin( mm, mm_up+1, mm_down+1, mm_left+1, mm_right+1,na.rm=T)
  }
  mm
}

take <- 1024

nrow(coords)
take <- 2882
(2890+2881)/2

(25+18)/2
take<- 21

mm <- flood( coords[1:take,])
mm[dim,dim]

coords[21,]-1
coords[2883,]-1
coords
length(coords)
