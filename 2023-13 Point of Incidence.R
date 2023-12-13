# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 13
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

is_vrefl <- \( vrefl )
{
  width <- min( vrefl, ncol(mm)-vrefl)
  seq1 <- seq( vrefl-width+1, vrefl)
  seq2 <- seq( vrefl+width, vrefl+1)
  sum( xor( mm[,seq1], mm[,seq2])) == error_tolerance
}

is_hrefl <- \( hrefl )
{
  width <- min( hrefl, nrow(mm)-hrefl)
  seq1 <- seq( hrefl-width+1, hrefl)
  seq2 <- seq( hrefl+width, hrefl+1)
  sum( xor( mm[seq1,], mm[seq2,])) == error_tolerance
}

error_tolerance <- 0 # part 1
error_tolerance <- 1 # part 2

groups <- cumsum( input == "")+1
note_summary <- 0
for( groupnum in unique( groups ))
{
  mm <- input[ groups==groupnum] |> keep( \(l) l!="") |> str_split( "" ) |> reduce( rbind )
  mm <- mm=="#"
  notes_v <- 1:(ncol( mm )-1) |> keep( is_vrefl ) |> sum()
  notes_h <- 1:(nrow( mm )-1) |> keep( is_hrefl ) |> sum()
  note_summary <- note_summary + 100*notes_h + notes_v
}
note_summary

