#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 11
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02db", day))

use_test = F
input = if( use_test ) test else raw
connect <- list()
walk( input, \(line) connect[[str_sub( line, 1,3 )]] <<- str_split_1( str_sub( line, 6), " ") )

count_paths <- \(source, dest, reset=T)
{
  if( reset ) path_num <<-list()
  if( source == dest) return(1)
  #if( source %in% exclude ) return(0)
  
  if( is.null( path_num[[source]]))
    path_num[[ source ]] <<- map_dbl( connect[[source]], \(x) count_paths( x, dest, F )) |> sum()
  
  path_num[[source]]
}

count_paths( "you", "out") # part 1

dacfft <- count_paths( "svr", "fft" ) * count_paths( "fft", "dac" ) * count_paths( "dac", "out" )
fftdac <- count_paths( "svr", "dac" ) * count_paths( "dac", "fft" ) * count_paths( "fft", "out" ) 
dacfft + fftdac # part 2
