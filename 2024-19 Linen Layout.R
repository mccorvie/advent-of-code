#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 19
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "a" ))

use_test = F
input = if( use_test ) test else raw

patterns <- input[1] |> str_split(", ", simplify = T) |> as.character()
designs  <- input[3:length(input)]

n_possible <- \(design, depth=1)
{
  #cat( depth, " ", design, "\n")
  if( !is.null( pcache[[design]] )) return( pcache[[design]])
  if( str_length(design) == 0) return( 0 )
  
  starts <- patterns[ str_starts( design, patterns)]
  
  pcache[[design]] <<- map_dbl( starts, \(s) n_possible( str_sub(design, str_length(s)+1) )) |> sum()
  pcache[[design]]
}

patterns <- tibble( pattern=patterns ) |> mutate( len = str_length(pattern)) |> arrange( len) |> pull( pattern)
pcache   <- list()

for( pattern in patterns )
  pcache[[pattern]] <- n_possible( pattern) + 1

ways <- map_dbl( designs, n_possible ) 

sum( ways != 0 ) # part 1 
sum( ways ) # part 2
length(pcache)
