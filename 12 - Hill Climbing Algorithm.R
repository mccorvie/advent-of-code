#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

print_dist <- function( hh )
{
  for( rr in 1:nrow(hh0))
  {
    for( cc in 1:ncol( hh0))
    {
      cat( hh |> filter( row == rr, col == cc) |> pull(dist), " ")
    }
    cat("\n")
  }
  cat("\n")
}

test<-c(
  "Sabqponm",
  "abcryxxl",
  "accszExk",
  "acctuvwj",
  "abdefghi"
)

raw <- read_advent(day = 12, year=2022) |> head(-1)
input <- raw

hh0 <- str_split(  input, "", simplify = T) 
hh  <- expand_grid( row =1:nrow(hh0), col=1:ncol(hh0)) |> 
  rowwise() |> 
  mutate( 
    heightL = hh0[ row,col],
    height  = match( str_replace_all( heightL, c( "S"="a", "E"="z")), letters ), 
    dist    = if_else( heightL == "E", 0, Inf )
  ) |> 
  ungroup()

neighbors <- expand_grid( hh, tibble( drow = c(0,0,-1,1), dcol = c( -1,1,0,0)) ) |> 
  mutate( nb_row = row + drow, nb_col = col + dcol ) |> 
  left_join( select( hh, nb_row = row, nb_col = col, nb_height = height ), by = c( "nb_row", "nb_col" )) |> 
  filter( !is.na( nb_height) & nb_height - height <= 1)

n_dist <- hh |> filter( !is.infinite( dist )) |> nrow() |> c(0)
while( n_dist[1] > n_dist[2]  )
{
  hh <- neighbors |> 
    left_join( select( hh, nb_row = row, nb_col = col, nb_dist = dist ), by = c( "nb_row", "nb_col")) |> 
    group_by( row, col ) |> 
    summarize( 
      heightL  = last( heightL ),
      height   = last( height ),
      dist     = min( nb_dist) +1,
      .groups = "drop"
    ) |> 
    mutate( dist = if_else( heightL == "E", 0, dist))
  
  n_dist <- hh |> filter( !is.infinite( dist )) |> nrow()  |> c( n_dist )
  cat( length( n_dist ), ": ", n_dist[1], " change: ", n_dist[1] - n_dist[2], "\n") 
}

# part 1
hh |> filter( heightL == "S" ) |> pull( dist ) 

# part 2
hh |> filter( heightL == "S" | heightL == "a") |> summarize( min(dist ))


##
## abandoned approach using matrices
##


mm0 <- str_split(input, "", simplify = T) |> 
  map_dbl( ~ which( c( "S", letters,"E") ==. )) |> 
  matrix( nrow = length( input))
dim <- dim( mm0)
mm <- matrix( Inf, nrow = dim[1]+2, ncol = dim[2]+2)
mm[ 2:(dim[1]+1), 2:(dim[2]+1)] <- mm0

ss <- matrix( Inf, nrow = dim(mm)[1], ncol = dim(mm)[2])
while( is.infinite( ss[22,2]))
{
  
}


#dir <- "~/Desktop/aoc-input"
#ff  <- "input01"

# input_raw <- read_delim( file.path( dir, ff) ) 
# input_raw <- readLines( file.path( dir, ff))
# input_raw <- scan(  file.path( dir, ff))
