#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)
library( bitops )
options(digits=20)

day <- 22
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "b" ))

use_test = F
input = if( use_test ) test else raw

evolve <- \(n)
{
  #  n <- 9437184
  n <- bitShiftL( n, 6 ) |> bitXor( n ) |> bitAnd( 16777215 )
  n <- bitShiftR( n, 5 ) |> bitXor( n ) |> bitAnd( 16777215 )
  n <- bitShiftL( n, 11 ) |> bitXor( n ) |> bitAnd( 16777215 )
  n
}

evolve2000 <- \(n, last = T)
{
  n_hist <- c( n )
  for( k in 1:2000)
    n_hist <- c( n_hist, evolve( last( n_hist )))
  if( last ) return( last( n_hist ))
  n_hist
}

input |> as.numeric() |> evolve2000() |> sum() #part 1

key <- \(v) paste0( v, collapse = ",")

make_price_table <- \(n)
{
  prices     <- evolve2000( n, F) %% 10 
  len        <- length(prices)
  price_diff <- diff( prices )
  price_cache <- tibble( 
    l4 = price_diff[1:(len-4)],
    l3 = price_diff[2:(len-3)],
    l2 = price_diff[3:(len-2)],
    l1 = price_diff[4:(len-1)], 
    price = prices[5:len] 
  )
  newname         <- "price"
  names(newname ) <- paste0("price",n)
  price_cache <- price_cache |> group_by( l1,l2,l3,l4) |> 
    summarize( price = first(price), .groups = "drop") |> 
    rename( all_of( newname ))
  price_cache
}

tt<- input |> as.numeric() |> map( make_price_table ) 

tt <- tt |> reduce( \(t1,t2) full_join( t1,t2, by= c("l1","l2","l3","l4"))) |> 
  mutate( bananas = rowSums( across(starts_with("price")), na.rm=T))

tt |> summarize(bananas = max(bananas)) #part 2



