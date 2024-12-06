#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 5
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw
test

breakline <- which( input == "")
forbidden <- input[1:(breakline-1)] |> str_split( "\\|", simplify=T) |> as_tibble()
colnames( forbidden ) <- c( "post", "pre")

jobs <- input[(breakline+1):length(input)] |> str_split( ",")

part1 <- 0
part2 <- 0
for( job in jobs ) 
{
  violations <- crossing(pre_idx = seq_along(job), post_idx = seq_along(job)) |> 
    filter( pre_idx < post_idx) |> 
    mutate( pre = job[pre_idx], post = job[post_idx]) |> 
    inner_join( forbidden, by = c( "pre", "post")) 
  
  sorted_mid <- tibble( page = job, order = seq_along( job)) |> 
    bind_rows( tibble( page = pull( violations, "pre"),  order = 1)) |> 
    bind_rows( tibble( page = pull( violations, "post"), order = -1)) |> 
    group_by( page ) |> 
    summarize( order =sum(order )) |> 
    filter( order == median(seq_along(job))) |> 
    pull( page ) |> 
    as.numeric()
  
  if( nrow(violations)==0)
    part1 <- part1 + sorted_mid
  else
    part2 <- part2 + sorted_mid
}

c( part1, part2 )


