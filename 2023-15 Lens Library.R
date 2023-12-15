# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 15
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

hash_me <- \( str )
{
  xx <- str |> str_split_1( "") |> map_dbl( utf8ToInt ) 
  c( 0,xx ) |> reduce( \(cur,new) ((cur+new)*17)%%256)
}
  
instr <- input |> str_split_1( ",") 

# spart 1
instr |> map_dbl( hash_me) |> sum()

modify <- instr |> str_match("^([a-z]+)=([1-9])$" )
remove <- instr |> str_match("^([a-z]+)\\-$" )
foc_len <- list()
nbox <- 256
order   <- list()
for( i in 1:nbox) order[[i]] <- list()

vv<-integer()
instr

for( idx in seq_along( instr ))
{
  if( !is.na( modify[idx,1]))
  {
    label  <- modify[idx,2]
    fl     <- modify[idx,3] |> as.numeric()
    foc_len[[label]] <- fl
    
    boxnum <- hash_me(label)+1
    if( is.null( order[[boxnum]][[label]]))
      order[[boxnum]][[label]]   <- max( unlist( order[[boxnum]]),0)+1
  }
  if( !is.na( remove[idx,1]))
  {
    label  <- remove[idx,2]
    foc_len[[label]] <- NULL
    
    boxnum  <- hash_me(label)+1
    cur_pos <- order[[boxnum]][[label]]
    order[[boxnum]][[label]]   <- NULL
    if( !is.null( cur_pos ))
      order[[boxnum]] <- order[[boxnum]] |> map( \(x) ifelse( x>cur_pos,x-1,x))
  }
  
  #cat( idx, "-->", names( unlist( order[[4]] )), " ",unlist( order[[4]] ), "\n")
}
power <-0
for( nn in 1:nbox )
  for( label in names(order[[nn]]))
  {
    order[[nn]][[label]]
    cat( nn, " ", label, " ", nn*order[[nn]][[label]]*foc_len[[label]],"\n")
    power <- power + nn*order[[nn]][[label]]*foc_len[[label]]
  }
power

