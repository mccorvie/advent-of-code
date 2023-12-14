# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 14
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw




col_roll <- \(col)
{
  col <- c( "#", col )
  groupnum     <- cumsum(col == "#")
  group_start  <- col=="#"
  group_offset <- seq_along(col)[ group_start ]-1
  round_cnt    <- unique( groupnum ) |> map_dbl( \(gn) col[groupnum == gn] |> keep( \(v) v=="O") |> length() ) 

  out <- rep( ".", length( col)-1 )
  out[ tail( group_offset, -1) ] <- "#"
  for( idx in seq_along( round_cnt))
  {
    if( round_cnt[idx]==0) next
    out[ 1:round_cnt[idx] + group_offset[idx] ] <- "O"
  }
 
  out 
#  map2_dbl( round_cnt, group_offset, \(cnt, off) ifelse( cnt ==0,0, length(col)*cnt - off*cnt - sum(seq(cnt))))
}

calc_load <- \( paradish ) sum(apply( paradish=="O",1,sum)*seq(ncol(paradish),1) )

print_paradish <- \(pp)
{
  str <- apply( t( pp), 2, \(c) paste0(c,collapse="")) 
  for( idx in seq_along( str )) cat( str[idx], "\n")
}


paradish0 <- str_split( input, "", simplify = T)

#paradish  <- rbind( rep( "#", ncol( paradish)), paradish)

#oart 1
paradish <- map( 1:ncol( paradish0 ), \(cc) col_roll( paradish0[,cc])) |> reduce( cbind)
calc_load( paradish)

print_paradish( paradish)

paradish <- paradish0
loads<-integer()
state_history <- list()
for( tilt in 1:10000 )
{
  paradish <- map( 1:ncol( paradish ), \(cc) col_roll( paradish[,cc])) |> reduce( cbind)
  paradish <- t( paradish[seq(ncol(paradish),1),])

  if( tilt %%4 ==0 )
  {
    key <- paste0( paradish, collapse="")
    if( !is.null( state_history[[key]] )) 
    {
      cat( "repeat!!!!\n")
      tilt_loop <- c( state_history[[key]], tilt )  
      break
    }
    state_history[[key]] <- tilt
    loads<-c( loads, calc_load(paradish))
    cat( tilt, " ", calc_load(paradish),"\n")
    
  }
  
}

tilt_num <-(1000000000*4-tilt_loop[1])%%(tilt_loop[2]-tilt_loop[1]) + tilt_loop[1]
loads[tilt_num/4]

