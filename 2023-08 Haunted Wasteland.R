

# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")
library( pracma)
library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 8
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))
test <- readLines("test8c")

use_test = F
input = if( use_test ) test else raw

network<-tail( input,-2) |> unglue_data( "{source} = ({L}, {R})")
rownames( network) <- network[,"source"]


instructions <- str_split_1( input[1],"")
idx <- rep(1:length( instructions),20000)


cur <- "AAA"
cur <- network$source |> keep( \(x) str_detect( x, ".*A$" ))


zlist <- list()
for( ii in 1:length(cur))
  zlist[[ii]] <- integer()

for( step in seq_along(idx))
{
  instr <- instructions[ idx[step]]
  cur   <- network[ cur, instr ]
  #if( cur == "ZZZ") break
  
  zs <- str_detect( cur, ".*Z$")
  if( any( zs )) 
  {
    cat( step, " ", which( zs ), "\n")
    zlist[ zs ] <- map( zlist[zs], \(vv) c( vv, step ))
    lens <- map_dbl( zlist, length )
    
    if( all( lens >=2 ))break
  }
}
step

zlist

first <- map_dbl( zlist, \(vv) vv[1])
diff  <- map_dbl( zlist, \(vv) vv[2]-vv[1])
diff


reduce( diff, Lcm)




