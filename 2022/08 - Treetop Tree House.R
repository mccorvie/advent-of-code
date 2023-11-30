#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)

raw <- read_advent(day = 8, year=2022) |> head(-1)

test<-c(
  "30373",
  "25512",
  "65332",
  "33549",
  "35390"
)

# test input
# dim <- 5
# input <- test

seq_along( letters)

dim <- 99
input <- raw

hh <- str_split( input, "", simplify =T) |> strtoi() |> matrix( nrow = dim)

tt <- matrix( FALSE, nrow=dim, ncol=dim)
tt[1,] = tt[dim,] = tt[,1] = tt[,dim] = T

for( i in 2:(dim-1))
  for( j in 2:(dim-1))
  {
    height <- hh[i,j]
    tt[i,j] <- height > max( hh[i,(j+1):dim]) || 
      height > max(hh[i,1:(j-1)]) ||
      height > max(hh[1:(i-1),j ] ) ||
      height > max(hh[(i+1):dim,j]) 
  }

sum( tt)

view <- \( h, v ) min( sum( cummax(v) < h) +1, length(v))

ss <- matrix(0, nrow=dim, ncol=dim)
for( i in 2:(dim-1))
  for( j in 2:(dim-1))
  {
    height <- hh[i,j]
    
    ss[i,j] <-  view( height, hh[i,(j+1):dim] ) *
      view(height, hh[i,(j-1):1]) *
      view(height, hh[(i-1):1,j ] ) *
      view(height, hh[(i+1):dim,j]) 
  }

max(ss)

##
##  Alternate approach for part 1 
##

tall <- function( vv)
{
  viz <- c(-1,cummax(vv))
  viz[1:dim]< viz[2:(dim+1)]
}

ttl <- map( 1:dim, ~ tall( hh[., ])) 
ttl <- do.call( rbind, ttl) 

ttr <- map( 1:dim, ~ rev( tall( rev(hh[.,])) ))
ttr <- do.call( rbind, ttr)

ttd <- map( 1:dim, ~ tall( hh[, .])) 
ttd <- do.call( cbind, ttd) 

ttu <- map( 1:dim, ~ rev( tall( rev( hh[,.])) ))
ttu <- do.call( cbind, ttu)

tt0 <- ttl | ttr | ttu | ttd
sum(tt0)


#1806
#2039
#2030
#2099
#1825
