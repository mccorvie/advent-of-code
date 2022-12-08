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

dim <- 99
input <- raw

mm <- str_split( input, "", simplify =T) |> strtoi() |> matrix( nrow = dim)

tt <- matrix( FALSE, nrow=dim, ncol=dim)
tt[1,] = tt[dim,] = tt[,1] = tt[,dim] = T

for( i in 2:(dim-1))
  for( j in 2:(dim-1))
  {
    height <- mm[i,j]
    tt[i,j] <- height > max( mm[i,(j+1):dim]) || 
      height > max(mm[i,1:(j-1)]) ||
      height > max(mm[1:(i-1),j ] ) ||
      height > max(mm[(i+1):dim,j]) 
  }

sum( tt)

view <- function( h, v )
{
  min( sum( cummax(v) < h) +1, length(v))
}

ss <- matrix(0, nrow=dim, ncol=dim)
for( i in 2:(dim-1))
  for( j in 2:(dim-1))
  {
    height <- mm[i,j]
    
    ss[i,j] <-  view( height, mm[i,(j+1):dim] ) *
      view(height, mm[i,(j-1):1]) *
      view(height, mm[(i-1):1,j ] ) *
      view(height, mm[(i+1):dim,j]) 
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

ttl <- map( 1:dim, ~ tall( mm[., ])) 
ttl <- do.call( rbind, ttl) 

ttr <- map( 1:dim, ~ rev( tall( rev(mm[.,])) ))
ttr <- do.call( rbind, ttr)

ttd <- map( 1:dim, ~ tall( mm[, .])) 
ttd <- do.call( cbind, ttd) 

ttu <- map( 1:dim, ~ rev( tall( rev( mm[,.])) ))
ttu <- do.call( cbind, ttu)

tt0 <- ttl | ttr | ttu | ttd
sum(tt0)


#1806
#2039
#2030
#2099
#1825
