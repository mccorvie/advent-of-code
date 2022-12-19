#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

day = 18
#raw  <- read_advent(day = day, year=2022) |> head(-1)
raw <- readLines( "input18")
test <- c( 
  "1,1,1",
  "2,1,1"
)

test <- c(
"2,2,2",
"1,2,2",
"3,2,2",
"2,1,2",
"2,3,2",
"2,2,1",
"2,2,3",
"2,2,4",
"2,2,6",
"1,2,5",
"3,2,5",
"2,1,5",
"2,3,5"
)

input<- raw
input <- test

coords<-tibble( input=input) |> 
  separate( input, c("x","y","z"), sep = ",", convert = T) |> 
  as.matrix()
dd <- max( coords ) + 1
droplets <- array( F, dim=rep( dd+2,3))
droplets[ coords+2 ] <- T

# all the coordinates, excluding either the leftmost slice or the rightmost slice
shifty <- \(  dim ,  shift =  0 )
{
  cc <- rep( list( 1:(dd+2) ), 3 )
  cc[[dim]] <- 1:(dd+1) + shift
  do.call( expand.grid, cc ) |> as.matrix()
}

surface_area <- \(mm)
{
  adj <- \(dim) map(0:1, ~ mm[ shifty(dim, .)] ) |>  reduce( `&` ) |>  sum()
  borders <- map_dbl( 1:3, adj ) |> sum()
  sum(mm) * 6 - borders*2 
}

surface_area( droplets ) # part 1

# fill the boundaries of the droplets with steam
steam <- array( F, dim=rep( dd+2,3))
steam[1,,]    = steam[,1,]    = steam[,,1] = T
steam[dd+2,,] = steam[,dd+2,] = steam[,,dd+2] = T

# Let the steam percolate
nsteam = c( sum(steam),0 )
while( nsteam[1] > nsteam[2])
{
  steam0 <- steam
  for( dim in 1:3)
  {
    superpos <- map( 0:1, ~ steam0[shifty(dim,. )] ) |> reduce( `|` )
    steam[shifty(dim,0)] <- superpos | steam[shifty(dim,0)]
    steam[shifty(dim,1)] <- superpos | steam[shifty(dim,1)]
  }
  steam <- steam & !droplets  
  nsteam <- c( sum( steam ), nsteam )
  cat( nsteam[1], " ")
}

surface_area( droplets | !steam & !droplets ) # part 2

##
##. submitted solution
##

rep( list(1:2), 3)


adj <- sum( droplets[1:dd,,] & droplets[ 2:(dd+1),,]) + 
sum( droplets[,1:dd,] & droplets[ ,2:(dd+1),])+
sum( droplets[,,1:dd] & droplets[ ,,2:(dd+1)])

sum(droplets) * 6 - adj*2


steam <- array( F, dim=rep( dd+1,3))
steam[1,,] = steam[,1,] = steam[,,1] = T
steam[dd+1,,] = steam[,dd+1,] = steam[,,dd+1] = T

nsteam = c( sum(steam),0 )
while( nsteam[1] > nsteam[2])
{
  steam0 <- steam
  shifty <- steam[1:dd,,] | steam[ 2:(dd+1),,]
  steam0[1:dd,,]     <- steam0[1:dd,,] | shifty
  steam0[2:(dd+1),,] <- steam0[2:(dd+1),,] | shifty
  
  steam0[1:dd,,]     <- steam0[1:dd,,] | shifty
  steam0[2:(dd+1),,] <- steam0[2:(dd+1),,] | shifty

  shifty <- steam[,1:dd,] | steam[,2:(dd+1),]
  steam0[,1:dd,]     <- steam0[,1:dd,] | shifty
  steam0[,2:(dd+1),] <- steam0[,2:(dd+1),] | shifty

  shifty <- steam[,,1:dd] | steam[ ,,2:(dd+1)]
  steam0[,,1:dd]    <- steam0[,,1:dd] | shifty
  steam0[,,2:(dd+1)] <- steam0[,,2:(dd+1)] | shifty
  


  steam0 <- steam0 & !droplets  
  
  steam <- steam0
  nsteam <- c( sum( steam ), nsteam )
  cat( nsteam[1], " ")
}

steam
droplets <- droplets | !steam & !droplets

adj <- sum( droplets[1:dd,,] & droplets[ 2:(dd+1),,]) + 
  sum( droplets[,1:dd,] & droplets[ ,2:(dd+1),])+
  sum( droplets[,,1:dd] & droplets[ ,,2:(dd+1)])

sum( droplets )* 6 - adj*2


