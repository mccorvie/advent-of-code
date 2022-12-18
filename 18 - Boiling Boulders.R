#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

day = 18
raw  <- read_advent(day = day, year=2022) |> head(-1)

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
coords <- coords +1
dd <- max( coords )

space <- array( F, dim=rep( dd+1,3))
space[coords]=T

adj <- sum( space[1:dd,,] & space[ 2:(dd+1),,]) + 
sum( space[,1:dd,] & space[ ,2:(dd+1),])+
sum( space[,,1:dd] & space[ ,,2:(dd+1)])

sum(space) * 6 - adj*2


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
  


  steam0 <- steam0 & !space  
  
  steam <- steam0
  nsteam <- c( sum( steam ), nsteam )
  cat( nsteam[1], " ")
}

steam
space <- space | !steam & !space

adj <- sum( space[1:dd,,] & space[ 2:(dd+1),,]) + 
  sum( space[,1:dd,] & space[ ,2:(dd+1),])+
  sum( space[,,1:dd] & space[ ,,2:(dd+1)])

sum( space )* 6 - adj*2


