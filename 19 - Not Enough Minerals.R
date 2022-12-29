#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

day = 19
raw  <- read_advent(day = day, year=2022) |> head(-1)
test <- readLines( paste0( "test", day ))

input<- raw
input <- test

prodbenefit = list( ore0 = c(1,0,0,0),ore = c(1,0,0,0),clay = c(0,1,0,0),obs = c(0,0,1,0),geo = c(0,0,0,1))
buildcost <- list( 
  "ore0" = c(0,0,0,0),
  "ore" = c(-4,0,0,0),
  "clay" = c( -2,0,0,0),
  "obs" = c( -3,-14,0,0),
  "geo" = c( -3,0,-7,0)
)


print_build <- \( buildlist, tt )
{
  print_tt <- \(tt)
  {
    if(is.null( buildlist[[tt]]))
      return( "")
    paste0( tt, "-", paste0( buildlist[[tt]], collapse = ""), " ")
  }
  cat( tt, ") " )
  map_chr( 1:length( buildlist ), print_tt ) |> paste0( collapse = "" ) |> cat()
  cat( "\n")
}

get_budget <- function( buildlist )
{
  buildtime <- map( names( buildcost ), \(robot) map_dbl( buildlist, \(day) sum( robot == day )))
  names( buildtime ) = names( buildcost )
  
  buildtime
  prodtime <- map( buildtime, \(x) cumsum( lag( x, default=0)))
  prodtime <- map( buildtime, \(x) lag( cumsum(x), default=0))
  prodtime$ore = prodtime$ore+1
  costs    <- reduce( map2(  buildtime, buildcost, \( time, cost ) time %o% cost ), `+` )
  costs
  benefits <- reduce( map2(  prodtime,  prodbenefit,  \( time, benefit ) time %o% benefit ), `+`)
  benefits
  apply( benefits, 2, cumsum)
  rbind( rep(0,4), apply( costs+benefits, 2, cumsum )  )
}


plan_build <- \( buildlist, tt )
{
  if( runif(1)<1/1000 )
    print_build( buildlist, tt )
  
 # tt=7
  budget <- get_budget( buildlist )
  best   <- max( budget[,4])
  best
  budget
  for( nrobot in c( "geo", "obs", "clay", "ore"))
  {
#    nrobot="clay"
    
    bb <- budget[1:nrow( budget) >= tt,,drop=F] |> sweep( 2, buildcost[[nrobot]],'+') 
    tt
    bb
    dt <- match( T, apply( bb >= 0, 1, all), nomatch=NA)
    dt
    if( is.na( dt )) next
    nbuildlist <- buildlist
    cat( nrobot, tt, dt, tt + dt-1, "\n")
    nbuildlist[[ tt + dt -1]] = c( nbuildlist[[ tt + dt -1]] , nrobot )
    nbuildlist
    best <- max( best, plan_build( nbuildlist, tt+dt ))
  }
  best
}
tt+dt-1
nrobot
tt+dt-1
nbuildlist



buildlist<-rep(list(NULL), 24)
buildlist[[1]] <- c( "ore0")
for( robot in c( "ore", "clay",  "obs", "geo" ))
{
  robot="ore"
  tt=1
  while( tt <= 24 )
  {
    print_build(buildlist, tt)
    budget <- get_budget( buildlist )
    bb <- budget[1:nrow( budget) >= tt,,drop=F]  |> sweep( 2, buildcost[[nrobot]],'+') 
    dt <- match( T, apply( bb >= 0, 1, all), nomatch=NA)-1
    if( is.na( dt ))
      break
    buildlist[[tt+dt]] = c( buildlist[[tt+dt]] , "ore")
    tt <- tt+dt
  }
}


budget
buildlist

buildlist
bb <- budget[1:nrow( budget) > tt,,drop=F]  |> sweep( 2, buildcost[[nrobot]],'+') 


bb <- budget[1:nrow( budget) > tt,,drop=F]  |> sweep( 2, buildcost[[nrobot]],'+') 
dt <- match( T, apply( bb >= 0, 1, all), nomatch=NA)


budget <- get_budget( buildlist )


budget





buildlist<-rep(list(NULL), 24)
tt<-1
plan_build( buildlist, 1 )




buildlist<-rep(list(NULL), 24)

buildlist<-rep(list(NULL), 24)
buildlist[[10]] <- c( "clay", "clay")
get_budget( buildlist)

#buildlist[[1]] <- c( "ore0")
buildlist[[3]] <- c( "clay")
buildlist[[17]] <- c( "obs")
buildlist[[24]] <- c( "geo")

tt <- 24


buildlist<-rep(list(NULL), 24)

buildlist[[3]] <- c( "clay")
buildlist[[5]] <- c( "clay")
buildlist[[7]] <- c( "clay")
buildlist[[11]] <- c( "obs")
buildlist[[12]] <- c( "clay")
buildlist[[15]] <- c( "obs")
buildlist[[18]] <- c( "geo")
buildlist[[21]] <- c( "geo")
print_build( buildlist, 1 )

