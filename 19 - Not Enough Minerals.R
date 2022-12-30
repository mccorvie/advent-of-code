#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

day = 19
test <- readLines( paste0( "test", day ))
input <- readLines( paste0( "input", day ))

input<- raw
input <- test


print_build <- \( buildlist )
{
  print_tt <- \(tt)
  {
    if(is.null( buildlist[[tt]]))
      return( "")
    paste0( tt, "-", paste0( buildlist[[tt]], collapse = ""), " ")
  }
  
  map_chr( 1:length( buildlist ), print_tt ) |> paste0( collapse = "" ) |> cat()
  cat( "\n")
}

get_number_robot <- \(buildlist, robot )
{
  sum( map_dbl( buildlist, \(day) sum( robot==day)))
}

get_budget_robot <- \( buildlist, robot )
{
  ptime <- map_dbl( buildlist, \(day) sum( robot == day ))
  cumsum( lag( cumsum( ptime ), 2, default=0))
}


get_total_geo <- \(buildlist)
{
  geotime <- map_dbl( buildlist, \(day) sum( "geo" == day ))
  last( cumsum( lag( cumsum( geotime ), default=0)))
}

get_total_geo2 <- \(buildlist)
{
  geotime <- map_dbl( buildlist, \(day) sum( "geo" == day ))
  last( cumsum( lag( cumsum( geotime ), default=0)))
}

get_budget <- function( buildlist )
{
  buildtime <- map( names( buildcost ), \(robot) map_dbl( buildlist, \(day) sum( robot == day )))
  names( buildtime ) = names( buildcost )
  prodtime <- map( buildtime, \(x) lag( cumsum(x), default=0))
  prodtime$ore = prodtime$ore+1
  costs     <- reduce( map2(  buildtime, buildcost, \( time, cost ) time %o% cost ), `+` )
  spendable <- reduce( map2(  prodtime,  prodbenefit,  \( time, benefit ) lag( time, default=0 ) %o% benefit ), `+`)
  apply( costs+spendable, 2, cumsum )
}

next_build_time <- \(buildlist, tt, robot)
{
  budget <- get_budget( buildlist )
  bb <- sweep( budget, 2, buildcost[[robot]],'+')[ 1:nrow(budget) > tt,, drop=F]
  dt <- match( T, apply( bb >= 0, 1, all), nomatch=NA )
  if( is.na( dt )) return( Inf )
  return( tt + dt  )
}

for( ores in 1:40 )
{
  buildlist<-rep(list(NULL), 24)
  tt <-1
  for( ore in rep( 0, ores ))
  {
    tt <- next_build_time( buildlist, tt, "ore" )
    if( tt > 24 )
      break
    buildlist[[tt]] <- c( buildlist[[tt]], "ore" )
  }
  
  buildlist 
  for( robot in c( "clay", "obs", "geo"))
  {
    
    if( robot != "clay")
      buildlist[[1]] <- rep( "ore0", 30 )
    
    repeat
    {
      #print_build( buildlist, tt)
      tt <- next_build_time( buildlist, tt, robot )
      #cat( tt, " " )
      if( tt > 24 )
        break
      buildlist[[tt]] <- c( buildlist[[tt]], robot )
    }
    tt = 1
  }  
  #get_budget_robot( buildlist, "ore")
  get_budget_robot( buildlist, "clay")
  get_budget_robot( buildlist, "obs")
  get_budget_robot( buildlist, "geo")
  get_total_geo( buildlist)
  
  cat( ores, " ", get_total_geo( buildlist), " ", get_number_robot( buildlist, "geo") , "\n" )
}

buildlist[[1]] <- NULL
get_budget( buildlist)

last_useful_time <- c(
  "geo" = 23,
  "obs" = 21,
  "clay" = 19,
  "ore" = 17
)


plan_build <- \( buildlist, tt, possible_robots )
{
  if( runif(1)<1/10000 )
    print_build( buildlist )

  buildlist2 <- buildlist
  for( tt2 in tt:length(buildlist))
    buildlist2[[tt2]] <- "geo"
  
  if( get_total_geo( buildlist2) <= best )
    return()
  buildlist2

  
  budget <- get_budget( buildlist )
  bb <- get_total_geo( buildlist )
  if( bb > best) 
  {
    cat( "!!!!!",  bb, " ")
    print_build( buildlist, tt )
    best<<- bb    
  }
  
  below_max <- map_lgl( possible_robots, \(rr) get_number_robot( buildlist, rr ) < max_robot[rr ] )
  possible_robots <- possible_robots[ below_max ]
  
  for( nrobot in possible_robots )
  {
    bb <- sweep( budget, 2, buildcost[[nrobot]],'+')[ 1:nrow(budget) > tt,, drop=F]
    next_t <- match( T, apply( bb >= 0, 1, all), nomatch=NA ) + tt
    if( is.na( next_t)) next
    nbuildlist <- buildlist
    nbuildlist[[ next_t ]] = c( nbuildlist[[ next_t ]], nrobot )
    plan_build( nbuildlist, next_t, possible_robots)
  }
}

pattern <-"Blueprint {blueprint}: Each ore robot costs {ore} ore. Each clay robot costs {clay} ore. Each obsidian robot costs {obs1} ore and {obs2} clay. Each geode robot costs {geo1} ore and {geo2} obsidian."
data <- unglue_data( input, pattern,convert=T)


bp_best <- NULL

#for( row in 1:nrow( data ))
for( row in 1:3 )
{
  cc = data[row,]
  buildcost <- list( 
    "ore0" = c( 0,0,0,0),
    "ore" = -c( cc$ore,0,0,0),
    "clay" = -c( cc$clay,0,0,0),
    "obs" = -c( cc$obs1, cc$obs2,0,0),
    "geo" = -c(  cc$geo1,0, cc$geo2,0)
  )
  buildcost
  cat( "---- blueprint", cc$blueprint, "----\n")
  
  max_robot <- NULL
  get_max_robot <- \(nn) max( map_dbl( buildcost, \(x) abs( x[nn])))
  max_robot <- map_dbl( 1:3, get_max_robot ) 
  names( max_robot ) <- c( "ore", "clay", "obs")
  max_robot["ore"] = max_robot["ore"]-1
  max_robot["geo"] = Inf
  
  max_robot
  best<-0
  buildlist<-rep(list(NULL), 32)
  tt<-1
  plan_build( buildlist, 1, c(  "ore", "clay", "obs", "geo") )
  bp_best[ cc$blueprint ] <- best 
}
#sum( bp_best * data$blueprint)
prod(bp_best)
##
##
##



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




prodbenefit = list( ore0 = c(1,0,0,0),ore = c(1,0,0,0),clay = c(0,1,0,0),obs = c(0,0,1,0),geo = c(0,0,0,1))
buildcost <- list( 
  "ore0" = c(0,0,0,0),
  "ore" = c(-4,0,0,0),
  "clay" = c( -2,0,0,0),
  "obs" = c( -3,-14,0,0),
  "geo" = c( -3,0,-7,0)
)


buildcost <- list( 
  "ore0" = c(0,0,0,0),
  "ore" = c(-2,0,0,0),
  "clay" = c( -3,0,0,0),
  "obs" = c( -3,-8,0,0),
  "geo" = c( -3,0,-12,0)
)

