#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

library(glmnet)
library(MASS)

options(digits=20)

day <- 10
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = F
input = if( use_test ) test else raw

get_indicator_v <- \(line)
{
  indicator <- str_extract( line, "\\[(.*)\\] (.*) \\{(.*)\\}", group=1) |> str_split_1("") 
  indicator <- (indicator == "#" ) |> as.numeric()
}

get_button_m <- \(line, len)
{
  buttons0 <- str_extract( line, "\\[(.*)\\] (.*) \\{(.*)\\}", group=2) |> str_split_1( " ") 
  nb = length( buttons0 )
  
  button_coord <- buttons0 |> 
    map( \(t) eval(parse(text = paste0( "c", t, "+1" )))) |> 
    map2( 1:nb, \(r,c) matrix( c( r,rep( c, length(r))), ncol=2)) |> 
    reduce( rbind )
 
  buttons <- matrix( 0, nrow = len, ncol = nb )
  buttons[ button_coord ] = 1
  buttons
}

get_joltage_v <- \(line)
{
  str_extract( line, "\\[(.*)\\] (.*) \\{(.*)\\}", group=3) |>  str_split_1(",") |> as.numeric() 
}

n_press_indicator <- \(line)
{
  
  indicator <- get_indicator_v( line ) 
  buttons   <- get_button_m( line, length( indicator ) )

  nb = ncol( buttons )
  
  zeros <- rep( 0, nb )
  for( npress in 1:nb )
  {
    pressed  <- apply( combn( nb, npress ), 2, \(x) replace( zeros,x,1) )
    
    # do the linear algebra and check for solutions
    result <- apply( buttons %*% pressed %% 2 - indicator, 2, \(x) sum( abs(x)))
    if( any( result < 1e-3 )) 
      return( npress )
  }
  NA 
}

map_dbl( input, n_press_indicator) |> sum()  # part 1

eps <-1e-5

n_press_joltage_qr <- \(line)
{
  joltage <- get_joltage_v( line )
  buttons <- get_button_m( line, length(joltage))

  res <- qr(buttons)
  Q <- qr.Q(res)
  R <- qr.R(res)
  
  rank <- res$rank
  R1  <- R[1:rank,1:rank]
  x_p <- backsolve( R1,t(Q)%*%joltage)

  if( rank == ncol(R))
    return( sum( x_p ))

  R2 <- R[1:rank,(rank+1):ncol(R)]
  NN <- -solve( R1) %*% R2
  
  explored <<- NULL
  explore_nullspace( NN, x_p)
  explored |> unlist() |> min()
}

explore_nullspace <- \( NN, x_p, cc=rep(0,ncol(NN)) )
{
  key <- paste0( cc, collapse="," )
  if( !is.null( explored[[key]]))
    return()

  xx <- x_p + NN %*% cc
  integer <- all( abs( round( xx )-xx)<eps )
  nonneg  <- all( xx > -eps )
  explored[[ key ]] <<- ifelse( integer & nonneg, sum( xx ) + sum( cc ), Inf )
  obstructed <- apply( (NN < -eps) & as.vector( xx < -eps ), 2, any)
  EE <- diag( length(cc))
  for( ii in 1:ncol( NN))
  {
    if( obstructed[ii]) next
    explore_nullspace( NN, x_p, cc + EE[ii,])
  }
}

map_dbl( input, n_press_joltage_qr) |> sum() # part 2


##
##  too slow solver
##


# stars and bars
# n is sum of entries, k is # entries
compositions <- \( n, k )
{    
  combn( n+k-1,k-1) |> apply( 2, \(x) diff( c( 0,x,n+k))-1) |> as.matrix()
}

recurse<<-0

n_press_joltage2 <- \( buttons, joltage )
{
  recurse <<- recurse+1
  if( recurse %% 10000 == 0)
    cat( "*")
  #cat( recurse, " buttons: ", ncol( buttons), " joltage: ", paste0(joltage, collapse=", "), "\n")
  
  # if( recurse > 1e6)
  #   return( Inf )
  
  bb<<-buttons
  jj <<-joltage
  buttons<-bb
  joltage<-jj
  
  
  if( ncol( buttons ) < length( joltage ))
  {
    fit = lm( joltage ~ buttons+0 )
    
  }
  
  buttons_for_jolt <- apply( buttons,1,sum)
  if( any( buttons_for_jolt==0))
    return( Inf )
  
  if( ncol( buttons)==1)
    return( ifelse( max(joltage)==min(joltage),max(joltage), Inf))
  
  n_butt <- apply(buttons,1,sum)
  target_idx <- which.min( lchoose( joltage+n_butt-1,n_butt-1))
  
  target_buttons <- which( buttons[target_idx,]==1 )
  
  comp <- compositions( joltage[ target_idx ], length( target_buttons ) )
  pressed <- matrix( 0, ncol=ncol(comp), nrow = ncol(buttons))
  
  pressed[target_buttons, ] <- comp
  jremain = joltage - buttons %*% pressed
  jremain_slack <- apply( jremain, 2, min) >= -1e-3 
  jremain
  jremain_slack
  if( !any( jremain_slack ))
    return( Inf )
  
  jremain <- jremain[,jremain_slack,drop=F]
  # 
  #   pb <- progress_bar$new(
  #     total = ncol(jremain),
  #     format = "[:bar] :percent eta: :eta"
  #   )
  
  min_npress <- Inf
  for( cc in 1:ncol(jremain))
  {
    #    pb$tick()
    live_j   <- jremain[,cc] > 1e-3
    live_j
    if( !any( live_j ))
      return( joltage[target_idx])
    
    live_b <- apply( buttons[!live_j,,drop=F],2,sum) < 1e-3
    if( !any( live_b))
      next
    
    npress <- n_press_joltage1( buttons[ live_j, live_b, drop=F], jremain[live_j, cc ])
    min_npress <- min( npress, min_npress )
  }
  
  return( min_npress + joltage[target_idx] )
}


