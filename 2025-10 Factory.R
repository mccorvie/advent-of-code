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

map_dbl( input, n_press_indicator) |> sum()


# stars and bars
# n is sum of entries, k is # entries
compositions <- \( n, k )
{    
  combn( n+k-1,k-1) |> apply( 2, \(x) diff( c( 0,x,n+k))-1) |> as.matrix()
}

recurse<<-0
n_press_joltage0 <- \( buttons, joltage )
{
  recurse <<- recurse+1
  if( recurse %% 1000 == 0)
    cat( recurse, " buttons: ", ncol( buttons), " joltage: ", paste0(joltage, collapse=", "), "\n")

  bb<<-buttons
  jj <<-joltage
   buttons<-bb
   joltage<-jj
  target_idx     <- which.min( joltage )
  target_buttons <- which( buttons[target_idx,]==1 )
  if( !length( target_buttons))
    return( Inf)
buttons
  
  buttons
  joltage
  target_idx
  target_buttons

  comp <- compositions( joltage[ target_idx ], length( target_buttons ) )
  comp
ncol(comp)
  pressed <- matrix( 0, ncol=ncol(comp), nrow = ncol(buttons))
  
  pressed[target_buttons, ] <- comp
  pressed
  buttons
  jremain = joltage - buttons %*% pressed
  jremain
  min_npress <- Inf
  cc<-1
  for( cc in 1:ncol(pressed))
  {
    live_j   <- abs(jremain[,cc]) > 1e-3
    if( !any( live_j ))
    {
      cat( "--> exact\n")
      return( joltage[target_idx])
    }
    live_j
    
    dead_b <- apply( buttons[live_j,,drop=F],2,sum) < 1e-3
    if( any( dead_b))
      next
    
    live_b <- apply( buttons[!live_j,,drop=F],2,sum) < 1e-3
    if( !any( live_b))
      next
    
    
    
    npress <- n_press_joltage0( buttons[ live_j, live_b, drop=F], jremain[live_j, cc ])
    min_npress <- min( npress, min_npress )
  }
  
  return( min_npress + joltage[target_idx] )
}

n_press_joltage <- \( line )
{
  cat( "*")
  joltage <- get_joltage_v( line )
  buttons <- get_button_m( line, length(joltage))
  n_press_joltage0( buttons, joltage )
}
bb
jj
line = input[1]
line
buttons

map_dbl( input, n_press_joltage ) |> sum()


bb
jj

## ridge regression

joltage <- get_joltage_v( line )
button  <- get_button_m (line, length( joltage))

apply( button, 1, sum )

fit <- glmnet( button, joltage, alpha=1)
fit
best_lambda <- fit$lambda.min
best_lambda
coef(fit, s = "lambda.min")


ridge_fit <- lm.ridge( joltage ~ button+0, lambda = 0.5)
ridge_fit
