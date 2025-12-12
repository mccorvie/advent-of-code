#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)
library(glmnet)
options(digits=20)

day <- 10
raw <- read_advent(day = day, year=2025) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( sprintf("test%02da", day))

use_test = T
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
    map2( 1:nb, \(r,c) matrix( c( r,rep( c, length(r))), ncol=2)) 
  button_coord <- do.call( rbind, button_coord )
 
  buttons <- matrix( 0, nrow = len, ncol = nb )
  buttons[ button_coord ] = 1
  buttons
}

get_joltage_v <- \(line)
{
  str_extract( line, "\\[(.*)\\] (.*) \\{(.*)\\}", group=3) |>  str_split_1(",") |> as.numeric() 
}


fewest_presses <- \(line)
{
  indicator <- get_indicator_v( line ) 
  buttons   <- get_button_m( line, length( indicator ) )
  nb = ncol( buttons )
  
  for( npress in 1:nb )
  {
    pressed_idx_list  <- combn( nb, npress ) 
    pressed <- matrix( 0, ncol=ncol( pressed_idx_list), nrow = nb )
    for( c in 1:ncol( pressed_idx_list ))
      pressed[pressed_idx_list[,c], c ] = 1
    
    # do the linear algebra and check for solutions
    result <- apply( buttons %*% pressed %% 2 - indicator, 2, \(x) sum( abs(x)))
    if( any( result < 1e-3 )) 
      return( npress )
  }
  NA 
}



map_dbl( input, fewest_presses) |> sum()


line = input[1]

joltage <- get_joltage_v( line )
button  <- get_button_m (line, length( joltage))


fit <- glmnet( button, joltage, alpha=1)
fit
best_lambda <- fit$lambda.min
best_lambda
coef(fit, s = "lambda.min")

out$beta




library(MASS)

ridge_fit <- lm.ridge( joltage ~ button+0, lambda = 0.5)

ridge_fit


joltage
button

c( 1, 3,0,3,1,2)

map( input, get_joltage_v )
