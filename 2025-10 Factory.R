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

presses_indicator <- \(line)
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

map_dbl( input, presses_indicator) |> sum()


# stars and bars
# n is sum of entries, k is # entries
compositions <- \( n, k )
{    
  combn( n+k-1,k-1) |> apply( 2, \(x) diff( c( 0,x,n+k))-1) |> as.matrix()
}


presses_joltage0 <- \( buttons, joltage )
{
  bb<<-buttons
  jj <<-joltage
   # buttons<<-bb
   # joltage<<-jj
  
  target_idx <- which.min(joltage)
  buttons
  joltage
  target_idx

  target_buttons <- which( buttons[target_idx,]==1 )
  target_buttons
  cat( nrow( buttons)," ", ncol( buttons), "\n")
  
  comp <- compositions( joltage[ target_idx ], length( target_buttons ) )
  comp
ncol(comp)
  pressed <- matrix( 0, ncol=ncol(comp), nrow = ncol(buttons))
  
  pressed[target_buttons, ] <- comp
  jremain = joltage - buttons %*% pressed
  
  if( any( apply( jremain, 2, \(x) sum( abs(x)))== 0))
    return( joltage[target_idx])
  
  n_presses <- map_dbl( 1:ncol(pressed), \(c) presses_joltage0( buttons[-target_idx,-target_buttons], jremain[-target_idx, c ])) 

  if( is.infinite( out ))
    return( NA )
  
  return( n_presses + joltage[target_idx] )
}


bb
jj

buttons
line = input[1]
line
joltage <- get_joltage_v( line )
buttons <- get_button_m( line, length(joltage))

presses_joltage0( buttons, joltage )

joltage_remainder[-1,3]

xx<-as.numeric( NA)
xx
mm <- min( xx, na.rm=T )
is.infinite(mm)


vv<-1:5
vv
vv[-c(3,4)]

mm <- matrix(1:9,nrow=3)
mm

mm[-c(1,2),-3]

which.min( 5:1)

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
