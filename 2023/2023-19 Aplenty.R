# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 19
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = T
input = if( use_test ) test else raw


section   <- cumsum( input=="")
workflows <- input[section==0]
parts     <- input[section==1] |> tail( -1)

perform_workflow <- \(x,m,a,s,spec)
{
  rules <- str_split_1( spec, ",") |> unglue_data( c("{condition}:{action}", "{action}")) |> 
    mutate( condition = coalesce(condition, "T")) |> 
    mutate( action_r  = paste0(action, "(x,m,a,s)"))

  for( idx in 1:nrow(rules ))
    if( eval( str2lang( rules[idx,"condition"])))
      return( eval( str2lang( rules[idx,"action_r"])))

  stop("shouldn't get here")  
}

A <- \(x,m,a,s) T
R <- \(x,m,a,s) F

function_factory <- \(spec) return( \(x,m,a,s) perform_workflow( x,m,a,s,spec ))

# in is a reserved word in R
workflow_match <- str_replace( workflows, "in\\{", "begin\\{") |> 
  str_match(  "([a-z]+)\\{(.*)\\}")
walk2( workflow_match[,2], workflow_match[,3], \(name, spec ) assign( name, function_factory( spec ), pos=1))

do_it <- \(init)
{
  init <- str_replace_all( init, c( "," = ";" ) )
  eval( str2lang( init ))
  if( begin( x,m,a,s))
    return( x+m+a+s)
  return( 0 )
}
map_dbl( parts, do_it ) |> sum()


var_condition <- str_extract_all( workflows, "(x|m|a|s)(>|<)\\d+") |> unlist() |> unglue_data( "{variable}{sign=(>|<)}{thresh=\\d+}")

var_condition |> group_by( variable ) |> summarize( n=n()) 

xx<-var_condition |> arrange( variable ) |> ungroup() |> summarize( p = prod(n))



workflow_match <- str_replace( workflows, "in\\{", "begin\\{") |> 
  str_match(  "([a-z]+)\\{(.*)\\}")

workflow_index <- workflow_match[,3]
names( workflow_index ) <- workflow_match[,2]

make_node <-\( condition )
{
  return( list( condition = condition, pass = list(), fail = list()))
}

eval_tree <- \(name)
{
  
}

name = "begin"
spec = workflow_index[name]  

rules <- str_split_1( spec, ",") |> unglue_data( c("{condition}:{action}", "{action}")) 
rules

|> 
  mutate( condition = coalesce(condition, "T")) |> 
  mutate( action_r  = paste0(action, "(x,m,a,s)"))


