# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 20
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw

pattern = "{type=(&|%|broadcast)}{label=[a-z]+} -> {output_str}"
modules <- unglue_data( input, pattern) |> 
  mutate( output = str_split( output_str, ", ")) |> 
  unnest( output) |> 
  select( -output_str)

conj_list <- modules |> filter( type == "&") |> select( label ) |> distinct() |> pull( label)
init_conj_mem <- \(conj)
{
  conj_input <- modules |> filter( output == conj) |> pull( label )
  setNames( rep( F, length( conj_input )), conj_input)
}

conj_mem <- set_names( map( conj_list, init_conj_mem ), conj_list)
conj_list <- modules |> filter( type == "&") |> select( label ) |> distinct() |> pull( label)
flip_list <- modules |> filter( type == "%") |> select( label ) |> distinct() |> pull( label)
flip_mem <- set_names( rep( F, length(flip_list)), flip_list)
bcast <- filter( modules, type == "broadcast") |> pull( output)

all_pulses = tibble( source = character(), dest = character(), pulse=logical())
for( idx in 1:1000)
{
#  cat( idx, " ")
  pulses <- tibble( source = "er", dest = bcast, pulse= F)
  cc<-0
  while( cc < nrow( pulses ))
  {
    cc<-cc+1
    
    ll <- pulses[cc,]$dest
    
    pp <- pulses[cc,]$pulse
    source <- pulses[cc,]$source
    if( ll == "hj" && pp )
    {
      cat( idx, cc, ")", source, pp, "->", ll,"\n")
    }
    
    tt  <- modules |> filter( label == ll ) |> pull( type ) |> unique()
    out <- modules |> filter( label == ll ) |> pull( output )
    
    #cat( cc, ")", source, pp, "->", ll, "|", tt, length( out ), "\n")
    if( length( tt ) == 0) next
    
    if( tt == "%" && !pp)
    {
      flip_mem[ll] <- !flip_mem[ll]
      #cat( "flip to", flip_mem[ll],"\n")
      pulses  <- pulses |> add_row( tibble( source = ll, dest = out, pulse = flip_mem[ll]))
    }  
    if( tt == "&")
    {
      source <- pulses[cc,]$source
      conj_mem[[ll]][source] <- pp
      pp_out <- !all( conj_mem[[ll]] )
      pulses <- pulses |> add_row( tibble( source = ll, dest = out, pulse = pp_out ))  
    }
  }
  all_pulses <- all_pulses |> add_row( source= "button", dest= "er", pulse = F ) |> add_row( pulses )
}


all_pulses |> group_by( pulse ) |> summarize( n=n()) |> pull( n) |> prod()

filter( modules, output=="hj")
pulses
flip_mem
conj_mem
is.null(tt)
modules
conj_mem["hj"]
select( modules, label) |> distinct() |> print( n=100)
select( modules, label, output ) |> print( n=200)

