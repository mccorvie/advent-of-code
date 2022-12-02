library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input01"

##
## How to get the data from the input file -----
## 

ss <- readLines( file.path( dir, ff)) |> strtoi()
ss <- scan( file.path( dir, ff), blank.lines.skip = F)


##
## explicit loop ----
##

max_elf = 0
cur_elf = 0
for( cal in ss )
{
  if( is.na( cal ))
  {
    max_elf = max( cur_elf, max_elf)
    cur_elf=0
    next
  } 
  
  cur_elf = cur_elf + cal
}

max_elf = max( cur_elf, max_elf )


##
## Interlude 1: Pipes -----
##

# the magrittr pipe

x %>% f() %>% g()

# is the same as 
g( f( x))

# the built-in pipe
x |> f() |> g()

##
##  Interlude 2: purrr tricks (functional programming) -----
##

# anonymous functions

~ max( sum( . ))

# is the same as
f <- function( x )
{
  max( sum( x))
}

# two arguments
~ max( sum( .x, .y))

# map iterates over a list (or vector) and returns a list

f = function( x )
{
  if( x %% 3 == 0) 
    "multiple of 3" 
  else 
    "could be prime"
}

# get a list
map( 1:10, f )

# get a character vector
map_chr( 1:10, f)

# get a double vector.  Note map is nice with anonymous functions
map_dbl( 1:10, ~ .%%3 + .%%5  )

#
# I like purr better than lapply(), vapply(), sapply() 
#

##
## Ryan's solution -----
##

#input_raw <- read_csv( file.path( dir, ff) ) 
ss <- readLines( file.path( dir, ff)) |> strtoi()

gg  <- is.na( ss) |> cumsum()
cal <- split( ss, gg) |> map_dbl(~ sum(., na.rm=T)) 

sola <- max( cal )
solb <- sum( sort( cal, decreasing=T)[1:3])


##
## String processing instead of vector processing -----
##

#
# paste and split
#

ll <- readLines( file.path( dir, ff)) |>
  str_replace( "^$", "|") |>
  paste( collapse=" ") |>
  str_split( "\\|" )

str_split( str_trim(ll[[1]]), "\\s+") |> map( as.numeric )

cal <- map_dbl( cal_list, ~ sum( ., na.rm=T))


#
# Same idea on a big blob of characters
# 

# due to Kara Woo (@kara_woo)

ll <- read_file( file.path( dir, ff))
str_split( ll, "\n\n")[[1]] |>
  str_split( "\n" ) |> 
  map( as.numeric )



##
## Tidyverse and data tables instead of lists and vectors ----
##

# Due to David Robinson (@drob)

cal_tt <- tibble( cal = ss) |>
  mutate( gg = cumsum( is.na(cal))) |>
  count( gg, wt=cal)

summarize( cal_tt,  max( n))

cal_tt |> 
  arrange( desc( n)) |>
  head(3) |>
  summarize( sum(n))

