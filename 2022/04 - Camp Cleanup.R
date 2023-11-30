library( tidyverse )
dir <- "~/Desktop/aoc-input"
ff  <- "input04"

input <- readLines( file.path( dir, ff))
aa <- tibble( raw=input) |>
  extract( raw,c( "elf1.l", "elf1.u", "elf2.l", "elf2.u" ), "(\\d+)-(\\d+),(\\d+)-(\\d+)") |>
  mutate( 
    across( starts_with( "elf"), strtoi),
    min_len = pmin( elf1.u-elf1.l, elf2.u-elf2.l ) +1,
    inner.l = pmax( elf1.l, elf2.l),
    inner.u = pmin( elf1.u, elf2.u), 
    inner_len = pmax( 0, inner.u-inner.l+1 )
  ) 

# part1
aa |> summarize( sum( inner_len == min_len))

# part2 
aa |> summarize( sum( inner_len>0))



##
## Coordinate comparisons ----
## @drob
##


# unfortunately, dplyrs between() expects start and end to be single values!
# So I rewrote it

between <- function( x,start,end) x>=start & x<=end 

parsed <- tibble( x=input ) |>
  extract( x, c( "x1","x2", "y1","y2"), "(\\d+)-(\\d+),(\\d+)-(\\d+)", convert=T) 

parsed |>
  filter( between( x1, y1, y2 ) & between( x2, y1, y2) |
            between( y1, x1, x2 ) & between( y2, x1, x2)) |>
  nrow()

parsed |>
  filter( between( x1, y1, y2 ) | between( x2, y1, y2) |
            between( y1, x1, x2 ) | between( y2, x1, x2)) |>
  nrow()



##
## Interpret the input as R ----
## @antoine_fabr 
## 
  
# part1

sum( sapply( 
  parse( text=chartr( "-,", ":~", input)),
  eval,
  list( "~" = \(x,y) all( x %in% y) || all( y %in% x))
))

# part2

sum( sapply( 
  parse( text=chartr( "-,", ":~", input)),
  eval,
  list( "~" = \(x,y) all( x %in% y) || all( y %in% x))
))
