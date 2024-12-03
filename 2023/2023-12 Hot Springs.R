# install.packages("devtools")
# devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 12
raw <- read_advent(day = day, year=2023) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day ))

use_test = F
input = if( use_test ) test else raw




impossible <- function( template, spec )
{
  sum(spec) < sum( template=="#")  || 
    sum( spec)  > sum( template == "#" | template=="?" ) || 
    sum(spec) + length( spec) > length(template) || 
    length( template ) > 0 && last(template) == "#"
}

take_largest <- function( template, spec, depth =1)
{
  key = paste0( "|",paste0( template,collapse=""), "|",paste(spec, collapse=","))
  #  cat( strrep( " ", depth ), key, "\n", sep="")
  
  if( !is.null( cache[[key]])) return( cache[[key]])
  if( length( spec) == 0) return( 1 )

  largest_spec <- max( spec )
  largest_idx  <- which.max( spec )
  spec_l <- spec[ seq_along( spec )<largest_idx ]
  spec_r <- spec[ seq_along( spec )>largest_idx ]

  total <- 0
  for( start_pos in 1:(length( template)-largest_spec ))
  {
    template_l <- template[ seq_along( template) < start_pos ]       
    template_r <- template[ seq_along( template) > start_pos+largest_spec ] 
    if( 
      impossible( template_l, spec_l) ||
      impossible( template_r, spec_r) ||
      any( template[seq( start_pos, start_pos+largest_spec-1)] == "." ) ||
      template[start_pos+largest_spec] == "#"
    )
      next

    total <- total + take_largest( template_l , spec_l, depth+1) * take_largest( template_r, spec_r, depth+1)
  }
  cache[[key]] <<- total
  total
}

# part 1
templates <- input |> str_split( " ") |> map_chr( \(v) v[1] )
specs     <- input |> str_split( " ") |> map(  \(v) as.numeric(str_split_1( v[2], ",")))

#part 2
templates <- input |> str_split( " ") |> map_chr( \(v) paste( rep( v[1], 5), collapse = "?") )
specs     <- input |> str_split( " ") |> map(  \(v) rep( as.numeric( str_split_1( v[2], ",")),5))

cache <- list()
tt<-0
for( idx in seq_along( templates))
{
  # the search logic above assumes all blocks can end in "."
  template <- c( str_split_1( templates[idx], ""), ".")
  spec     <- specs[[idx]]
  res <- take_largest( template, spec)
  tt <- tt + res
}






##
##
##  Old solutions
##

#.??..??...?##.

choose( 14, 4)

#123456789012345
#?#?#?#?#?#?#?#?

mm <- input |> str_split( " ")  |> 
  map( \(v) tibble( template = v[1], spec = list( as.numeric(str_split_1( v[2], ","))))) |> 
  reduce( add_row ) |> 
  rowwise() |> 
  mutate( 
    template2 = paste( rep( template, 5), collapse = "?"),
    spec2     = list( rep( spec, 5 )),
    template = template2,
    spec     = list( spec2 )
  ) |> 
  mutate( 
    template_len = str_length(template),
    spec_cnt     = length( spec ),
    spec_len     = sum( spec ),
    gap          = template_len - spec_len,
    base_len     = template_len - spec_len + spec_cnt,
    choose       = choose( template_len, spec_cnt),
    choose2      = choose( base_len, spec_cnt)
  ) |> 
  ungroup()

mm |> summarize( cc = sum( choose2 ))

part1 <- 0
for( idx in 1:nrow(mm))
{
  entry <- mm[idx,]
  
  combos <- combn( entry$base_len, entry$spec_cnt) |> t()
  
  base <- rep( ".", entry$base_len )
  insert <- entry$spec |> unlist() |> map_chr( \(len) strrep( "#", len))
  template_v <- str_split_1( entry$template, "")
  
  construct_pattern <- \( v )
  {
    out <- base
    out[v] = insert
    str_split_1( paste0( out, collapse="") , "")
  }
  
  valid <- 1:nrow( combos ) |> 
    map( \(rr) combos[rr,]) |> 
    keep( \(v) all( diff(v)!=1)) |>
    map( construct_pattern ) |> 
    keep( \(pattern) all(  pattern[template_v!="?"] == template_v[ template_v!="?"]))
  
  part1 <- part1 + length( valid)
}
part1


fill_in <- \(template, spec, depth =1)
{
  
  recurse <<- recurse + 1
  if( recurse %%10000 == 0)
    cat( recurse, "/", depth, "/", length( spec), "  ")
  #cat( strrep( " ", depth ),template, " ", spec, "\n")
  
  if( length( spec) == 0)
  {
    if( str_detect( template, "#")) return( 0 )
    return( 1 )
  }
  
  if( sum( spec) + length( spec ) -1 > str_length( template))
    return( 0 )
  
  
  
  # match partially completed template
  template_complete <- str_extract( template, "\\.*(#+\\.+|#+$)*" )
  spring_counts <- template_complete |> 
    str_split_1( "\\.") |> 
    str_length() |> keep( \(x) x>0)
  
  if( 
    length( spring_counts ) > length( spec ) ||
    any( spring_counts != head( spec, length( spring_counts)))
  )
    return( 0 )
  
  # trim completed pattern
  template <- str_sub( template, str_length( template_complete)+1)
  if( length( spring_counts) > 0)
    spec <- tail( spec, -length( spring_counts))
  
  if( length( spec) == 0)
  {
    if( str_detect( template, "#")) return( 0 )
    return( 1 )
  }
  
  if( sum( spec) + length( spec ) -1 > str_length( template))
    return( 0 )
  
  total <- 0
  
  # fill in 1st ? with .
  if( str_sub( template, 1,1) == "?")
    total <- total + fill_in( str_sub( template, 2 ), spec, depth+1 )
  
  # fill in next block starting here
  fill_me <- str_extract( template, "(\\?|#)+")
  
  next_spec <- head( spec, 1)
  if( str_length( fill_me) > next_spec) 
  {
    if( str_sub( fill_me, next_spec+1, next_spec+1 ) == "?" )
      total <- total + fill_in( str_sub( template, next_spec+2), tail( spec, -1), depth+1 )
    
  } else if( str_length( fill_me) == next_spec) 
  {
    total <- total + fill_in( str_sub( template, next_spec+1), tail( spec, -1), depth+1 )
  }
  
  
  total
}





recurse <- 0
tt<-0
for( idx in seq_along( templates))
{
  template <- templates[idx]
  spec     <- specs[[idx]]
  res <- fill_in( template, spec)
  cat( "****** ", res, "\n")
  tt <- tt + res
}
tt

