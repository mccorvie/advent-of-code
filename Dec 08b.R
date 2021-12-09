
# 
# Solution 2:
# For each digit, calculate a signature which is unique and doesn't change if you scramble the 
# segments or digit order.  Then you can just match the scrambled signatures to the unscrambled 
# signatures.  The list of lengths of the intersections of a given digit with all the other 
# digits is a signature which satisfies these properties.
#

dir <- "~/Desktop/Advent-Code-2021/Dec 08"
ff <- "input"
input_raw <- file.path( dir, ff) |> readLines() |> str_split( " ", simplify=T)

str_to_vec <- function( ss  ) as.vector( str_split( ss, "", simplify = T))

generate_signatures <- function( digit_list )
{
  signatures = as.list(rep(0,10))
  for( i in  1:10)
    for( j in 1:10 )
      signatures[[i]][j] = length( intersect( digit_list[[i]] , digit_list[[j]]))
  lapply( signatures, sort )
}

unscrambled <- c( "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" )
unscrambled_sig <- generate_signatures( lapply( unscrambled, str_to_vec) )

answer2 =0 
for( line_num in 1:nrow( input_raw))
{
  digit_list <- lapply( input_raw[line_num,1:10], str_to_vec )
  signatures <- generate_signatures(digit_list)
  
  # match the unscrambled digits to the scrambled digits by their signatures
  remap <- signatures |> 
    sapply( function( x) sapply(unscrambled_sig, function( y ) all( x==y ) )) |>
    apply( 2, which)
  
  # identify which digits in the digit list correspond to the readout digits
  readout <- lapply( input_raw[line_num,12:15], str_to_vec ) |>
    sapply( function( x) sapply( digit_list, function( y ) setequal( x,y ) )) |>
    apply( 2, which)
  
  answer2 = answer2 + sum( ( remap[ readout ] -1)* c(1000,100,10,1))
}
answer2



# Check the signatures are unique
#for( i in 1:10)
#  for( j in 1:10)
#    if( i != j && all(unscrambled_sig[[i]] == unscrambled_sig[[j]]))
#      stop( paste( i, "and", j, "match"))
