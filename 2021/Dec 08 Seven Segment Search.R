library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 08"
ff <- "input"
#ff<-"test"
lines <- readLines( file.path( dir, ff))
#lines <- "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
input_raw <- str_split( lines, " ", simplify=T)

mm <- matrix(str_length( input_raw), nrow=nrow(input_raw), ncol=ncol( input_raw))

out <-mm[,12:15]

answer1 <- sum(out==7 | out==4 | out==3 | out==2)

unscrambled <- c( "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" )
strokes <- head(letters, 7 )
place_value = c(1000,100,10,1)


str_to_vec <- function( ss  )
{
  as.vector( str_split( ss, "", simplify = T))
}

has_strokes <- function( real_ss, exact=F )
{
  tok <- lapply( digit_list, str_to_vec )
  ss  <- strokes[ real_stroke %in% real_ss ]
  if( exact )
    has_ss <- digit_list[sapply( tok,  function(x) setequal(ss,x) )]
  else
    has_ss <- digit_list[sapply( tok,  function(x) length( setdiff(ss,x) )==0)]
  
  has_ss
}


answer2=0
for( line_num in 1:nrow(input_raw))
{
  input_sorted <- sapply( lapply( lapply( input_raw[line_num,], str_to_vec ), sort), function( x) paste0( x,collapse=""))
  
  digit_list <- input_sorted[1:10]
  readout    <- input_sorted[12:15]
  #digit_list <- unscrambled
  has_stroke <- sapply( strokes, function(x) str_count( digit_list, x))
  digit_count <- apply( has_stroke, 2, sum)
  digit_count
  
  digits_ordered <- rep( "", 10)
  stroke_count <- str_length( digit_list)
  
  digits_ordered[2] = digit_list[ stroke_count==2 ] # 1
  digits_ordered[8] = digit_list[ stroke_count==3 ] # 7
  digits_ordered[5] = digit_list[ stroke_count==4 ] # 4
  digits_ordered[9] = digit_list[ stroke_count==7 ] # 7
  digits_ordered
  
  real_stroke = rep("",7)
  names( real_stroke) = strokes

  real_stroke[[ strokes[ digit_count == 4 ] ]] = "e" # e
  real_stroke[[ strokes[ digit_count == 6 ] ]] = "b" # b
  real_stroke[[ strokes[ digit_count == 9 ] ]] = "f" # f
  
  # 1+f -> c
  cc <- setdiff( str_to_vec( digits_ordered[2] ), strokes[ real_stroke == "f"] )
  real_stroke[[cc]] = "c"
  
  # 7+cf -> a
  cf <- strokes[ real_stroke %in% c( "c", "f" )]
  aa <- setdiff( str_to_vec( digits_ordered[8] ), cf )
  real_stroke[[ aa ]] = "a"
  
  # 4 + bcf -> d
  bcf <- strokes[ real_stroke %in% c( "b", "c", "f" ) ]
  dd <- setdiff( str_to_vec( digits_ordered[5] ), bcf )
  real_stroke[[ dd ]] = "d"
  
  # abcef+8 -> 0
  digits_ordered[1] = setdiff( has_strokes( c("a","b","c","e","f")), digits_ordered[9])
  
  # 0 + abcef -> g
  gg <- setdiff( str_to_vec(digits_ordered[1] ), strokes[ real_stroke %in% c( "a","b","c","e","f") ]) 
  real_stroke[[gg]] = "g"
  
  digits_ordered[10] = has_strokes( str_to_vec( unscrambled[10]), exact=T)
  digits_ordered[7]  = has_strokes( str_to_vec( unscrambled[7]), exact=T)
  digits_ordered[6]  = has_strokes( str_to_vec( unscrambled[6]), exact=T)
  digits_ordered[4]  = has_strokes( str_to_vec( unscrambled[4]), exact=T)
  digits_ordered[3]  = has_strokes( str_to_vec( unscrambled[3]), exact=T)
  
  readout_decoded = sum((sapply( readout, function( x) which( digits_ordered == x ))-1)*place_value)
  answer2 = answer2+readout_decoded
  cat( readout_decoded," ")
}
answer2

