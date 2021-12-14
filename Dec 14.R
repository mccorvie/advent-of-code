library( tidyverse)
dir <- "~/Desktop/Advent-Code-2021/Dec 14"
ff  <- "input"
input_raw <- readLines( file.path( dir, ff))

polymer0 <- polymer <- unlist( str_split( input_raw[1], ""))
transition_table <- str_split( input_raw[3:length(input_raw)], "->", simplify=T)
trans_match <- str_trim( transition_table[,1])
transition  <- str_trim( transition_table[,2])
names( transition) <- trans_match
# 
# for( i in 1:10)
# {
#   new_polymer <- NULL
#   for( loc in 1:(length(polymer)-1))
#   {
#     trans_char <- transition[ paste0(polymer[loc], polymer[loc+1] )]
#     new_polymer <- c( new_polymer, polymer[loc])
#     if( !is.na( trans_char ))
#       new_polymer <- c( new_polymer,trans_char)
#   }
#   cat( i, length( polymer), "->", length( new_polymer), "\n")
#   
#   polymer <- c( new_polymer, last(polymer))
# }
# answer1=max( table( polymer))-min( table( polymer))
# cat( answer1)

polymer <- polymer0
dimer_list <- paste0(rep(LETTERS,times=26), rep(LETTERS,each=26))
polymer_count <- rep(0, length(dimer_list))
names( polymer_count )<-dimer_list
for( loc in 1:(length(polymer)-1))
{
  key = paste0( polymer[loc], polymer[loc+1])
  polymer_count[ key ] = polymer_count[ key ] +1
}
#polymer_count

sum( polymer_count)
length( polymer)

for( i in 1:40 )
{
  new_polymer_count <- rep(0, length(dimer_list))
  names( new_polymer_count )<-dimer_list
  
  for( dimer in dimer_list)
  {
    if( polymer_count[dimer] == 0)
      next
    
    trans_char <- transition[ dimer ]
    if( !is.na( trans_char ))  
    {
      ins_dimer1 = paste0( str_sub( dimer, 1,1), trans_char )
      ins_dimer2 = paste0( trans_char, str_sub( dimer, 2,2))
      new_polymer_count[ ins_dimer1 ] = new_polymer_count[ ins_dimer1 ] + polymer_count[ dimer ] 
      new_polymer_count[ ins_dimer2 ] = new_polymer_count[ ins_dimer2 ] + polymer_count[ dimer ] 
    }
    else
    {
      new_polymer_count[ dimer ] = polymer_count[ dimer ] 
    }
  }
  cat( i, sum( polymer_count), "->", length( new_polymer_count), "\n")
  
  polymer_count <- new_polymer_count 
}
polymer_count
polymer_summary <- rep(0, length( LETTERS))
names( polymer_summary ) <- LETTERS
for( let in LETTERS)
  polymer_summary[let] = sum(polymer_count[paste0( let, LETTERS)])

polymer_summary[last(polymer)] <- polymer_summary[last(polymer)]+1
polymer_summary = polymer_summary[ polymer_summary!=0]
answer2 <- sprintf( "%.100g",max( polymer_summary) - min(polymer_summary))
answer2
# 1 20 -> 38 
# 2 39 -> 76 
# 3 77 -> 152 
# 4 153 -> 304 
# 5 305 -> 608 
# 6 609 -> 1216 
# 7 1217 -> 2432 
# 8 2433 -> 4864 
# 9 4865 -> 9728 
# 10 9729 -> 19456 
# 3587
