#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

day = 20
raw  <- readLines( paste0( "input", day ))
test <- readLines( paste0( "test", day ))


printme = \( mixme,   pos=1)
{

  for( i in 1:length( mixme ))
  {
   
    # cat( "(<- ", mixme[[pos]]$predpos, ")", pos, "=", mixme[[pos]]$val, " (", mixme[[pos]]$succpos, "->)" )
    cat( mixme[[pos]]$val, " " )
    pos = mixme[[pos]]$succpos
  }
  cat( "\n")
  
}



mixwrap = \(idx)  (idx -1) %% length( input) +1


sanity = \( mixme,   pos=1)
{
  forward = backward = pos
  
  for( i in 1:(length( mixme )-1))
  {
    foward = mixme[[forward ]]$succpos
    backward = mixme[[backward ]]$predpos
    if( forward == pos || backward == pos )
    {
      cat( "!!! 1 ")
      return( F )      
    }
  }
  if( backward$predpos != pos || foward$succpos != pos )
  {
    cat( "!!! 2")
    return( F )      
  }
  
  return( T)
}

use_test = T
if( use_test )
{
  input <- test
} else {
  input <- raw
}


mixme <- map( 1:length( input), ~ list( val = strtoi( input[.] ), idx=., succpos = .+1, predpos = .-1 ))
mixme[[ 1]]$predpos = length( mixme )
mixme[[ length( mixme )]]$succpos = 1

# succ <- \(node) node["succpos"]
# pred <- \(node) node["predpos"]
# pred 
# idx=3

#cutpoint = detect( mixme, ~ .["idx"] == idx )
part = 2
if( part == 1)
{
  times = 1
  key = 1 
  
} else {
  
  
  key = 811589153
  times = 10
}


for( t in 1:times )
for( pos in 1:length( mixme ))
{
  # if( !sanity( mixme ))
  #   break
  
  cutpoint = mixme[[pos]]
  if( cutpoint$val == 0 )
    next

  # cut
  mixme[[ cutpoint$predpos ]]$succpos = cutpoint$succpos
  mixme[[ cutpoint$succpos ]]$predpos = cutpoint$predpos
  
  dpos = cutpoint$val
  if( part == 2 )
    dpos = sign( dpos ) * ( ( dpos * key -1 )%% (length( mixme)-1)+1)

  
  inspoint = pos
  if( sign( dpos ) > 0 )
  {
    for( i in 1:abs( dpos  ))
      inspoint = mixme[[inspoint]]$succpos
    inspoint 
    mixme[[inspoint]]
    mixme[[ pos ]]$succpos = mixme[[ inspoint ]]$succpos
    mixme[[ pos ]]$predpos = inspoint
    
    mixme[[ mixme[[ inspoint ]]$succpos ]]$predpos = pos
    mixme[[ inspoint ]]$succpos = pos
    
  } else {
    for( i in 1:abs( dpos ))
      inspoint = mixme[[inspoint]]$predpos
    
    mixme[[ pos ]]$predpos = mixme[[ inspoint ]]$predpos
    mixme[[ pos ]]$succpos = inspoint
    mixme[[ mixme[[ inspoint ]]$predpos ]]$succpos = pos
    mixme[[ inspoint ]]$predpos = pos
  }
  
 #printme( mixme )
}
pos = detect_index( mixme, ~ .$val == 0)
out = 0
for( times in 1:3 )
{
  for( i in 1:1000 )
    pos = mixme[[pos]]$succpos
  cat( mixme[[pos]]$val * key, " " )
  out = out + mixme[[pos]]$val* key
  
}
printme( mixme)
cat( out, "\n")



# 1611

#insert
mixme[[ idx ]]$succpos = inspoint$
mixme[[ inspoint$succpos ]]$predpos = idx
mixme[[ inspoint$predpos ]]$succpos = idx

any( strtoi( input ) * 811589153 %% length( input) ==0 & input != 0)

811589153 %% length( input)
length( input)
##
## subsetting manipulations
##


mixed_indices <- 1:length( mixme)



for( idx in 1:length( input ))
{
  #  cat( mixme, "\n")
  
  cutpoint = match( idx, mixed_indices )
  mixval   = mixme[ cutpoint ]
  dp = if( mixval < 0) mixval-1 else mixval
  inspoint = mixwrap( cutpoint + dp )
  if( inspoint >= cutpoint )
    inspoint = inspoint -1
  seq1 = 1:inspoint
  seq2 = if( inspoint == length( mixme )-1) NULL else (inspoint+1):(length( mixme)-1)
  #  cat( ">> cut ", cutpoint, "val ", mixval, "ins ", inspoint, "seq1 ", seq1, "seq2 ", seq2, "\n")
  mixme = mixme[ -cutpoint]
  mixme  = c( mixme[ seq1 ], mixval, mixme[seq2])
  mixed_indices = mixed_indices[ -cutpoint ]
  mixed_indices  = c( mixed_indices[seq1], idx, mixed_indices[seq2])
  
  mixed_indices
  
}


cat( mixme, "\n")

coordbase = match( 0, mixme )
mixme[ mixwrap( coordbase+1000 ) ] + mixme[mixwrap( coordbase+2000) ] +  mixme[mixwrap( coordbase+3000 )]
# 5195
