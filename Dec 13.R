
dir <- "~/Desktop/Advent-Code-2021/Dec 13"
ff  <- "input"
input_raw <- readLines( file.path( dir, ff))

last_point <- 908
if( ff == "test")
  last_point = length( input_raw )

# +1 because x,y coords are 0-indexed
points <- matrix(strtoi( str_split( input_raw[1:last_point], ",", simplify=T)), nrow=last_point)+1 
# swap x, y because matrix coords are r,c
points <- points[,2:1]
sheet  <- matrix( F, nrow = max( points[,1]), ncol = max( points[,2]))
sheet[points] = T

instructions= input_raw[910:921]

# instruction 1
fold_loc = 655+1
fold_span = ncol(sheet)- fold_loc

sheet[,(fold_loc-fold_span):(fold_loc-1)] = sheet[,(fold_loc-fold_span):(fold_loc-1)] | sheet[,(fold_loc+fold_span):(fold_loc+1)]
sheet <- sheet[,(fold_loc-fold_span):(fold_loc-1)]
answer1 = sum( sheet)

for( instruction in instructions[-1] )
{
  fold_loc = strtoi( str_match( instruction, "[0-9]+$")) +1
  
  # if vertical fold, turn the page and do a horizontal fold
  if( str_detect( instruction, "y="))
  {
    sheet=t(sheet)
  }
  
  fold_span = ncol(sheet)- fold_loc
  cat( ncol(sheet), "(", fold_loc-fold_span, "|", fold_loc, "|", fold_loc+fold_span,")\n")
  sheet[,(fold_loc-fold_span):(fold_loc-1)] = sheet[,(fold_loc-fold_span):(fold_loc-1)] | sheet[,(fold_loc+fold_span):(fold_loc+1)]
  sheet <- sheet[,1:(fold_loc-1)]
  
  if( str_detect( instruction, "y="))
  {
    sheet=t(sheet)
  }
}

for( i in 1:nrow(sheet))
{
  for( j in 1:ncol(sheet))
    if(sheet[i,j])
      cat( "X" )
    else
      cat( " ")
  cat("\n")  
}
