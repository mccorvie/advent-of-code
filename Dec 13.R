
dir <- "~/Desktop/Advent-Code-2021/Dec 13"
ff  <- "input"
input_raw <- readLines( file.path( dir, ff))
# swap x,y because matrix coords are r,c, +1 because x,y coords are 0-indexed
points <- matrix(strtoi( str_split( input_raw[1:908], ",", simplify=T)), nrow=908)[,2:1]+1 
sheet  <- matrix( F, nrow = max( points[,1]), ncol = max( points[,2]))
sheet[points] = T
instructions= input_raw[910:921]

answer1=NULL
for( instruction in instructions )
{
  fold_loc = strtoi( str_match( instruction, "[0-9]+$")) +1
  # if vertical fold, turn the page and do a horizontal fold
  if( str_detect( instruction, "y="))
    sheet=t(sheet)
  fold_span = ncol(sheet)- fold_loc
  sheet[,(fold_loc-fold_span):(fold_loc-1)] = sheet[,(fold_loc-fold_span):(fold_loc-1)] | sheet[,(fold_loc+fold_span):(fold_loc+1)]
  sheet <- sheet[,1:(fold_loc-1)]
  if( str_detect( instruction, "y="))
    sheet=t(sheet)
  if( is.null(answer1))
    answer1 = sum( sheet)
}

cat( paste0(sapply( 1:nrow( sheet), function( x) paste0( if_else( sheet[x,], "#", " "), collapse="")), collapse="\n"))
