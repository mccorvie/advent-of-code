library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day14"
mask_data <- readLines( file.path( dir, ff))

hex_to_bin = c("0000", "0001", "0010", "0011","0100","0101","0110","0111", "1000", "1001", "1010", "1011","1100","1101","1110","1111")
names( hex_to_bin) = c( "0":"9", letters[1:6])

to_binary <- function( x ) strtoi( unlist(str_split(recode( str_split( sprintf( "%x", strtoi( x)),"")[[1]], !!!hex_to_bin ), "")))
to_dec <- function( x ) sum( x * 2^((length(x)-1):0))


registers <- list()
for( line in mask_data )
{
  mask_match <- str_match( line, "^mask = ([01X]+)")[,2]
  if( !is.na( mask_match))
  {
    mask <- strtoi(str_split( mask_match, "")[[1]])
    next
  }
  
  register_data <- str_match( line, regex( "^mem\\[(\\d+)\\] = (\\d+)$") )[1,2:3]
  bin <- to_binary( register_data[2])
  bin <- c( rep(0, length( mask)-length( bin)), bin)
  bin <- coalesce( mask, bin)
  
  registers[[register_data[1]]] <- to_dec( bin )
}
sprintf( "%.100g",sum(unlist(registers)))


registers <- list()
for( line_idx in 1:length(mask_data ))
{
  line_idx<-1
  line <- mask_data[line_idx]
  mask_match <- str_match( line, "^mask = ([01X]+)")[,2]
  if( !is.na( mask_match))
  {
    mask <- str_split( mask_match, "")[[1]]
    next
  }
  
  register_data <- str_match( line, regex( "^mem\\[(\\d+)\\] = (\\d+)$") )[1,2:3]

  names(mask) <- paste0( "v", 1:length(mask))
  mem_address <- as_tibble( matrix(mask, nrow=1) )
  
  for( float in 0:(2^float_size-1))
  {
    float <- tail( c( rep(0,length(mask)),to_binary( float)), float_size )
    mem_address <- tail( c( rep(0,length(mask)), to_binary( register_data[1])), length(mask))
    mem_address <- coalesce(mask,0) | mem_address
    mem_address[ is.na(mask)] = float
    #cat( to_dec( mem_address), "=", register_data[2],"\n")
    registers[[ paste( mem_address, collapse="")]] = strtoi( register_data[2])
  }
}
sprintf( "%.100g",sum(unlist(registers)))

registers

cross <- tibble( V = "X", N = 1:2)
mem_address %>% left_join( cross, by( V="V8"))

