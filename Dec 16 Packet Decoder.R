
dir <- "~/Desktop/Advent-Code-2021/Dec 16"
ff  <- "input"
chunk <- readLines( file.path( dir, ff))

# part1 test
# chunk <- "8A004A801A8002F478"
# chunk <- "620080001611562C8802118E34"
# chunk <- "C0015000016115A2E0802F182340"
# chunk <- "A0016C880162017C3686B18A3D4780"

# part2 test

#chunk <-"C200B40A82" 
# chunk <-"04005AC33890" 
# chunk <-"880086C3E88112" 
# chunk <-"CE00C43D881120" 
# chunk <-"D8005AC2A8F0" 
# chunk <-"F600BC2D8F" 
# chunk <-"9C005AC2F8F0" 
# chunk <-"9C0141080250320F1802104A08"

print_binary <- function( xx, width)
{
  for( shift in (width-1):0)
  {
    mask <- bitwShiftL( 1, shift )
    cat( bitwShiftR( bitwAnd( xx,mask), shift))
    xx <- bitwXor( mask, xx )
  }
}


buffer <- buffer_sz <- 0L
loc <- 1
cum_bits_requested <- 0

buff_sz_list <- NULL
get_bits <- function( nn )
{
  cum_bits_requested <<- cum_bits_requested + nn
  while( buffer_sz < nn )
  {
    if( loc > str_length( chunk))
      stop( "no more bits for you")
    
    buffer <<- bitwOr(bitwShiftL(buffer,4),strtoi( str_sub( chunk, loc, loc), base=16L))
    loc    <<- loc+1
    buffer_sz <<-buffer_sz + 4
  }  
  out    <- bitwShiftR( buffer, buffer_sz - nn)
  buffer <<- bitwXor( buffer,  bitwShiftL( out, buffer_sz - nn))
  buffer_sz <<- buffer_sz - nn
  buff_sz_list <<- c( buff_sz_list, buffer_sz)
  
  out
}


packet_tree <- function()
{
  VV <- get_bits( 3 )
  TT <- get_bits( 3 )
  if( TT == 4 )
  {
    literal_value <- 0
    literal_mask <- bitwShiftL( 1L, 4 )
    
    while( T)
    {
      PP <- get_bits( 5)
      more_groups   <- bitwAnd( PP, literal_mask)
      literal_value <- literal_value * 16 + bitwXor( PP, more_groups)
      if( !more_groups )
        break
    }
    
    out <- if( part1 ) VV else literal_value
    cat( out, " " )
    return( out)
  }
  
  if( part1)
    cat( VV,"(" )
  else
    cat( TT, "(")
  
  II <- get_bits(1)
  operator_args <- NULL
  if( II )
  {
    num_packets <- get_bits( 11)
    for( pn in 1:num_packets)
      operator_args <- c( operator_args, packet_tree())
  }
  else
  {
    length_packets <- get_bits( 15)
    br0 <- cum_bits_requested
    while( cum_bits_requested-br0 < length_packets)
      operator_args <- c( operator_args, packet_tree())
  }
  
   
#  cat(  "=",paste0( operator_args, collapse=", "), ")")
  cat( ")")
  if( part1 )
    return( VV + sum(operator_args))

  funcs <- list( sum, prod, min, max, NULL, `>=`, `<=`, `==` )
  func <- funcs[[ TT+1 ]]

  return( as.numeric( do.call( func, as.list( operator_args ))))
}

total_vv <- 0

# while( loc < str_length( chunk))
# {
#   
#   total_vv <- total_vv +  packet_tree()
# }

part1<-F
total <- packet_tree()
sprintf( "%.100g",total)

# loc
# #packet_tree()
# 
# chunk <- "8A004A801A8002F478"
# print_binary( get_bits(3),3)
# print_binary( get_bits(3),3)
# 
# chunk <-"C200B40A82" 
# 
# buffer <- buffer_sz <- 0
# loc <- 1
# cum_bits_requested <- 0
# 
# get_bits( 5)


# guess 63107991125
# guess 258889206734
# guess 85872105804846
max(buff_sz_list)
