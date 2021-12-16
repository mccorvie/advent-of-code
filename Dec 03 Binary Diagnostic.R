
library( tidyverse )
dir <- "~/Desktop/Advent-Code-2021/Dec 03"
ff <- "input"

input_file <- read_csv( file.path( dir, ff), col_names= "bits", col_types= "c")%>%
  mutate( idx = 1:1000 )

bits <- input_file$bits
bit_length = str_length( bits[1])

gamma_bits   <- NULL
epsilon_bits <- NULL

for( bit_pos in 1:bit_length)
{
  bits_extract <- str_sub( bits, bit_pos, bit_pos )

  if(  sum(bits_extract == "1") > 500 )
  {
    gamma_bit   = "1"
    epsilon_bit = "0"
  } 
  else if(  sum(bits_extract == "1") < 500 )
  {
    gamma_bit   = "0"
    epsilon_bit = "1"
  } 
  else
  {
    stop( "There's a tie!")
  }
  
  gamma_bits = c( gamma_bits, gamma_bit )
  epsilon_bits = c( epsilon_bits, epsilon_bit )
}

gamma_rate = strtoi(paste( gamma_bits, collapse= ""), base=2)
epsilon_rate = strtoi(paste( epsilon_bits, collapse= ""), base=2)

gamma_rate * epsilon_rate


# oxy_rating 

bits <- input_file$bits

bit_length = str_length( bits[1])

oxy_bits <- bits
bit_pos = 1
while( length( oxy_bits ) > 1)
{
  bits_extract <- str_sub( oxy_bits, bit_pos, bit_pos )
  if( sum( bits_extract == "1" ) >= length(oxy_bits)/2 )
    target_bit <- "1"
  else
    target_bit <- "0"
  
  cat( "pos", bit_pos, ":", sum( bits_extract == "1" ), "ones, half is", length(oxy_bits)/2, "\n")

  oxy_bits = oxy_bits[ bits_extract == target_bit ]
  cat(  " --- selected ", target_bit, "giving", length(oxy_bits),"strings left\n" )
  
  
  bit_pos <- bit_pos+1  
}


co2_bits <- bits
bit_pos = 1
while( length( co2_bits ) > 1)
{
  bits_extract <- str_sub( co2_bits, bit_pos, bit_pos )
  if( sum( bits_extract == "1" ) < length(co2_bits)/2 )
    target_bit <- "1"
  else
    target_bit <- "0"
  
  cat( "pos", bit_pos, ":", sum( bits_extract == "1" ), "ones, half is", length(co2_bits)/2, "\n")

  co2_bits = co2_bits[ bits_extract == target_bit ]
  cat(  " --- selected ", target_bit, "giving", length(co2_bits),"strings left\n" )
  
  bit_pos <- bit_pos+1  
}

oxy_bits
co2_bits

oxy_rate = strtoi(paste( oxy_bits, collapse= ""), base=2)
co2_rate = strtoi(paste( co2_bits, collapse= ""), base=2)
oxy_rate*co2_rate
