#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 24
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "b" ))

use_test = F
input = if( use_test ) test else raw


vv <- list()
inputgap <- which( input == "" )
input_bits <- input[1:(inputgap-1)] |> str_match( "([a-z0-9]{3}): (\\d)")
map_chr( 1:nrow(input_bits), \(n) paste0( 'vv[["',input_bits[n,2], '"]]<<-', input_bits[n,3]==1))  |> 
  walk( \(rcode ) eval( parse( text = rcode )))

node_rename <- c( 
  "dmw" = "c02",
  "rmb" = "c03",
  "fcw" = "c04",
  "wbn" = "c05",
  "nbm" = "c06",
  "btq" = "c07",
  "nqk" = "c08",
  "msm" = "c09",                         
  "mht" = "c10",                        
  "krt" = "c11",                         
  "npp" = "c12",                         
  "fgm" = "c13",                         
  "mjn" = "c14",                        
  "rqq" = "c15",                        
  "mmq" = "c16",                        
  "nmq" = "c17",                        
  "fcg" = "c18",                        
  "rkn" = "c19",                        
  "wgs" = "c20",                        
  "dht" = "c21",                        
  "jsb" = "c22",                        
  "rsf" = "c23",   
  "nmp" = "c24",
  "tgs" = "c25",                        
  "bhs" = "c26",                        
  "pst" = "c27",                        
  "hrf" = "c28",                        
  "ttp" = "c29",                        
  "vss" = "c30",                        
  "hdn" = "c31",                        
  "fth" = "c32",                        
  "pcs" = "c33",                        
  "sfv" = "c34",                        
  "tgg" = "c35",                        
  "tmt" = "c36",                        
  "rqt" = "c37",                        
  "crc" = "c38",                        
  "wnb" = "c39",                        
  "hst" = "c40",                       
  "php" = "c41",                        
  "jbn" = "c42",                        
  "pvd" = "c43",                        
  "jhn" = "c44"
  #"fsh" = "c45"
)

swap_instr <- \(s1,s2)
{
  s1_cur <- instructions[[s1]]
  s2_cur <- instructions[[s2]]
  s1_cur[4] <- s2
  s2_cur[4] <- s1
  instructions[[s1]] <<- s2_cur
  instructions[[s2]] <<- s1_cur
}


device <- input[(inputgap+1):length(input)] |> 
  str_replace_all( node_rename ) |> 
  str_match( "([a-z0-9]{3}) ([A-Z]+) ([a-z0-9]{3}) -> ([a-z0-9]{3})")
instructions <- list()
1:nrow(device) |> walk( \(n) instructions[[device[n,5]]] <<- device[n,2:5])


and <- \(x,y) x&&y
or  <- \(x,y) x||y
operate_device <- \(var)
{
  instr <- instructions[[var]]
  if( is.null( vv[[instr[1]]])) operate_device( instr[1])
  if( is.null( vv[[instr[3]]])) operate_device( instr[3])
  rcode <- paste0( str_to_lower( instr[2]),'( vv[["', instr[1], '"]], vv[["', instr[3], '"]])')
  vv[[var]] <<- eval( parse( text = rcode ))
}

output_bits <- names( instructions) |> str_extract( "^z\\d+") |> sort() |> map_lgl( operate_device )
sum(2^(0:(length(output_bits)-1)) * output_bits )

to_bits <- \(n, varname )
{
  for( bit in 0:44)
  {
    vv[[ sprintf( "%s%02d", varname,bit)]] <<- n%% 2 == 1
    n <- n %/% 2
  }
}


# carry_1 = and(x_0,y_0)
# carry_n = or( and( x_{n-1}, y_{n-1} ),
#               and( xor( x_{n-1}, y_{n-1} ), carry_{n-1}))
# 
# z_n = xor( xor( x_n,y_n), carry_n)

operate_device2 <- \(x,y)
{
  vv <<- list()
  to_bits( x, "x")
  to_bits( y, "y")
  output_bits <- names( instructions) |> str_extract( "^z\\d+") |> sort() |> map_lgl( operate_device )
  sum(2^(0:(length(output_bits)-1)) * output_bits )
}

mul = 2^6
mul
operate_device2( mul,0) == mul
operate_device2( 0,mul) == mul
operate_device2( mul,mul) == 2*mul
operate_device2( mul,mul) 


device_expression <- \(var, depth )
{
  if( str_detect( var,"[xy]\\d+")) return( var )
  instr <- instructions[[var]]
  if( is.null(instr)) return( "[NA]")
  if( instr[2] == "OR") depth <- depth-1
  if( depth == 0) return( var )
  left  <- device_expression( instr[1], depth)
  right <- device_expression( instr[3], depth)
  if( str_length(left) > str_length(right) || 
      str_length(left) == str_length(right) && left > right)
  {
    tmp  <- left
    left  <- right
    right <- tmp
  }
  return( paste0( str_to_lower( instr[2]), "( ",left, ", ", right, " )"))
}


swap_instr( "z07", "c08" )
swap_instr( "z24", "fpq" )
swap_instr( "z32", "srn" )
swap_instr( "pcp", "fgt" )

map_chr( 0:45, \(n) device_expression( sprintf("z%02d",n),1))
map_chr( 0:45, \(n) device_expression( sprintf("c%02d",n),2))

c("z07","nqk","z24","fpq","z32","srn","pcp","fgt") |> sort() |> paste0(collapse=",")
