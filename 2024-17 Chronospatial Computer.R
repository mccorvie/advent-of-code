#install.packages("devtools")
#devtools::install_github("benjaminguinaudeau/adventr")
if( file.exists("cookie.R"))
  source( "cookie.R")

library( tidyverse )
library( adventr)
library( unglue)

options(digits=20)

day <- 17
raw <- read_advent(day = day, year=2024) |> head(-1)
# raw  <- readLines( paste0( "input", day ))

test <- readLines( paste0( "test", day, "" ))



use_test = F
input = if( use_test ) test else raw
data <- input |> str_extract_all("\\d+") |> map( as.numeric)

get_combo <- \(n)
{
  if( n<=3) return(n)
  if( n==4 ) return(A)
  if( n==5 ) return(B)
  if( n==6 ) return(C)
  stop("bad combo operand")
}

adv <- \(operand)
{
  A <<- A %/% (2^(get_combo(operand)))
}

bxl <- \(operand)
{
  B <<- bitwXor(B,operand)
}

bst <- \(operand)
{
  B <<- get_combo(operand) %% 8
}

jnz <- \(operand)
{
  NULL
}

bxc <- \(operand)
{
  B <<- bitwXor(B,C)
}

out <- \(operand)
{
  outval <- get_combo(operand) %%8
  cat( outval, ",")
  total_out <<- c(total_out, outval)
}

bdv <- \(operand)
{
  B <<- A %/% (2^(get_combo(operand)))
}

cdv <- \(operand)
{
  C <<- A %/% (2^(get_combo(operand)))
}

instr_fun <- c( adv,bxl,bst,jnz,bxc,out,bdv,cdv)
A <-data[[1]]
B <- data[[2]]
C <- data[[3]]
instr <- data[[5]]
C <-0
B <-0
A <-265061364597659

pos <- 0
total_out <-c()
while( pos < length(instr))
{
  pos
  opcode  <- instr[pos+1]
  opfun   <- instr_fun[[opcode+1]]
  operand <- instr[pos+2]
  opfun
  opfun(operand)
  
  if( opcode == 3 && A != 0 )
  {
    pos = operand
    next
  }
  pos <- pos + 2
}

cat( "A=",A, " B=",B, " C=",C, "\n")
paste0(total_out,collapse=",")
paste0(instr,collapse=",")



# 2 4   bst A    B <- A %% 8
# 1 7   bxl 7    B <- B xor 7
# 7 5   cdv B    C <- A / 2^B
# 1 7   bxl 7    B <- B xor 7  
# 4 6   bxc      B <- B xor C
# 0 3   adv 3    A <- A / 8
# 5 5   out B    out B %% 8
# 3 0   jnz      if(A!=0) loop
#A <- 0
#itwShiftL(A,)
#A <- rep(F,10)

#A_low xor A << (7-A_low) = out


#all(c( F,F,F) == c(NA,F,F),na.rm=T)

bincode <- \(x) c( x%%2, (x%/%2)%%2,(x%/%4)%%2 )


A = rep(0,10)

#cdv2   <- \(A,n) c(A,rep(F,n+3))[(n+1):(n+3)]
cdv2   <- \(A,n) A[(n+1):(n+3)]
shiftl <- \(A,n) c( rep(NA,3),A )


tt <- bincode(0)
map2( Bs,Cs, \(v1,v2) binsolve( v1,v2,tt))

# fill in NA's in v2 to solve v1 xor v2 = tt, or return NA if impossible
binsolve <- \(v1,v2,tt)
{
  xor1 <- v1 & !v2 | !v1 & v2
  xor2 <- v1 & !tt | !v1 & tt
  if( any( xor1 != tt, na.rm=T )) return( NA )
  return( xor2 )
}

binsolvable <- \(n,A,tt)
{
  v1 <- bincode( n )
  v2 <- cdv2( c(v1, A), 7-n)
  xor <- (v1+v2)%%2
  if( any(is.na(xor))) 
    stop( paste0( c(n, " ", A, " ", t), collapse = ""))
  all( xor == bincode( tt ))
}


match_output <- \(output, A, first=F)
{
  # output <- c(3,0)
#   A <- c( rep(1,3),rep(0,10))
#  cat( length(output), " ")
  if( length(output)==0)
    return(A)
  
  tt     <- last(output)
  output <- head(output,-1)

  Bs <- keep( 0:7, \(n) binsolvable( n, A, tt))
  for( B in Bs )
  {
    if( B==0 && all(A==0)) next
    Aout <- match_output( output, c( bincode(B), A))
    if( !is.null(Aout)) return( Aout )
  }
  NULL
}

A <- rep( 0,10)

real_output <- c(2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0)

xx <-match_output( real_output, A,T)
xx
sum(2^(0:(length(xx)-1))*xx)
