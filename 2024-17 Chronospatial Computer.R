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
