dir <- "~/Desktop/Advent-Code-2021/Dec 24"
ff <- "input"
inst_regex <- regex( "(inp|add|mul|div|mod|eql) (w|x|y|z) *(-?\\d+|w|x|y|z)*")
instructions <- str_match( readLines( file.path( dir, ff)), inst_regex )[,2:4]

inst_groups <-split( 1:nrow( instructions ), cumsum(instructions[,1] == "inp"))

param_loc = matrix( c( 5,6,16,3,3,3), ncol=2)
params <- list()
for( idx in 1:14 )
{
  ll <- strtoi(instructions[inst_groups[[idx]],][param_loc])
  names( ll) <- c( "a","b","c")
  params[[idx]] <- ll
}

# 
# x<-(z%%26 + param$b)!=w
# z<-z/param$a
# z<-z*(25*x+1)+(w+param$c)*x
# z<-z+x*(25*z + w+param$c)
#
# 
# zz<-0
# 
# tt<- zz[1] !=w-b
# if( a == 26)
#   pop(zz)
# if(tt)
#   push(zz, w+c)  

zz<-NULL
for( idx in 1:14)
{
  if( params[[idx]]["b"] < 10 )
  {
    cat( paste0( zz[1], "= w",idx, "+",-params[[idx]]["b"], collapse = "" ),"\n")
  }
  if( params[[idx]]["a"]==26)
    zz <- zz[-1]
  
  if( params[[idx]]["b"] >= 10)
    zz<-c( paste0( "w", idx, "+", params[[idx]]["c"], collapse=""), zz)      
}

# w4= w5+3
# w7= w8+2
# w6= w9+1
# w3= w10+7
# w11= w12+6
# w2+7= w13 
# w1+8= w14 
# 

# biggest
# w1=1
# w2=2
# w3=9
# w4=9
# w5=6
# w6=9
# w7=9
# w8=7
# w9=8
# w10=2
# w11=9
# w12-3
# w13=9
# w14=9
#
# smallest
# w1=1
# w2=1
# w3=8
# w4=4
# w5=1
# w6=2
# w7=3
# w8=1
# w9=1
# w10=1
# w11=7
# w12=1
# w13=8
# w14=9
# 
