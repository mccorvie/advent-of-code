dir <- "~/Desktop/Advent-Code-2021/Dec 24"
ff <- "input"
inst_regex <- regex( "(inp|add|mul|div|mod|eql) (w|x|y|z) *(-?\\d+|w|x|y|z)*")
instructions <- str_match( readLines( file.path( dir, ff)), inst_regex )[,2:4]

inputs <- 1:20%%10

funclist <- list( "add" = "%s<-sum(%s,%s)", "mul" = "%s<-prod(%s,%s)", "div" = "%s<-floor(%s/%s)", "mod" = "%s<-%s %%%% %s", "eql"= "%s<-as.numeric(%s==%s)" )

alu <- function( input, instructions, z )
{
  w<-x<-y<-0
  for( inst_num in 1:nrow(instructions ))
  {
    instruction <- instructions[inst_num,]
    #cat( paste( instruction), " ")
    
    if( instruction[1]=="inp")
    {
      eval( parse( text = sprintf( " %s<-input[1]", instruction[2])))
      input <- input[-1]
      next
    }
    
    rinst <- sprintf( funclist[[ instruction[1]]], instruction[2], instruction[2], instruction[3])
    eval( parse( text=rinst ))
  }
  return( list( w=w, x=x, y=y, z=z))  
}



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
big   <- c( 1,2,9,9,6,9,9,7,8,2,9,3,9,9)
alu(big, instructions, 0)
small <- c( 1,1,8,4,1,2,3,1,1,1,7,1,8,9)
alu(small, instructions, 0)
