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

# Try to recursively solve for inputs through each instruction group
# Unfortunately the search range is too small and it doesn't work

memo <- list()
solve_alu <- function( group_idx, target )
{
  if( group_idx ==0 )
    return( list())
  cat( paste0(group_idx, "/", target ), " ")
  
  if( !is.null( memo[[paste0(group_idx, "/", target)]]))
    return( memo[[paste0(group_idx, "/", target)]])
  
  if( group_idx == 1 )
    zlist <- 0
  else
    zlist <- 0:(26*target)
  
  out = list()
  for( z in zlist)
    for( in_digit in 1:9)
        if( target == alu( in_digit, instructions[inst_groups[[group_idx]],], z)$z )
          out <- c( out, lapply( solve_alu( group_idx-1, z), \(x) c(x, in_digit)))

  memo[[paste0(group_idx, "/", target)]] <<- out
  out
}

# inst_groups <-split( 1:nrow( instructions ), cumsum(instructions[,1] == "inp"))
# solve_alu( 14,0)


big   <- c( 1,2,9,9,6,9,9,7,8,2,9,3,9,9) 
alu(big, instructions, 0)

small <- c( 1,1,8,4,1,2,3,1,1,1,7,1,8,9)
alu(small, instructions, 0)

