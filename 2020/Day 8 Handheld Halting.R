library( tidyverse)
library( lubridate)

dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day08"
instruction <- str_match( readLines( file.path( dir, ff)), "^(acc|jmp|nop) (\\+?\\-?\\d+)$" )
argument <- strtoi( instruction[,3])
instruction <- instruction[,2]

evaluate_instruction <- function( instruction, return_type ="accumulator" )
{
  exec_line   <- 1
  accumulator <- 0
  seen <- rep( F, length( instruction))
  while( !seen[exec_line] && exec_line <= length( instruction ))
  {
    #cat( exec_line, ":", instruction[exec_line], argument[exec_line], accumulator,"\n")
    seen[exec_line] <- T
    if( instruction[exec_line]=="jmp")
    {
      exec_line <- exec_line + argument[exec_line]
      next
    }
    if( instruction[exec_line]=="acc")
      accumulator <- accumulator + argument[exec_line]
    exec_line <- exec_line+1
  }
  if( return_type == "exec_line")
    return( exec_line )
  accumulator
}

answer1 <- evaluate_instruction( instruction )
answer1

for( flip  in which( instruction != "acc" ))
{
  instruction_mod <- instruction
  if( instruction_mod[flip] == "jmp")
    instruction_mod[flip] <- "nop"
  else
    instruction_mod[flip] <- "jmp"
  
  if( evaluate_instruction( instruction_mod, return_type = "exec_line") > length( instruction))
    answer2 <- evaluate_instruction( instruction_mod, return_type = "accumulator")
}
answer2
