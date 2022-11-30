library( tidyverse )

dir <- "~/Desktop/Advent-Code-2021/Dec 11"
ff  <- "input"
input_raw <- readLines( file.path( dir, ff))

dd  <- length(input_raw) # dimension
dd1 <- dd+2 # dim with border

energy <- matrix(strtoi(str_split( input_raw, "", simplify = T)), nrow=dd, ncol=dd)
# add a border to make edge math easier
energy <- rbind( rep( 0, dd1 ), cbind( rep(0,dd), energy, rep(0,dd)), rep(0, dd1))

num_flashed <- NULL
for( tt in 1:1000)
{
  # reset record of who is flased
  flashed <- matrix( T,dd1,dd1)
  flashed[2:(dd1-1),2:(dd1-1)] <- F

  energy <- energy + 1 # bump the energy
  high_energy = which( energy>9, arr.ind=T)
  while( nrow( high_energy )> 0 )
  {
    flashed <- flashed | (energy > 9)
    flash_effect <- matrix( 0, nrow=dd1, ncol=dd1 )
    for( rr in 1:nrow( high_energy))
    {
      i <- high_energy[rr,1]
      j <- high_energy[rr,2]
      
      flash_effect[ (i-1):(i+1), (j-1):(j+1)] = flash_effect[ (i-1):(i+1), (j-1):(j+1)] + 1 
    }
    energy <- energy + flash_effect
    energy <- energy * !flashed # zero out flashed entries, they are zero energy until next time step
    high_energy = which( energy>9, arr.ind=T)
  }
  
  num_flashed <- c( num_flashed, sum(flashed) - 4*dd-4 )
  
  if( tail(num_flashed,1) ==100)
    break
}
answer1 = sum(num_flashed[1:100])
answer2 = length( num_flashed)

