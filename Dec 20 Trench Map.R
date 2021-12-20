dir <- "~/Desktop/Advent-Code-2021/Dec 20"
ff  <- "input_test"
ff  <- "input"
lines <- readLines( file.path( dir, ff))

key <- str_split(lines[1], "", simplify = T)[1,]=="#"
image <- str_split(lines[c(-1,-2)], "", simplify = T) == "#"

pad <- 75
padc <- matrix( F, nrow = nrow(image), ncol = pad )
padr <- matrix( F, nrow = pad, ncol = ncol(image)+pad*2 )
new_image <- image <- rbind( padr, cbind( padc, image, padc ), padr)
place_value <- 2^(8:0)
infinity <- F
for( i in 1:50)
{
  for( rr in 2:(nrow(image)-1))
    for( cc in 2:(ncol(image)-1))
    {
      bits <- as.vector( t(image[(rr-1):(rr+1),(cc-1):(cc+1)]))
      new_image[ rr, cc] <- key[sum( bits * place_value )+1]    
    }
  # the borders get the infinity pattern
  infinity <- key[ sum(place_value * infinity)+1]
  new_image[1,] <- new_image[nrow(new_image),]<-infinity
  new_image[,1] <- new_image[,ncol(new_image)]<-infinity

  image <- new_image
  
  ww1 <- which(apply(image, 1, any))
  ww2 <- which(apply(image, 2, any))
  
  cat( i, "size of image: [", min(ww1), max(ww1), "]x[", min(ww2), max(ww2), "]\n")
}

sum( image)
