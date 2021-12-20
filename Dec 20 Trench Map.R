dir <- "~/Desktop/Advent-Code-2021/Dec 20"
ff  <- "input_test"
ff  <- "input"
lines <- readLines( file.path( dir, ff))

key <- str_split(lines[1], "", simplify = T)[1,]=="#"
image <- str_split(lines[c(-1,-2)], "", simplify = T) == "#"


pad <- 75
padc <- matrix( 0, nrow = nrow(image), ncol = pad )
padr <- matrix( 0, nrow = pad, ncol = ncol(image)+pad*2 )
image <- rbind( padr, cbind( padc, image, padc ), padr)
place_value <- 2^(8:0)

for( i in 1:50)
{
  cat( i,"\n")
  for( rr in 2:(nrow(image)-1))
    for( cc in 2:(ncol(image)-1))
    {
      bits <- as.vector( t(image[(rr-1):(rr+1),(cc-1):(cc+1)]))
      lookup <- sum( bits * place_value )+1
      new_image[ rr, cc] <- key[lookup]    
    }
  blink <- !new_image[1,1]
  new_image[1,] <- new_image[nrow(new_image),]<-blink
  new_image[,1] <- new_image[,ncol(new_image)]<-blink

  image <- new_image
  
  ww1 <- which(apply(image, 1, any))
  ww2 <- which(apply(image, 2, any))
  
  cat( "[", min(ww1), max(ww1), "]x[", min(ww2), max(ww2), "]\n")
}

sum( image)
