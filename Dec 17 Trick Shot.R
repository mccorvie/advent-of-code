target_x = c( 244,303 )
target_y = c(-91, -54)

# target_x= c(20, 30 )
# target_y= c( -10,-5)

tt_max <- 200
tt <- 1:tt_max

count <- 0
maxy  <- 0
for( vx0 in 1:max(target_x))
  for( vy0 in min(target_y):500)
  {
    xx <- ( 2*vx0 - tt+1 )*tt/2
    if( vx0 < tt_max)
      xx[(vx0+1):tt_max ] <- xx[vx0]
    
    yy <- ( 2*vy0 - tt+1 )*tt/2
    
    if( any(target_x[1]<= xx & xx <= target_x[2] & target_y[1]<= yy & yy <= target_y[2]))
    {
      count <- count+1
      maxy = max( max(yy), maxy)
    }
  }

answer1 <- maxy
answer2 <- count

answer1
answer2
