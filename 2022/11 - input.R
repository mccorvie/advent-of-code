

testmonkeys = list(
  list(
    #Monkey 0:
    items  = c(79, 98),
    worryop  = \(x) x * 19,
    divtest = 23,
    throwT =  2,
    throwF =  3
  ),
  list(
    #Monkey 1:
    items  = c(54, 65, 75, 74),
    worryop  = \(x) x + 6,
    divtest = 19,
    throwT =  2,
    throwF =  0
  ),
  list(
    
    #Monkey 2:
    items  = c(79, 60, 97),
    worryop  = \(x) x * x,
    divtest = 13,
    throwT =  1,
    throwF =  3
  ),
  list(
    
    #Monkey 3:
    items  = 74,
    worryop  = \(x) x + 3,
    divtest = 17,
    throwT =  0,
    throwF =  1
    
  )
)

realmonkeys= list(
  ## Monkey= 0:
  list(
    items = c( 66, 59, 64, 51 ),
    worryop = \(x)  x * 3,
    divtest =  2,
    throwT =  1,
    throwF =  4
  )  ,
  list(
    # Monkey 1:
    items = c( 67, 61),
    worryop = \(x)  x * 19,
    divtest =  7,
    throwT =  3,
    throwF =  5
  )  ,
  list(
    
    # Monkey 2:
    items = c( 86, 93, 80, 70, 71, 81, 56),
    worryop = \(x)  x + 2,
    divtest =  11,
    throwT =  4,
    throwF =  0
  )  ,
  list(
    
    # Monkey 3:
    items = c( 94),
    worryop = \(x)  x * x,
    divtest =  19,
    throwT =  7,
    throwF =  6
  )  ,
  list(
    
    # Monkey 4:
    items = c( 71, 92, 64),
    worryop = \(x)  x + 8,
    divtest =  3,
    throwT =  5,
    throwF =  1
  )  ,
  list(
    
    # Monkey 5:
    items = c( 58, 81, 92, 75, 56),
    worryop = \(x)  x + 6,
    divtest =  5,
    throwT =  3,
    throwF =  6
  )  ,
  list(
    
    # Monkey 6:
    items = c( 82, 98, 77, 94, 86, 81 ),
    worryop = \(x)  x + 7,
    divtest =  17,
    throwT =  7,
    throwF =  2
  ),
  list(
    
    # Monkey 7:
    items = c( 54, 95, 70, 93, 88, 93, 63, 50),
    worryop = \(x)  x + 4,
    divtest =  13,
    throwT =  2,
    throwF =  0
  )
)


