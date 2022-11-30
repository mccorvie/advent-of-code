dir <- "~/Desktop/Advent-Code-2021/Dec 19"
ff  <- "input_test"
ff  <- "input"
lines <- readLines( file.path( dir, ff))

raw <- str_split( read_file(file.path(dir,ff)), "---", simplify=T)
raw <- raw[seq(from= 3, to=length( raw), by=2)]

charts   <- map( raw, ~ read_csv( ., skip=1, col_names=c( "x","y","z")))
nscanner <- length(charts)
for( i in 1:nscanner)
  charts[[i]] <- charts[[i]] %>% mutate( scanner = i, beacon = letters[1:nrow(charts[[i]])])
charts <- bind_rows( charts)

scanner_chart <- tibble( x=0,y=0,z=0, scanner=1 )
dd_memo <- as.list( rep( NA, nscanner))

distance_matrix <- function( scanner )
{
  if( is.matrix( dd_memo[[scanner]]))
    return( dd_memo[[scanner]])
  scan_chart <- filter( charts, scanner==!!scanner)
  dd <-expand_grid( pt1 = scan_chart, pt2 = scan_chart) %>%
    mutate( beacon1=pt1$beacon, beacon2 = pt2$beacon, dist = (pt1$x-pt2$x)^2+(pt1$y-pt2$y)^2+(pt1$z-pt2$z)^2) %>%
    select( beacon1, beacon2, dist)%>%
    pivot_wider( names_from=beacon2, values_from = dist) %>%
    arrange( beacon1) %>%
    select( -beacon1) %>%
    as.matrix()
  
  rownames(dd)<- colnames(dd)
  dd_memo[[scanner]] <<- dd
  dd
}


similarity_matrix <- function(scanner1, scanner2)
{
  chart1 <- charts %>% filter( scanner==scanner1)
  chart2 <- charts %>% filter( scanner==scanner2)
  ss <- matrix(NA,nrow=nrow(chart1), ncol= nrow(chart2))
  rownames(ss) <- chart1$beacon
  colnames(ss) <- chart2$beacon
  dd1 <-  distance_matrix( scanner1)
  dd2 <-  distance_matrix( scanner2)
  for( b1 in chart1$beacon)
    for( b2 in chart2$beacon )
      ss[b1, b2] <- length( intersect( dd1[b1,], dd2[b2,]))
  ss
}

overlap_matrix <- function()
{
  overlap <- matrix( NA, nrow=nscanner, ncol=nscanner)
  for( scanner1 in 1:(nscanner-1))
    for( scanner2 in (scanner1+1):nscanner)
    { 
      cat( scanner1, scanner2, "\n")
      overlap[ scanner1, scanner2] <- overlap[ scanner2, scanner1] <- sum(similarity_matrix( scanner1, scanner2 ) >= 12 )
    }
  overlap
}

align_regions <- function( scanner_base, scanner_fit )
{
  similarity_map <- map( keep(apply( similarity_matrix( scanner_base,scanner_fit) >=12, 1, which ), ~ length(.) >0 ), names)
  chart_fit <- filter( charts, scanner == scanner_fit)
  chart_mash <- charts %>% 
    filter( scanner==scanner_base, beacon %in% names( similarity_map)) %>% 
    mutate( beacon_map = recode( beacon, !!!similarity_map))%>%
    rename( x0=x, y0=y, z0=z) %>%
    left_join( chart_fit, by = c( beacon_map = "beacon"))
  
  lm.x <- lm( x0 ~ x + y + z, chart_mash )
  lm.y <- lm( y0 ~ x + y + z, chart_mash )
  lm.z <- lm( z0 ~ x + y + z, chart_mash )
  
  chart_fit <- chart_fit %>%
    mutate(
      x=round(predict( lm.x, chart_fit )),
      y=round(predict( lm.y, chart_fit )),
      z=round(predict( lm.z, chart_fit )),
    )
  scanner_chart <<- scanner_chart %>%
    bind_rows( tibble(x= round(coef(lm.x)[1]),y= round(coef(lm.y)[1]),z= round(coef(lm.z)[1]), scanner=scanner_fit))
  
  charts <<- bind_rows( filter( charts, scanner != scanner_fit), chart_fit)
}

# do a breadth first traversal of the overlap graph and align the charts
mm <- overlap_matrix()
aligned <- next_base <- c(1)

while( length( next_base))
{
  base <- first( next_base)
  has_overlap <- setdiff(which( mm[ base,] >=12), aligned)
  next_base <- c(next_base[-1], has_overlap)
  for( to_fit in has_overlap)
  {
    cat( "aligning", base, to_fit, "\n")
    align_regions( base, to_fit )
  }
  aligned <- c( aligned, has_overlap)
}

universal_chart <-charts %>% select( x,y,z) %>% distinct()
answer1 <- universal_chart%>% count()
gg<-expand_grid( pt1=as.matrix(scanner_chart), pt2=as.matrix(scanner_chart))
answer2 <- max(abs(gg$pt1[,"x"]-gg$pt2[,"x"])+abs(gg$pt1[,"y"]-gg$pt2[,"y"])+abs(gg$pt1[,"z"]-gg$pt2[,"z"]))
