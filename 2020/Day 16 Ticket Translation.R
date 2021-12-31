library( tidyverse)
dir <- "~/Desktop/Advent-Code-2021/2020_inputs"
ff  <- "day16-test"
ticket_info <- readLines( file.path( dir, ff))

ticket_info <- split( ticket_info, cumsum(ticket_info==""))
valid <- list()
field_spec <- str_match( ticket_info[[1]], regex( "^([a-z ]+)\\: (\\d+)\\-(\\d+) or (\\d+)\\-(\\d+)"))
fields <- field_spec[,2]
field_vals <- matrix( strtoi( field_spec[,3:6]), ncol=4)
dd <- length(fields)

all_vals <- NULL
for( spec_idx in 1:length( fields) )
{
  valid[[ fields[spec_idx]]] = c( field_vals[spec_idx,1]:field_vals[spec_idx,2], field_vals[spec_idx,3]:field_vals[spec_idx,4])
  all_vals <- union( all_vals,valid[[ fields[spec_idx]]] )
}
nearby_tickets <- ticket_info[[3]]
nearby_tickets <- nearby_tickets[3:length( nearby_tickets)]
nearby_tickets <- matrix(strtoi(str_split( nearby_tickets, ",", simplify = T)), ncol=dd)
error  <- keep( nearby_tickets, ~ !(. %in% all_vals))
answer1<-sum(error)
answer1

valid_tickets <- matrix(map_lgl( nearby_tickets, ~ . %in% all_vals), ncol=dd)
nearby_tickets <- nearby_tickets[apply(valid_tickets, 1, all),]

row_valid <- matrix( NA, ncol = dd, nrow=dd)
for( rr in 1:dd)
  for( cc in 1:dd)
    row_valid[rr,cc] = all( nearby_tickets[,cc] %in% valid[[names(valid)[rr]]])

possible <- apply(row_valid,2, which)

while(length(unlist(possible))>dd)
{
  identified <- unlist(keep( possible, ~ length(.)==1))
  possible <- map( possible, ~ if( length(.)==1) . else setdiff(., identified))
  cat( length(identified), length(unlist(possible)),"\n")
}
possible
my_ticket <- strtoi(str_split(ticket_info[2][[1]][3], ",")[[1]])
my_ticket[unlist(possible[1:3])]
