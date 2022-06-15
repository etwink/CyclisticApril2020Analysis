rideableDistribution <- function(d = data.frame()) {
  #!!! CREATING PIE CHART SHOWING THE USAGE OF DIFFERENT RIDEABLES BY MEMBERSHIP GROUP !!!#
  
  # creating a table that represents the percentage of ridership based on rideable
  # designed to be reusable (current dataset only has 2 rideable types used)
  D1 <- d %>% 
    group_by(member_casual, rideable_type) %>% 
    summarize(number_of_rides = n())
  
  total_num_of_riders <- sum(D1$number_of_rides)
  
  D1 <- mutate(D1, percent_of_rides = round(100*number_of_rides/total_num_of_riders, 2))
  D1 <- unite(D1, "membership_plus_biketype", member_casual, rideable_type, sep = " + ")
  
  
  return(D1)
}

