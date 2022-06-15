getRideLength <- function(d = data.frame()) {
  # function to normalize a vector
  source("helper_functions/normalizeVector.R")
  
  #!!! CREATING LINE GRAPH OF NUM OF RIDES VS RIDE LENGTH !!!#
  
  # creating a ride_time column
  D1 <- mutate(d, ride_time = as.numeric(ended_at - started_at))
  
  # The majority of rides (98.6%) are less than 2 hours so we will focus on those. 
  # percent_of_rides_under_two_hours <- nrow(filter(D1, ride_time <= 120))/nrow(D1)*100
  D2 <- filter(D1, ride_time <= 120)
  
  # average casual ride time under 2 hours is 31 minutes and average member ride time under 2 hours is 17.2 minutes
  avg_times <- D2 %>% 
    group_by(member_casual) %>% 
    summarize(avg_ride_time = mean(ride_time))
  
  p1 <- D2 %>% 
    group_by(member_casual, ride_time) %>% 
    summarize(number_of_rides_at_minute_n = n())
  
  #!!! CREATING NORMALIZED LINE GRAPH OF NUM OF RIDES VS RIDE LENGTH !!!#
  
  # normalizing the counts of riding time by membership to more easily understand the different ride length patterns
  D3 <- D2 %>% 
    group_by(member_casual, ride_time) %>% 
    summarize(num_of_rides_at_minute_n = n())
  
  D3_member <- filter(D3, member_casual == "member")
  D3_casual <- filter(D3, member_casual == "casual")
  
  # attaching normalized vectors to data frames
  D3_member <- mutate(D3_member, norm_scale = normalize_vector(D3_member$num_of_rides_at_minute_n))
  D3_casual <- mutate(D3_casual, norm_scale = normalize_vector(D3_casual$num_of_rides_at_minute_n))
  
  # outer join the two tables so that they can be graphed together
  p2 <- merge(D3_member, D3_casual, all=TRUE)
  
  return(list(p1,p2))
}