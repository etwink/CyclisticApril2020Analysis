rideStartTimes <- function(d = data.frame()) {
  source("helper_functions/normalizeVector.R")
  library(hms)
  
  D1 <- select(d, ride_id, member_casual, started_at)
  D2 <- mutate(D1, time_started_at = as_hms(format(as.POSIXct(D1$started_at), format = "%H:%M:%S")))
  D3 <- D2 %>% 
    group_by(member_casual, time_started_at) %>% 
    summarize(time_started_at_count = n())
  
  D4_member <- filter(D3, member_casual == "member")
  D4_casual <- filter(D3, member_casual == "casual")
  D4_member <- mutate(D4_member, normalized_time_started_at_count = normalize_vector(D4_member$time_started_at_count))
  D4_casual <- mutate(D4_casual, normalized_time_started_at_count = normalize_vector(D4_casual$time_started_at_count))
  
  D4 <- merge(D4_member, D4_casual, all=TRUE)
  
  library(lubridate)
  
  return(list(D3, D4))
}


