endDestinationPopularity <- function(d = data.frame()) {
  # function to normalize a vector
  source("helper_functions/normalizeVector.R")
  
  #!!! CREATING BAR CHART SHOWING THE FAVORITE DESTINATIONS OF EACH MEMBERSHIP GROUP !!!#
  
  # popular end destinations of members and casuals
  D1 <- d %>% 
    group_by(end_station_name, member_casual) %>% 
    summarize(end_station_count = n())
  
  # getting top 10 member favorite destinations
  D1_member <- data.frame(D1) %>% 
    filter(member_casual == "member") %>%
    slice_max(end_station_count, n = 10) %>% 
    arrange(-end_station_count)
  
  # getting top 10 casual favorite destinations
  D1_casual <- data.frame(D1) %>% 
    filter(member_casual == "casual") %>%
    slice_max(end_station_count, n = 10) %>% 
    arrange(-end_station_count)
  
  # merging the favorite destinations together
  D2 <- merge(D1_member, D1_casual, all=TRUE)
  
  # left join on the favorite destinations with the original data frame to get all the destinations for both membership types
  # cleaning the new data frame of any duplicates and sorting it in descending order
  D3 <- merge(D2, D1, by = 'end_station_name', all.x=TRUE)
  D4 <- distinct(select(D3, end_station_name, member_casual = member_casual.y, end_station_count = end_station_count.y))
  D4 <- arrange(D4, -end_station_count)
  
  #!!! CREATING NORMALIZED BAR CHART SHOWING THE FAVORITE DESTINATIONS OF EACH MEMBERSHIP GROUP !!!#
  D4_member <- filter(D4, member_casual == "member")
  D4_casual <- filter(D4, member_casual == "casual")
  D4_member <- mutate(D4_member, normalized_end_station_count = normalize_vector(D4_member$end_station_count)) 
  D4_casual <- mutate(D4_casual, normalized_end_station_count = normalize_vector(D4_casual$end_station_count)) 
  
  D5 <- merge(D4_member, D4_casual, all=TRUE)
  
  return(list(D4, D5))
}

