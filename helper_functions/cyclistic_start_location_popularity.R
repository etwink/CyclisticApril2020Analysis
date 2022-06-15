startLocationPopularity <- function(d = data.frame()) {
  # function to normalize a vector
  source("helper_functions/normalizeVector.R")
  
  #!!! CREATING BAR CHART SHOWING THE FAVORITE START LOCATION OF EACH MEMBERSHIP GROUP !!!#
  D11 <- d %>% 
    group_by(start_station_name, member_casual) %>% 
    summarize(start_station_count = n())
  
  # getting top 10 member favorite start locations
  D11_member <- data.frame(D11) %>% 
    filter(member_casual == "member") %>%
    slice_max(start_station_count, n = 10) %>% 
    arrange(-start_station_count)
  
  # getting top 10 casual favorite start locations
  D11_casual <- data.frame(D11) %>% 
    filter(member_casual == "casual") %>%
    slice_max(start_station_count, n = 10) %>% 
    arrange(-start_station_count)
  
  # merging the favorite start locations together
  D12 <- merge(D11_member, D11_casual, all=TRUE)
  
  # left join on the favorite start location with the original data frame to get all the destinations for both membership types
  # cleaning the new data frame of any duplicates and sorting it in descending order
  D13 <- merge(D12, D11, by = 'start_station_name', all.x=TRUE)
  D14 <- distinct(select(D13, start_station_name, member_casual = member_casual.y, start_station_count = start_station_count.y))
  D14 <- arrange(D14, -start_station_count)
  
  #!!! CREATING NORMALIZED BAR CHART SHOWING THE FAVORITE DESTINATIONS OF EACH MEMBERSHIP GROUP !!!#
  D14_member <- filter(D14, member_casual == "member")
  D14_casual <- filter(D14, member_casual == "casual")
  D14_member <- mutate(D14_member, normalized_start_station_count = normalize_vector(D14_member$start_station_count)) 
  D14_casual <- mutate(D14_casual, normalized_start_station_count = normalize_vector(D14_casual$start_station_count)) 
  
  D15 <- merge(D14_member, D14_casual, all=TRUE)
  
  return(list(D14, D15))
}
