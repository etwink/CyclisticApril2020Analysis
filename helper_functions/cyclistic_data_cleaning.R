cleanData <- function() {
  # opening the data spreadsheet
  d <- read.csv("datasets/202004-divvy-tripdata.csv")
  
  # checking datatypes of attributes
  #str(d)
  
  # dropping rows with null values (99 of 84776 rows dropped)
  # since only 0.12% of data has NA values, it should not significantly impact the analysis if they are dropped
  # assuming there was a software and/or data entry issue as the rest of the observation looks normal
  D1 <- drop_na(d)
  
  # transforming started_at and ended_at from strings to date-time
  D2 <- transform(D1, started_at = mdy_hm(started_at), ended_at = mdy_hm(ended_at))
  
  # there are 54 observations that include ride times less than or equal to 0 and have different start and end locations.
  # we are assuming there was a software and/or data entry issue and will be filtered out in the next step 
  # filter(D2, (ended_at - started_at) <= 0, start_station_id != end_station_id)
  
  # dropping rows with a ride time (ended_at - started_at) <= 0 (14 rides with ride time < 0 and 628 rides with ride time = 0)
  # since only 0.76% of the remaining data (642 of 84677) has ride time <= 0, it should not significantly impact the analysis if they are dropped
  # we are assuming there was a software and/or data entry issue with negative ride times and rides with zero time elapsed are not relevant to our analysis
  D3 <- subset(D2, (ended_at - started_at > 0))
  
  # there were no duplicates detected in the data set and each station has a unique id number and name
  
  #creating cleaned csv file
  write.csv(D3, "datasets/202004-divvy-tripdata-cleaned.csv", row.names = FALSE)
  return(D3)
}
