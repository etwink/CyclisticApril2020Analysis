source("helper_functions/my_install_packages.R")
source("helper_functions/cyclistic_data_cleaning.R")
source("helper_functions/cyclistic_ride_length.R")
source("helper_functions/cyclistic_rideable_distribution.R")
source("helper_functions/cyclistic_start_location_popularity.R")
source("helper_functions/cyclistic_end_destination_popularity.R")
source("helper_functions/cyclistic_start_time_distribution.R")

myInstallPackages()
d <- cleanData()

#!!! Ride Length Analysis !!!#
my_data_sets <- getRideLength(d)

# distribution of rides based on length and type of ridership
d1 <- data.frame(my_data_sets[1])
jpeg("graphs/ride_length_line_graph.jpeg")
ggplot(data=d1, aes(x=ride_time, y=number_of_rides_at_minute_n, color=member_casual))+
  geom_line(size = 1.25) + 
  labs(title = "Number of Rides vs. Ride Length", x="Ride Length (minutes)", y="Number of Rides")
dev.off()

# line graph of normalized count of ride times
d2 <- data.frame(my_data_sets[2])
jpeg("graphs/normalized_ride_length_line_graph.jpeg")
ggplot(data=d2, aes(x=ride_time, y=norm_scale, color=member_casual))+
  geom_line(size = 1.25) + 
  labs(title = "Normalized Number of Rides vs. Ride Length", x="Ride Length (minutes)", y="Normalized Number of Rides")
dev.off()

#!!! Rideable Analysis !!!#
d3 <- rideableDistribution(d)

# pie chart showing percentage of members and casuals using each type of rideable
jpeg("graphs/rideable_distribution.jpeg")
pie(d3$percent_of_rides, labels = d3$percent_of_rides, main = "Percent Usage of Rideable by Membership Type", col = rainbow(length(d3$percent_of_rides)))
legend("topright", d3$membership_plus_biketype, cex=0.8, fill = rainbow(length(d3$percent_of_rides)))
dev.off()

#!!! Start Location Popularity Analysis !!!#
my_data_sets <- startLocationPopularity(d)

# bar chart of the comparing which end destinations are popular among each membership group
d4 <- data.frame(my_data_sets[1])
jpeg("graphs/start_location_bar_chart.jpeg")
ggplot(data=d4, aes(x=start_station_name, y=start_station_count, fill=member_casual))+
  geom_col(position = "dodge", color = "black", width = .75) + 
  coord_flip() + 
  labs(title = "Favorite Start Location of Members and Casual Riders", x="Start Station Name", y="Start Station Count")
dev.off()

# normalized bar chart of the comparing which end destinations are popular among each membership group
d5 <- data.frame(my_data_sets[2])
jpeg("graphs/normalized_start_location_bar_chart.jpeg")
ggplot(data=d5, aes(x=start_station_name, y=normalized_start_station_count, fill=member_casual))+
  geom_col(position = "dodge", color = "black", width = .75) + 
  coord_flip() +
  labs(title = "Normalized Favorite Start Locations", x="Start Station Name", y="Normalized Start Station Count")
dev.off()

#!!! End Location Popularity Analysis !!!#
my_data_sets <- endDestinationPopularity(d)

# bar chart of the comparing which end destinations are popular among each membership group
d6 <- data.frame(my_data_sets[1])
jpeg("graphs/end_destination_bar_chart.jpeg")
ggplot(data=d6, aes(x=end_station_name, y=end_station_count, fill=member_casual))+
  geom_col(position = "dodge", color = "black", width = .75) + 
  coord_flip() + 
  labs(title = "Favorite Destinations of Members and Casual Riders", x="End Station Name", y="End Station Count")
dev.off()

# normalized bar chart of the comparing which end destinations are popular among each membership group
d7 <- data.frame(my_data_sets[2])
jpeg("graphs/normalized_end_destination_bar_chart.jpeg")
ggplot(data=d7, aes(x=end_station_name, y=normalized_end_station_count, fill=member_casual))+
  geom_col(position = "dodge", color = "black", width = .75) + 
  coord_flip() +
  labs(title = "Normalized Favorite Destinations", x="End Station Name", y="Normalized End Station Count")
dev.off()

#!!! Start Time Analysis !!!#
my_data_sets <- rideStartTimes(d)

d8 <- data.frame(my_data_sets[1])
jpeg("graphs/start_time_distribution.jpeg")
ggplot(data = d8, aes(x=time_started_at, y=time_started_at_count, color=member_casual)) + 
  geom_line() +
  scale_x_time(breaks = breaks_width("5 hours"), minor_breaks = breaks_width("1 hour")) +
  labs(title = "Start Time Distribution", x="Time Started At (HH:MM:SS)", y="Count of Time Started At")
dev.off()

d9 <- data.frame(my_data_sets[2])
jpeg("graphs/normalized_start_time_distribution.jpeg")
ggplot(data = d9, aes(x=time_started_at, y=normalized_time_started_at_count, color=member_casual)) + 
  geom_line() + 
  scale_x_time(breaks = breaks_width("5 hours"), minor_breaks = breaks_width("1 hour")) +
  labs(title = "Normalized Start Time Distribution", x="Time Started At (HH:MM:SS)", y="Normalized Count of Time Started At")
dev.off()

