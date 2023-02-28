library(tidyverse)
library(lubridate)
library(ggplot2)

firefighter = read_csv("Fire_Incidents.csv")
#1. How long does it take Wake County Fire to respond to incidents, on average (i.e. the time between when an incident is dispatched and when firefighters arrive on the scene)? (hint: you can subtract lubridate date columns from each other). 
firefighter = mutate(firefighter, arrive_date_time = ymd_hms(arrive_date_time)) #This line converts the arrive_date_time column from a character string to a datetime object.
firefighter= mutate(firefighter, dispatch_date_time = ymd_hms(dispatch_date_time)) #This line does the same but converts the dispatch_date_time column from a character string to a datetime object. 
firefighter= mutate(firefighter, subtract_time = arrive_date_time - dispatch_date_time) #This line creates a new column in the firefighter dataset called subtract_time, which is the difference between the arrive_date_time and dispatch_date_time columns. This will give us the response time for every incident in the dataset.
mean(firefighter$subtract_time, na.rm = T) #This line calculates the mean response time for all the incidents in the firefighter dataset, using the subtract_time column that I created in the line above. The na.rm = T removes the null values. 

#The time difference of 318.7497 secs or 5.31 minutes. Therefore, it takes around 5.31 minutes for Wake County Fire to respond to incidents. 

#2. Does this response time vary by station? What stations have the highest and lowest average response times?
high_low_response = group_by(firefighter, station) %>%
  summarize(
    subtract_time=mean(subtract_time, na.rm = T) 
  ) #This code created a new data frame called high_low_response. I grouped the firefighter dataset by the "station" column. Then, I calculated the mean value of the subtract_time variable I created before. for each group using the summarize() function. The na.rm = T removed the null values.  The new high_low_response data frame shows the average response time for each fire station in the firefighter dataset. 
#The found that station 13 has the lowest response time of 223.0000 secs and station 29 has the highest response time of 495.7640 secs.


#3. Have Wake County Fire’s response times been going up or down over time? What might be the reason for these changes? 

firefighter$response_time <- as.numeric(difftime(firefighter$arrive_date_time, firefighter$dispatch_date_time, units = "mins")) #First I calculated the response time for each incident using the difftime() function. This calculated the difference between the arrive_date_time and dispatch_date_time columns and converted it to minutes. 
firefighter$year <- year(firefighter$arrive_date_time) #Next I created a new column called "year". 
firefighter_avg <- firefighter %>% 
  group_by(year) %>%
  summarize(avg_response_time = mean(response_time)) # I created a new dataframe called firefighter_avg. I grouped the data by year and calculated the mean response time for each year using the mean() function. 

firefighter_merged <- merge(firefighter, firefighter_avg, by = "year") #I merged the original firefighter dataset with the firefighter_avg dataframe by the year column. 
ggplot(firefighter_avg, aes(x = year, y = avg_response_time)) +
  geom_line()
ggplot(firefighter_avg, aes(x = year, y = avg_response_time)) +
  geom_line() +
  labs(title = "Average Response Time by Year",
       x = "Year",
       y = "Average Response Time (mins)") #Created a line plot using ggplot to visualize the average response time by year. The geom_line() function created the line plot, and I used the labs() function to add a title and axis labels to the plot.


#The response time was declining drastically. 

#4. At what times of day are fire calls most likely to occur? 

firefighter$time_of_day <- hour(firefighter$dispatch_date_time) #I first created a new column in the dataset called "time_of_day", which extracts the hour from the "dispatch_date_time" column using the "hour" function 
ggplot(data = data.frame(freq_table), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Fire Calls by Time of Day",
       x = "Hour of the Day",
       y = "Number of Fire Calls")
#Then I created a bar chart.. I used the "data.frame" function, which converts the frequency table into a dataframe. I set the "x" and "y" aesthetics to "Var1" and "Freq". "Var1" represents the values in the "time_of_day" column. "Freq" represents the frequency of each value. The "geom_bar" function makes sure the it is a bar chart. I used the "labs" function to add a title and labels to the x and y axis.

#around 9-11 pm

#5. The dataset contains all types of fire department calls, other than emergency medical services (which are removed to protect privacy). The codes for the different incident types can be found on page 3-22 of the National Fire Incident Reporting System Complete Reference Guide. How many calls to Wake County Fire are recorded in this dataset, and how many of them are actual fires? 

nrow(firefighter) #this counted all the calls made
# I first defined a vector for the incident type codes for fires using the reference guide. 
fire_codes <- c(111,112,113,114,115,116,117,118,121,122,123,120,131,132,133,134,135,136,137,138,130,141,142,143,140,151,152,153,154,155,150,161,162,163,164,160,171,172,173,170,100)

# I then filtered the firefighter dataset for incident types that are actual fires. 
fire_calls <- firefighter[firefighter$incident_type %in% fire_codes,]

# I counted the number of fire calls
num_fire_calls <- nrow(fire_calls)

# This was used to print the number of fire calls. I concatenated the text with the value. 
cat("Number of fire calls:", num_fire_calls)


#229047 total calls in dataset
#17230 number of actual fire calls


#6. It is reasonable that firefighters might respond more quickly to some types of incidents than others (e.g., a building fire, code 111 might be higher priority than a cat stuck in a tree, code 542). Using the reference guide linked above to determine appropriate incident codes, evaluate the average response time to actual fires. Is this response time faster than the average response time for all incidents? 

# First I subset the data to only include actual fires
actual_fires <- firefighter[firefighter$incident_type %in% c(111,112,113,114,115,116,117,118,121,122,123,120,131,132,133,134,135,136,137,138,130,141,142,143,140,151,152,153,154,155,150,161,162,163,164,160,171,172,173,170,100), ]
# Then I calculated the response time in minutes
actual_fires$response_time <- as.numeric(difftime(actual_fires$arrive_date_time, actual_fires$dispatch_date_time, units = "mins"))
# Then I calculated the average response time across all the years while removing the null values. 
mean(actual_fires$response_time, na.rm = TRUE)
#The result was 5.18 minutes response time for actual fires. 

# Now for all the incidents, I had already done this but I did it again for all the average response time for all incidents. I did it in minutes this time. 
average_response_time <- mean(as.numeric(difftime(firefighter$arrive_date_time, firefighter$dispatch_date_time, units = "mins")), na.rm = TRUE) #The response time was calculated by subtracting the dispatch_date_time from the arrival_date_time. This time difference was converted into minutes using the difftime() function with the units set to "mins". 
# I then concatenated the text with the value to display the average response time in minutes for all incidents. I also used the round function for the value. 
cat("The average response time for all incidents is", round(average_response_time, 2), "minutes.")
#5.31 minutes



#7

#variation by station for actual fires, response time. 

# I first filtered for the actual fires. 
actual_fires <- firefighter %>% filter(incident_type %in% c("111","112","113","114","115","116","117","118","121","122","123","120","131","132","133","134","135","136","137","138","130","141","142","143","140","151","152","153","154","155","150","161","162","163","164","160","171","172","173","170","100"))

# Then I calculated the average response time for actual fires by station
avg_response_time_by_station <- actual_fires %>%
  group_by(station) %>%
  summarize(avg_response_time = mean(response_time, na.rm = TRUE)) #I took the actual_fires dataframe and grouped it by the station column. Then I calculated the mean of the response_time column. The new data frame has two columns: station and avg_response_time, which gives the average response time for each fire station.

#station 3 has lowest response time to actual fires of 3.88 minutes and station 23 highest response time of 9.79 minutes. 

####################################

# Calculate the average response time for actual fires by year
# Filtered the dataset to include only actual fires
actual_fires <- firefighter %>% filter(incident_type %in% c("111", "112", "113", "114", "115", "116", "117", "118", "121", "122", "123", "120", "131", "132", "133", "134", "135", "136", "137", "138", "130", "141", "142", "143", "140", "151", "152", "153", "154", "155", "150", "161", "162", "163", "164", "160", "171", "172", "173", "170", "100"))

# I calculated the response time and year
actual_fires$response_time <- as.numeric(difftime(actual_fires$arrive_date_time, actual_fires$dispatch_date_time, units = "mins"))
actual_fires$year <- year(actual_fires$arrive_date_time)

# I calculated the average response time for each year
wake_avg_response_time <- actual_fires %>% 
  group_by(year) %>% 
  summarize(avg_response_time = mean(response_time, na.rm = TRUE))

# I created a line graph with the results
ggplot(wake_avg_response_time, aes(x = year, y = avg_response_time)) +
  geom_line() +
  labs(title = "Wake County Average Response Time to Actual Fires",
       x = "Year",
       y = "Average Response Time")


#############################



# Filter the firefighter dataset by the specified incident codes
firefighter_filtered <- firefighter %>% 
  filter(incident_type %in% c("111","112","113","114","115","116","117","118",
                              "121","122","123","120","131","132","133","134",
                              "135","136","137","138","130","141","142","143",
                              "140","151","152","153","154","155","150","161",
                              "162","163","164","160","171","172","173","170","100"))

# Created a new column to store the hour of the day
firefighter_filtered$time_of_day <- hour(firefighter_filtered$dispatch_date_time)

# Created a frequency table for the time of day column
freq_table <- table(firefighter_filtered$time_of_day)

# Created a bar chart to visualize the frequency of fire calls by time of day
ggplot(data = data.frame(freq_table), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Fire Calls by Time of Day for Actual Fires",
       x = "Hour of the Day",
       y = "Number of Fire Calls") 

#8-10 pm was the time fire calls were more likely to occur for actual fires. 


