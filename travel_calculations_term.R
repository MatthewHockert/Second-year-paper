Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home")
library(sf)
library(rJava)
library(r5r)
library(dplyr)
library(sp)
options(java.parameters = "-Xmx8g")
r5_core <- setup_r5(data_path = "/Users/matthewhockert/Desktop/UMN/second year paper/osm/", verbose = TRUE)

dates_of_interest <- as.character(c("2019-10-01", "2019-07-01", "2019-04-01", "2019-01-01", 
                                    "2018-10-01", "2018-07-01", "2018-04-01", "2017-10-01", 
                                    "2017-08-01", "2016-08-01", "2015-09-01", "2015-06-01", 
                                    "2015-03-01", "2014-12-01", "2014-09-01", "2014-05-01", 
                                    "2014-03-01", "2013-12-01", "2013-09-01"))


# source("/Users/matthewhockert/personal_projects/travel_calculations_term.R")
combined_stops_import <- sf::st_read("/Users/matthewhockert/Desktop/UMN/second year paper/combined_stops.shp")
names(combined_stops_import)[names(combined_stops_import) == "stop_id"] <- "id"
combined_stops_import <- st_transform(combined_stops_import,4326)
combined_stops_import$dates <- as.character(combined_stops_import$date)
combined_stops_import$opportunities <- 1

child_care_centers_mp2_select_import <- st_read("/Users/matthewhockert/Desktop/UMN/second year paper/child_care_centers_mp2_select.shp")
names(child_care_centers_mp2_select_import)[names(child_care_centers_mp2_select_import) == "prvdr_d"] <- "id"
child_care_centers_mp2_select_import$dates <- as.character(child_care_centers_mp2_select_import$date)
child_care_centers_mp2_select_import <- st_transform(child_care_centers_mp2_select_import,4326)


all_results_df <- data.frame()

for (x in dates_of_interest) {
  # Filter childcare centers for the current date
  print(x)
  filtered_child_care_centers <- child_care_centers_mp2_select_import %>%
    dplyr::filter(dates %in% x)
  print(nrow(filtered_child_care_centers))
  
  filtered_combined_stops <- combined_stops_import %>% #comes from transit_file_generator.R
    dplyr::filter(dates %in% x)
  print(nrow(filtered_combined_stops))
  
  filtered_combined_stops$opportunities <- 1
  
  access_df <- accessibility(r5_core,
                            origins = filtered_child_care_centers,
                            destinations = filtered_combined_stops,
                            mode = "WALK",
                            departure_datetime = as.POSIXct(paste(x, "08:00:00"), tz = "America/Chicago"),  # Adjust time as necessary
                            cutoffs = 10)
  access_df <- as.data.frame(access_df)
  access_df$date <- x
  print(access_df)
  print(class(access_df))
  
  all_results_df <- rbind(all_results_df, access_df)

}

# all_results_df <- NULL
# 
# for (i in seq_along(results_list)) {
#   # Now, for each index, loop through each data frame
#   for (j in seq_along(results_list[[i]])) {
#     # Access the data frame
#     all_results_df <- results_list[[i]][[j]]
#   }
# }


names(all_results_df)[names(all_results_df) == "accessibility"] <- "number_of_stops_walking"
names(all_results_df)[names(all_results_df) == "id"] <- "provider_uid"
#rm(all_results_df)
rm(results_list)
#rm(all_results)

# # Optionally, combine all results into a single dataframe
# all_results <- do.call(rbind, results_list)
# all_results_df <- as.data.frame(all_results)

write.csv(all_results_df, "/Users/matthewhockert/Desktop/UMN/second year paper/all_results_df.csv", row.names = FALSE)
all_results_df <- read.csv("/Users/matthewhockert/Desktop/UMN/second year paper/all_results_df.csv")


