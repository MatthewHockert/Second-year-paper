
combined_data <- st_as_sf(combined_data)
combined_data <- st_transform(combined_data, crs=4326)

combined_data$date <- as.character(combined_data$date)

dates_of_interest <- as.character(c("2019-10-01", "2019-07-01", "2019-04-01", "2019-01-01", 
                                    "2018-10-01", "2018-07-01", "2018-04-01", "2017-10-01", 
                                    "2017-08-01", "2016-08-01", "2015-09-01", "2015-06-01", 
                                    "2015-03-01", "2014-12-01", "2014-09-01", "2014-05-01", 
                                    "2014-03-01", "2013-12-01", "2013-09-01"))
start_time <- Sys.time()
intersect_date <- function(dates) {
  # Step 1: Filter both datasets for the specified date
  combined_data_subset <- combined_data %>% #from above. Original data comes from main script
    subset(date %in% dates)
  
  # filtered_combined_stops <- combined_stops %>% #comes from transit_file_generator.R
  #   subset(date %in% dates)
  # print(nrow(filtered_combined_stops))
  # 
  print("intersection")
  # Step 2: Perform the spatial join
  joined_data <- st_intersection(block_minneapolis,combined_data_subset)
  
  # Step 3: Ensure 'date' is in the correct format
  joined_data <- mutate(joined_data, date = as.character(dates))
  
}

# Split the dates into chunks
n_cores <- detectCores() - 7 
dates_chunks <- split(dates_of_interest, cut(seq_along(dates_of_interest), n_cores, labels = FALSE))

# Prepare the cluster
cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c("intersect_date", "combined_data", "block_minneapolis", "dates_of_interest"))
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
})

# Modify the process_date function to work with a vector of dates
process_vector <- function(dates_vector) {
  results <- lapply(dates_vector, intersect_date)
  return(results)
}

# Run the process_date function in parallel
intersection_results_list <- parLapply(cl, dates_chunks, process_vector)

# Stop the cluster
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

intersection_results_flattened <- unlist(intersection_results_list, recursive = FALSE)

# # Iterate over the flattened list to assign each result to a dynamically named variable
# for (i in seq_along(intersection_results_flattened)) {
#   # Construct the variable name based on the original date
#   variable_name <- paste0("joined_data_", gsub("-", "_", dates_of_interest[i]))
#   # Assign the result to the variable in the global environment
#   assign(variable_name, intersection_results_flattened[[i]], envir = .GlobalEnv)
# }

for (i in seq_along(intersection_results_flattened)) {
  intersection_results_flattened[[i]]$date_processed <- dates_of_interest[i]
}
combined_data_Minneapolis <- do.call(rbind, intersection_results_flattened)
#rm(intersection_results_flattened,intersection_results_list)
names(combined_data_minneapolis)




# combined_data_metro_non_white <- subset(combined_data_Minneapolis, select = c("date","provider_uid","number_of_stops",
#                                                                           "date_processed", "Block_Group_Code","Percent_Black","Percent_White","Percent_Asian",               
#                                                                            "Percent_Male","Percent_Female","Percent_Public_Transportation",
#                                                                           "Percent_Drive","Percent_Bachelors_Or_Higher","Percent_GED_Or_12_Year",      
#                                                                          "Percent_Less_Than_12_Year","geometry"))
# 
# 
# combined_data_metro_non_white2 <- na.omit(combined_data_metro_non_white)

# subset_intersect <- subset(combined_data, subset = date %in% "2019-10-01")
# start_time <- Sys.time()
# 
# # Your code block here
# result <- st_intersection(subset_intersect, block_group_data)
# 
# end_time <- Sys.time()
# time_taken <- end_time - start_time
# time_taken



###### Ogininal ######
combined_data$date <- as.character(combined_data$date)

dates_of_interest <- as.character(c("2019-10-01", "2019-07-01", "2019-04-01", "2019-01-01", 
                                    "2018-10-01", "2018-07-01", "2018-04-01", "2017-10-01", 
                                    "2017-08-01", "2016-08-01", "2015-09-01", "2015-06-01", 
                                    "2015-03-01", "2014-12-01", "2014-09-01", "2014-05-01", 
                                    "2014-03-01", "2013-12-01", "2013-09-01"))
start_time <- Sys.time()
intersect_date <- function(dates) {
  # Step 1: Filter both datasets for the specified date
  combined_data_subset <- combined_data %>% #from above. Original data comes from main script
    subset(date %in% dates)
  
  filtered_combined_stops <- combined_stops %>% #comes from transit_file_generator.R
    subset(date %in% dates)
  print(nrow(filtered_combined_stops))
  # 
  print("intersection")
  # Step 2: Perform the spatial join
  joined_data <- st_join(combined_data_subset, filtered_combined_stops, join = st_intersects)
  
  # Step 3: Ensure 'date' is in the correct format (if not already)
  joined_data <- mutate(joined_data, date = as.character(dates))
  
  ## Need code to count stops ##
}

# Split the dates into chunks
n_cores <- detectCores() - 7 
dates_chunks <- split(dates_of_interest, cut(seq_along(dates_of_interest), n_cores, labels = FALSE))

# Prepare the cluster
cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c("intersect_date", "combined_data", "combined_stops", "dates_of_interest"))
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
})

# Modify the process_date function to work with a vector of dates
process_vector <- function(dates_vector) {
  results <- lapply(dates_vector, intersect_date)
  return(results)
}

# Run the process_date function in parallel
results_list <- parLapply(cl, dates_chunks, process_vector)

# Stop the cluster
stopCluster(cl)
end_time <- Sys.time()
#rm(data_frames_list)
results_flattened <- unlist(results_list, recursive = FALSE)

# # Iterate over the flattened list to assign each result to a dynamically named variable
# for (i in seq_along(intersection_results_flattened)) {
#   # Construct the variable name based on the original date
#   variable_name <- paste0("joined_data_", gsub("-", "_", dates_of_interest[i]))
#   # Assign the result to the variable in the global environment
#   assign(variable_name, intersection_results_flattened[[i]], envir = .GlobalEnv)
# }

for (i in seq_along(results_list)) {
  results_list[[i]]$date_processed <- dates_of_interest[i]
}
combined_data <- do.call(rbind, results_list)

