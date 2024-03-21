# Files are found here - https://transitfeeds.com/p/metro-transit/179?p=116
# Additional resources - https://gtfs.org/resources/data/
# Takes GTFS files from my local desktop, filters, edits, and binds them

library(sf)
library(dplyr)
library(stringr)

read_and_combine_gtfs_stops <- function(base_path) {
  folders <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  combined_stops <- NULL
  
  for (folder in folders) {
    tryCatch({
      folder_name <- basename(folder)
      stops_file_path <- file.path(folder, "stops.txt")
      stops_data <- import(stops_file_path)
      
      # Extracting date information from folder name
      date_parts <- str_match(folder_name, "([a-zA-Z]+)([0-9]{4})")
      month_name <- tolower(date_parts[,2])  # Convert month name to lowercase
      year <- date_parts[,3]
      
      # Convert full month name to month number
      month_num <- match(month_name, tolower(month.name))
      if (is.na(month_num)) {
        stop(paste("Month name not found for folder:", folder_name))
      }
      date_column <- as.Date(sprintf("%04d-%02d-01", as.numeric(year), month_num))
      
      # Add the date column to the stops data
      stops_with_date <- mutate(stops_data, date = date_column)
      
      # Convert the stops data into a spatial data frame
      stops_sf <- st_as_sf(stops_with_date, coords = c("stop_lon", "stop_lat"), crs = 4326)
      stops_sf <- stops_sf %>% select(stop_id,date, stop_name, stop_desc, geometry)
      
      stops_sf <- stops_sf %>%
        arrange(desc(date))
      # Combine with the previously combined data
      combined_stops <- if (is.null(combined_stops)) {
        stops_sf
      } else {
        rbind(combined_stops, stops_sf)
      }
    }, error = function(e) {
      message("Error in processing folder: ", folder, "\nError Message: ", e$message)
    })
  }
  
  combined_stops <- combined_stops %>%
    arrange(desc(date))
  
  return(combined_stops)
}

combined_stops <- read_and_combine_gtfs_stops("../second year paper/gtfs_transitfeeds")

mtva_stops <- read_and_combine_gtfs_stops("../second year paper/mtva_feeds")

# Combine data
combined_stops <- rbind(combined_stops,mtva_stops)
combined_stops <- combined_stops %>%
  arrange(stop_id,date)



non_unique_ids_by_date <- combined_stops %>%
  group_by(stop_id, date) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(Count > 1)

non_unique_ids_by_date <- non_unique_ids_by_date %>%
  select(stop_id, date)

non_unique_ids_by_date
