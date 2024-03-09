library(osrm)

options(osrm.profile = "foot")
# 
# # Function to calculate walking times from a childcare center to all bus stops
# calculate_walking_times <- function(childcare_center) {
#   # Ensure the osrmTable function call is correct
#   routes <- osrmTable(loc = st_coordinates(child_care_centers_mp2_select_latest),
#                       dst = st_coordinates(combined_stops_latest))
#   
#   # Extract walking times
#   walking_times <- routes$durations
#   return(walking_times)
# }
# 
# # Apply the function to each childcare center
# # Correctly applying the function to each row of the sf dataframe
# walking_times_list <- lapply(1:nrow(child_care_centers_mp2_select_latest), function(i) {
#   calculate_walking_times(child_care_centers_mp2_select_latest[i, , drop = FALSE])
# })
# # childcare_centers_sf <- st_as_sf(child_care_centers_mp2_select_latest, coords = c("longitude", "latitude"), crs = crs)

child_care_centers$osrm_node <- osrm::osrmTable(loc = st_coordinates(child_care_centers))$sources
transit_stops$osrm_node <- osrm::osrmTable(loc = st_coordinates(transit_stops))$sources



