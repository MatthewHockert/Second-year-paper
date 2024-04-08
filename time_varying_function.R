#generate_time_varying_dataset 

generate_time_varying_dataset <- function(provider_data, child_care_centers_2008, child_care_centers2020, start_date_2013 = "2013-09-01"){
  library(dplyr)
  library(lubridate)
  
  provider_data_processed <- provider_data %>%
  arrange(provider_uid, date) %>%
  group_by(provider_uid) %>%
  mutate(
    next_date = dplyr::lead(date),  # Get the date of the next observation
    time_to_next = next_date - date,  # Calculate time until next observation
    event = if_else(is.na(time_to_next), 1, 0)  # Mark the last observation with 1
  ) %>%
  ungroup()

provider_data_processed <- provider_data_processed %>%
  arrange(provider_uid, date) %>%
  group_by(provider_uid) %>%
  mutate(start_date = min(date),  # Calculate start date for each provider
         duration = as.numeric(date - start_date)) %>%
  ungroup()

# make sure censoring is correct by making sure the childcare centers event are right in 2019
#provider_data_processed$event <- ifelse(provider_data_processed$date == "2019-10-01", provider_data_processed$event == 1,provider_data_processed$event)
provider_data_processed <- provider_data_processed %>%
  mutate(event = ifelse(date == as.Date("2019-10-01") & provider_uid %in% child_care_centers2020$provider_uid, 0, event))

#provider_data_processed$event <- ifelse(provider_data_processed$provider_uid %in% child_care_centers2020$provider_uid,provider_data_processed$event == 0,provider_data_processed$event == 1)
provider_data_processed <- provider_data_processed %>%
  mutate(event = ifelse(date == as.Date("2019-10-01") & provider_uid %in% child_care_centers2020$provider_uid, 0, 
                        ifelse(date == as.Date("2019-10-01"), event, event)))

provider_data_processed$event <- as.numeric(provider_data_processed$event)

provider_data_processed <- provider_data_processed %>%
  arrange(provider_uid, date)


provider_data_time_varying <- provider_data_processed %>%
  group_by(provider_uid) %>%
  arrange(provider_uid, date) %>%
  mutate(
    tstart = (duration),  # Start of the interval
    tstop = dplyr::lead(duration, default = last(duration))  # End of the interval
  ) %>%
  ungroup()

provider_data_time_varying <- provider_data_time_varying %>%
  left_join(dates_df, by = c("date" = "dates")) %>%
  mutate(
    # Adjust tstop based on the condition tstart == tstop
    tstop = if_else(tstart == tstop, tstop +days_diff, tstop)
  )
provider_data_time_varying <- provider_data_time_varying %>%
  arrange(provider_uid, date)

# Find minimum date
providers_starting_2013 <- provider_data_time_varying %>%
  group_by(provider_uid) %>%
  summarise(start_date_2013 = min(date)) %>%
  filter(start_date_2013 == as.Date(start_date_2013)) %>%
  select(provider_uid)

# Calculate the duration since the first appearance in 2008 for the select providers
duration_since_first_appearance <- child_care_centers_2008 %>%
  inner_join(providers_starting_2013, by = "provider_uid") %>%
  group_by(provider_uid) %>%
  summarise(first_date = min(date)) %>%
  mutate(duration_since_2008 = as.numeric(as.Date("2013-09-01") - first_date)) %>%
  select(provider_uid, duration_since_2008)

# Join and adjust tstart and tstop for these providers
provider_data_time_varying_adjusted <- provider_data_time_varying %>%
  left_join(duration_since_first_appearance, by = "provider_uid") %>%
  mutate(
    tstart_adjusted = if_else(!is.na(duration_since_2008), tstart + duration_since_2008, tstart),
    tstop_adjusted = if_else(!is.na(duration_since_2008), tstop + duration_since_2008, tstop)
  ) %>%
  select(-tstart, -tstop, -duration_since_2008) %>%
  rename(tstart = tstart_adjusted, tstop = tstop_adjusted)

provider_data_time_varying_adjusted <- provider_data_time_varying_adjusted %>%
  arrange(provider_uid, date)

return(provider_data_time_varying_adjusted)
}
