library(dplyr)
library(gt)
provider_data_time_varying_minneapolis$event_char <- as.character(provider_data_time_varying_minneapolis$event)

desc_stats <- subset(provider_data_time_varying_minneapolis, select = c("number_of_stops_walking","type_of_care", "event_char",
                                                                'Percent_Black','Percent_White', 'Percent_Asian', 
                                                                'Percent_Drive', 'Percent_Public_Transportation', 
                                                                'Percent_Bachelors_Or_Higher', 'Percent_Less_Than_12_Year'))
desc_stats <- st_set_geometry(desc_stats, NULL)
numeric_stats <- desc_stats %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    n = ~sum(!is.na(.))
  ), .names = "{.col}_{.fn}")) 

# Instead of separating on '_', modify the approach to create a 'measure' and 'variable' column correctly
numeric_stats_long <- pivot_longer(numeric_stats, cols = everything(), names_to = "variable_stat", values_to = "value")

# Extract the last occurrence of "_" to separate statistic from variable names
numeric_stats_long <- numeric_stats_long %>%
  mutate(statistic = sub(".*_(.*)$", "\\1", variable_stat),
         variable = sub("_(mean|sd|median|min|max|n)$", "", variable_stat, perl = TRUE)) %>%
  select(-variable_stat) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(statistic, everything()) %>%
  arrange(match(statistic, c("mean", "sd", "median", "min", "max", "n")))

# This creates a new column 'statistic' for the type of statistic and 'variable' for the original variable name,
# then rearranges the data as needed.

# Printing the structured data frame
print(numeric_stats_long)

combined_data_minneapolis_non_sf_final$Block_Group_Code <- as.character(combined_data_minneapolis_non_sf_final$Block_Group_Code)

minneapolis_desc_stats <- subset(provider_data_processed_minneapolis, select = c("number_of_stops","type_of_care"))
minneapolis_desc_stats <- st_set_geometry(minneapolis_desc_stats,NULL)
numeric_stats <- minneapolis_desc_stats %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    n = ~sum(!is.na(.))
  ), .names = "{.col}_{.fn}"))  # This appends the function name to the original column name
numeric_stats
# Check the names of the columns generated
numeric_stats_long <- numeric_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_pattern = "(.+)_(mean|sd|median|min|max|n)",
    values_to = "value"
  )
numeric_stats_long
print(names(numeric_stats))
# Ensure that the column names are unique before this step
numeric_stats_wide <- numeric_stats_long %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

# Now print the table to see the structure
print(numeric_stats_wide)

categorical_stats <- desc_stats %>%
  select(event_char) %>%
  group_by(event_char) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# If you have more than one categorical variable, repeat the process for each,
# or automate it with a loop or lapply for efficiency.

print(categorical_stats)
