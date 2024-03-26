# Second-year-paper
The information provided in this GitHub is from my second year paper as a PhD student at the University of Minnesota Applied Economics Program.
The childcare data used here is confidential and will not be shared.

Process:

1. Start with second_year_paper.R and run the childcare import and cleaning. This will form several childcare files that will be used in future steps.
2. Go to transit_file_generator.R to pull and subset the transit stops from the locally stored gtfs files.
3. Calculate walkability in the travel_calculations_term.R and use the all_results_df in second_year_paper.R.
4. Run the intersection script with child_care_centers dataframe from second_year_paper.R and the appropriate census level shapefile. This will generate the combined_data dataframe.
5. Go back to the second_year_paper.R script and merge all_results_df and combined_data by provider id and date.
6. Run the processing data functionality to manipulate the data to work with a cox model.
7. Run Models.
