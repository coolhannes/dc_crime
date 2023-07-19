setwd("/Users/johannesfischer/git/dc_crime")

library(readr)
library(tidyverse)
dc_crimes_raw <- read_csv("~/Downloads/dc-crimes-search-results.csv")

library(tidycensus)
library(dplyr)
library(stringr)


# Set up a vector of years from 2008 to the current year (2023)
years <- 2009:2022

# Create an empty list to store the results for each year
dc_population_data <- list()

# Loop through each year and retrieve Census data
for (year in years) {
    # Define the variables we want to retrieve (total population for Washington, D.C.)
    vars <- c("NAME", "B01003_001E")
    
    # Fetch data from the Census API for Washington, D.C. for the specified year
    dc_data <- get_acs(
        geography = "state", 
        variables = vars, 
        state = "DC", 
        year = year, survey = "acs5"
    )
    
    # Append the data to the list, along with the corresponding year
    dc_population_data[[as.character(year)]] <- dc_data
}

# Combine the data from all years into a single data frame
dc_population_df <- bind_rows(dc_population_data, .id = "year")

# Clean up the column names and filter relevant columns
dc_population_df <- dc_population_df %>%
    rename(year = year) %>%
    select(year, population = estimate)

# Convert the 'year' column to numeric
dc_population_df$year <- as.numeric(dc_population_df$year)

# Sort the data frame by year
dc_population_df <- arrange(dc_population_df, year)
rm(dc_population_data, dc_data)



dc_crimes <- dc_crimes_raw %>%
    janitor::clean_names() %>%
    select(
        year, district, census_tract, block_group, location,
        start_date, end_date, report_dat, 
        offensekey, offense_text, offense, method
    ) %>%
    mutate(
        start_date = lubridate::mdy_hms(start_date, tz = "EST"),
        end_date = lubridate::mdy_hms(end_date, tz = "EST"),
        report_dat = lubridate::mdy_hms(report_dat, tz = "EST"),
        day_of_year = lubridate::yday(start_date),
        month_of_year = factor(lubridate::month(start_date)),
        year = lubridate::year(start_date),
    ) %>%
    left_join(dc_population_df) %>%
    mutate(
        offense = case_when(
            offense %in% c('theft f/auto', 'theft/other') ~ 'theft',
            offense == "motor vehicle theft" ~ "auto theft",
            offense == 'assault w/dangerous weapon' ~ 'assault',
            TRUE ~ offense
        ),
        population = coalesce(population, 683154),
        district_name = paste("District", district)
    ) %>%
    filter(year >= 2008)


dc_crimes_plot <- dc_crimes %>%
    filter((year == 2023 & as.integer(month_of_year) < 7) | year < 2023) %>%
    group_by(year, month_of_year, district_name, district, offense, population) %>%
    summarize(
        incidents = n()
    ) %>%
    mutate(
        incident_rate = round(incidents/(population/1000),3),
        year = factor(year),
        Time = factor(ifelse(year == '2023', 'Current (2023)', 'Past (Since 2008)'))
    ) %>%
    ungroup() %>%
    filter(!is.na(month_of_year) & !is.na(district)) %>%
    filter(!offense %in% c("arson"))
    
historical_data <- dc_crimes_plot %>%
    filter(year != '2023') %>%
    group_by(district_name, offense, month_of_year) %>%
    summarize(
        mean = mean(incident_rate), 
        sd = sd(incident_rate)
    ) %>%
    mutate(
        incident_rate = mean,
        low = mean - 2*sd,
        high = mean + 2*sd
    )

dc_crimes_plot %>%
    filter(year == "2023") %>%
    ggplot(aes(x = month_of_year, y = incident_rate, group = year, color = "red")) + 
    geom_line(data = historical_data, color = "grey") + 
    geom_ribbon(data = historical_data, aes(ymin = low, ymax = high), color = "grey", alpha = 0.1) + 
    geom_point(alpha = .70) + 
    geom_line() + 
    facet_grid(rows = vars(offense), cols = vars(district_name), scales = "free_y") + 
    theme_minimal() +
    theme(legend.position="none") + 
    labs(x = 'Month', y = "Crime Rate / 1000 People")

