# Install packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, # tidyverse pkgs including purrr
  glue, # combining strings and objects
  gapminder, # dataset
  ggplot2, # plotting
  gridExtra, # arranging plots
  skimr, # Skim Inspection
  VIM, # Imputation
  kableExtra # Table
) 

options(scipen = 999)

# Importing data:
energy_access = read.csv("Number of People with and without energy access.csv")

###################################
### Inspecting for missing values, means, and standard deviations:
glimpse(energy_access) # View columns and data type for each columns
skim(energy_access) # Check for Number or Missing Value, mean and SD


###################################
### Data Wrangling 

# 1. Select only the necessary columns for analysis to reduce data complexity.
# In this case, we choose columns relevant to energy access and year for simplicity.
Selecting_variables = function(data){
  selected = data %>%
    select(Entity, Year, Number.of.people.with.access.to.electricity,Number.of.people.without.access.to.electricity, number_with_clean_fuels_cooking) %>%
    rename(Country = Entity, 
           Number_with_electricity = Number.of.people.with.access.to.electricity,
           Number_with_clean_fuels_cooking = number_with_clean_fuels_cooking,
           Number_without_electricity = Number.of.people.without.access.to.electricity)
  return(selected)
}



# 2. Clean those missing values and non-responses:
# Impute missing values with hotdeck imputation
Clean_missing_values = function(data) {
  data_na <- data %>%
    mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))
  missing_value = hotdeck(data_na) %>%
    select( -contains("_imp"))
  return(missing_value)
}



# 3. Create a new column for whether the number of people who have electricity is greater than the number without:
More_access_energy = function(data) {
  create_new_column = data %>%
    mutate(More_access = ifelse(Number_with_electricity > Number_without_electricity, TRUE, FALSE))
  return(create_new_column)
}


# 4. Check the average for each country:
Country_average = function(data) {
  energy_average = data %>%
    group_by(Country) %>%
    filter( !str_detect(Country, regex("Euro|Asia|pacific|IDA|IBRD|income|World|Early|Late|demographic|and|&|area|countries|OECD", ignore_case = TRUE))) %>%
    summarise( average_electricity_access = mean(Number_with_electricity, na.rm = TRUE) %>% round(2),
               average_no_electricity_access = mean(Number_without_electricity, na.rm = TRUE) %>% round(2),
               average_access_to_clean_fuels = mean(Number_with_clean_fuels_cooking, na.rm = TRUE) %>% round(2)
    )
  return(energy_average)
}


### Call functions:
energy_selected = Selecting_variables(energy_access)
glimpse(energy_selected)

energy_NA = Clean_missing_values(energy_selected)
glimpse(energy_NA)

energy_new_column = More_access_energy(energy_NA)
glimpse(energy_new_column)

energy_cleaned = Country_average(energy_new_column)
glimpse(energy_cleaned)


#####################################
### Data Visualisation:
# Create tables to see the top 10 countries with the highest and lowest number of people who have access to energy:
# And put them together by using map() function:

top_10_energy = energy_cleaned %>%
  arrange(desc(average_electricity_access)) %>%
  slice(0:10) %>%
    # Apply formatting for the table:
  kable(caption = "Top 10 Countries by Electricity access") %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)
top_10_energy


### Visualization 1: Access to Electricity by Country 
filtered_top_10_energy = energy_cleaned %>%
  filter(Country %in% c("China", "India", "North America", "United States", "Sub-Saharan Africa","Indonesia",
                        "Brazil", "Russia", "Japan", "Pakistan"))

bar_plot = function(data, variable) {
  ggplot(data, aes(x = Country, y = !!sym(variable))) +
    geom_bar(stat = "identity", fill = "orange") +
    coord_flip() +
    labs(
      title = glue("{variable} (Average Number of People) by Country"),
      x = "Country",
      y = variable
    ) 
}
bar_variables = c("average_electricity_access", "average_no_electricity_access")
bar_plots_list = map(bar_variables, ~bar_plot(filtered_top_10_energy, .x))
bar_plots_grid = grid.arrange(grobs = bar_plots_list, nrow = 2)
bar_plots_grid


### Visualization 2: Trends Over Time (Line Plot) for China and India (Top 2 number of access to energy)
filtered_energy = energy_selected %>%
  filter(Country %in% c("China", "India"))

line_plot <- function(data, y) {
  ggplot(data, aes(x = Year, y = !!sym(y), color = Country)) +
    geom_line(size = 1) +
    geom_point() +
    #geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add trend line with confidence interval
    labs(
      title = glue("Trend of {y} Over Time in China and India"),
      x = "Year",
      y = glue("{y}"),
      color = "Country"
    ) 
}

line_variables = c("Number_with_electricity", "Number_without_electricity")
line_plots_list = map(line_variables, ~line_plot(filtered_energy, .x))
line_plots_grid = grid.arrange(grobs = line_plots_list, ncol = 2)
line_plots_grid


### Visualisation 3: Relationship Between Clean Cooking Fuels and Electricity Access 
scatter_plot <- function(data, x, y) {
  ggplot(data, aes(x = !!sym(x) , y = !!sym(y))) +
    geom_point() +
    geom_smooth(method = "lm", color = "green", linetype = "dashed") +
    labs(
      title = glue("Relationship Between {x} and {y}"),
      x = glue("{x}"),
      y = glue("{y}")
    )
}

x_variable = "average_electricity_access"
y_variable = "average_access_to_clean_fuels"
scatter_plot = map(y_variable, ~scatter_plot(energy_cleaned, x_variable, .x))
scatter_plot


### Visualisation 4: Average number of access to energy by income level:
income_average = energy_new_column %>%
  group_by(Country) %>%
  filter( str_detect(Country, regex("income", ignore_case = TRUE)), !str_detect(Country, regex("&|-", ignore_case = TRUE))) %>%
  summarise( average_electricity_access = mean(Number_with_electricity, na.rm = TRUE) %>% round(2))

income_bar_plot = function(data, variable) {
  ggplot(data, aes(x = Country, y = !!sym(variable))) +
    geom_bar(stat = "identity", fill = "yellow") +
    labs(
      title = glue("{variable} (Average number of people) by Income Level"),
      x = "Income Level",
      y = variable
    ) 
}
income_bar_plot(income_average, "average_electricity_access")













