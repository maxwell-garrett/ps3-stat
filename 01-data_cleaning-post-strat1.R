#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS 2018 ACS 5yrs
# Author: Maxwell Garrett
# Data: 2 November 2020
# Contact: maxwell.garrett@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(forcats)

# Read in the raw data.
setwd("C:/Users/Max/OneDrive - University of Toronto/4th Year/Semester 1/STA304/Problem Set 3")
raw_data <- read_dta("inputs/usa_00004.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(#region,
         stateicp,
         sex, 
         age, 
         race)
         #hispan,
         #marst, 
         #bpl,
         #citizen,
         #educd,
         #labforce,
         #labforce)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

# fix age values
reduced_data <- reduced_data %>% mutate(age = as.character(age))
#"less than 1 year old"
reduced_data <- reduced_data %>% mutate(age=replace(age, age == "less than 1 year old", 1))
#"90 (90+ in 1980 and 1990)" 
reduced_data <- reduced_data %>% mutate(age=replace(age, age == "90 (90+ in 1980 and 1990)", 90))
#"100 (100+ in 1960-1970)"
reduced_data <- reduced_data %>% mutate(age=replace(age, age == "100 (100+ in 1960-1970)" , 100))
#"112 (112+ in the 1980 internal data)"
reduced_data <- reduced_data %>% mutate(age=replace(age, age == "112 (112+ in the 1980 internal data)", 112))
#"115 (115+ in the 1990 internal data)"
reduced_data <- reduced_data %>% mutate(age=replace(age, age == "115 (115+ in the 1990 internal data)", 115))
reduced_data <- reduced_data %>% mutate(age = as.factor(age))

# need to change state names to abbreviations to match survey
reduced_data <- reduced_data %>%
                filter(stateicp != "state not identified") %>%
                filter(stateicp != "military/mil. reservations") %>%
                filter(stateicp != "state groupings (1980 urban/rural sample)")
short_state = c("CT", "ME", "MA", "NH", "RI", "VT", "DE", "NJ",
                "NY", "PA", "IL", "IN", "MI", "OH", "WI", "IA",
                "KS", "MN", "MO", "NE", "ND", "SD", "VA", "AL",
               "AR", "FL", "GA", "LA", "MS", "NC", "SC", "TX",
               "KY", "MD", "OK", "TN", "WV", "AZ", "CO", "ID",
               "MT", "NV", "NM", "UT", "WY", "CA", "OR", "WA",
               "AK", "HI", "PR", "REMOVE", "REMOVE", "DC", "REMOVE")
levels(reduced_data$stateicp) <- short_state
                                  
reduced_data <- reduced_data %>% mutate(state = stateicp)                              

reduced_data <- reduced_data %>% mutate(gender = fct_recode(sex, Male = "male", Female = "female"))


#"white"  => White                         
#rest => non-white \
reduced_data <- reduced_data %>% mutate(race = as.character(race))
reduced_data <- reduced_data %>% mutate(white=replace(race, race != "white", "non-white"))

reduced_data <- 
  reduced_data %>%
  count(age, gender, state, white) %>%
  group_by(age) 

reduced_data$age <- as.integer(reduced_data$age)

reduced_data <- reduced_data %>% filter(age >= 18)
# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data.csv")



         