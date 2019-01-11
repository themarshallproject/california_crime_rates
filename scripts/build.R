# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Call all other scripts in the repo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Settings

require(tidyverse) #dplyr, tidyr, readr, ggplot2
require(readxl) # To read .xlsx files
require(zoo) # for na.locf
require(scales)
require(stringr)
require(ggrepel)
require(stats) # For principal component analysis
require(factoextra) # Used to create a ggplot2-based viz of PCA

setwd("~/Documents/GitHub/california_crime_rates")



# 1. Population data ==================================================================

# Crime rates are defined as number of crimes per 100,000 people, which means their 
# computation requires population data. The FBI UCR includes population in its crime 
# data, but the California DOJ does not, so we obtained that information for theGolden
# State and its 58 counties from the Census.
source('scripts/population/population_ca_county_level.R') # County level
source('scripts/population/population_ca_city_level.R') # City level



# 2. Crime rates =====================================================================

# Using the population output calculated in step 1 and the crime data from the CA DOJ
# and the FBI UCR, we now calculate the annual crime rates for the period 1985-2017,
# at various levels.
source('scripts/crime/crime_rates_us_state_level.R') # For the 50 states
source('scripts/crime/crime_rates_ca_county_level.R') # For California's 58 counties
source('scripts/crime/crime_rates_ca_agency_level.R') # For California's local agencies



# 3. Personnel =======================================================================

# Next, we calculate numbers for personnel employed in California's criminal justice
# system (law enforcement, pubic defending, prosecution and paroling.)
source('scripts/personnel/personnel_ca_cjs.R')



# 4. Story fact-checking =============================================================

# Finally, we fact-check all the data quoted in our published stories.
source('scripts/story_checking/fact_check_cali_story_lat.R')
source('scripts/story_checking/fact_check_cali_story_tmp.R')
source('scripts/story_checking/fact_check_joaquin_story.R')
