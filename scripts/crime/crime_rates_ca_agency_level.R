# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Compute crime rates and crime rate changes at CA law enforcement agency level
# Sources:  California DOJ Open Justice Initiative, U.S. Census
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. SOURCE DATA ========================================================================

# 1.1. Population data -----

df_population <- read.csv('output_data/population_1980_2017_census_ca_cities.csv', header = TRUE, sep = ',')
df_population <- filter(df_population, year >= 1985) # We have no crime data before 1985

# 1.2. Crime data -----

# browseURL('https://openjustice.doj.ca.gov/downloads/Crimes_and_Clearances_with_Arson-1985-2017.csv')
df_crimes <- read.csv('source_data/crime/Crimes_and_Clearances_with_Arson-1985-2017.csv', header=TRUE, sep=',')

# Correct some column name misspellings
names(df_crimes) <- gsub('Sum', 'sum', names(df_crimes))

# Note 1:
# Colums represent number of crimes, but not all their values are >= 0.
# Most are small negatives; the greatest negative is -33.  According to a response by the CA DOJ, negative
# values appear when agencies “unfound” previously reported crimes. Details are in the response email, but
# the important matter is that they are not mistakes; they are supposed to be negative.

# Note 2:
# In 1 Januray 2013, the FBI broadened its definition of rape.
# Rape data from that year therefore became incomparable with previous data. 
# We decided to strip all the rape data from the calculation of violent and total crimes rates.
# browseURL('https://ucr.fbi.gov/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/violent-crime/rape/rapemain_final.pdf')
df_crimes <- mutate(df_crimes, Violent_nr_sum = Homicide_sum + Robbery_sum + AggAssault_sum)
df_crimes <- mutate(df_crimes, TotalCrime_sum = Violent_nr_sum + Property_sum)



# 2. MERGING DFs ==========================================================================

# Convert factor columns to charachters; otherwise we won't be able to join the dataframes
factor_cols <- sapply(df_crimes, is.factor)
df_crimes[factor_cols] <- lapply(df_crimes[factor_cols], function(x) as.character(x))
rm(factor_cols)

factor_cols <- sapply(df_population, is.factor)
df_population[factor_cols] <- lapply(df_population[factor_cols], function(x) as.character(x))
rm(factor_cols)


# Join me, brother!
df_rates <- left_join(df_crimes, df_population, by=c('County'='county', 'NCICCode'='city', 'Year'='year'))

# Bring the TOTAL_POP column to the front
m <- length(df_rates)
df_rates <- df_rates[c(2:3, 1, m, 4:(m-1))]
rm(m)



# 3. COMPUTING CRIME RATES ==========================================================

# Correcting the name of a column that is misspelled
names(df_rates) <- gsub('[Ss]um', 'sum', names(df_rates))

# Annual chcnges in number of crimes
df_rates <- arrange(df_rates, County, NCICCode, Year)
df_rates <- group_by(df_rates, County, NCICCode)
df_rates <- mutate_at(df_rates, 
                      vars(ends_with('sum')), 
                      funs(chg = . - lag(.,1)))
df_rates <- ungroup(df_rates)

# Annual crime rates
df_rates <- mutate_at(df_rates, 
                      vars(ends_with('sum')), 
                      funs( rate = ./TOTAL_POP*100e3))
names(df_rates) <- gsub('sum_rate', 'rate', names(df_rates))

# Annual chcnges in crime rates
df_rates <- group_by(df_rates, County, NCICCode)
df_rates <- mutate_at(df_rates, 
                      vars(ends_with('rate')), 
                      funs(chg = . - lag(.,1)))
df_rates <- ungroup(df_rates)



# 4. CONSISTENCY TESTS ============================================================

# 4.1. Values -----

stopifnot(length(unique(df_rates$Year)) == 33)
stopifnot(length(unique(df_rates$County)) == 58)


# 4.2. Signs -----

# Are all crime numbers non-negative? (They shouldn't be)
stopifnot(!all(select(df_rates, ends_with('sum')) >= 0, na.rm = TRUE))

# Are crime and rate changes both positive and negative?
stopifnot(!all(select(df_rates, ends_with('chg')) >= 0, na.rm = TRUE))
stopifnot(!all(select(df_rates, ends_with('chg')) <  0, na.rm = TRUE))


# 4.3. Maximums & minimums -----

# Maximum & minimum annual number of crimes
min(select(df_rates, ends_with('sum')), na.rm = TRUE)
max(select(df_rates, ends_with('sum')), na.rm = TRUE) # 344,258 (Aggregate crimes, L.A. Agency, 1991)

# Maximum & minimum annual crime rates
min(select(df_rates, ends_with('rate')), na.rm = TRUE)
max(select(df_rates, ends_with('rate')), na.rm = TRUE) # 1,392,593 (Aggregatge crime rate, Vernon, 1993. Pop: 81)

# Maximum & minimum annual crime changes
min(select(df_rates, ends_with('sum_chg')), na.rm = TRUE) # -34,219 (Aggregate crime drop, L.A. Agency, 1994)
max(select(df_rates, ends_with('sum_chg')), na.rm = TRUE) # 24,736 (Aggregate crime increase, L.A. Agency, 1991)

# Maximum & minimum annual crime rate changes
min(select(df_rates, ends_with('rate_chg')), na.rm = TRUE) # -264,883.9 (Property rate, Vernon, 1996. Pop: 83)
max(select(df_rates, ends_with('rate_chg')), na.rm = TRUE) # 639,851.9 (Aggregatge crime rate chg, Vernon, 1990. Pop: 81)



# 5. EXPORTING RESULTS ===============================================================

write.csv(df_rates, 'output_data/crime_rates_1985_2017_doj_ca_agencies.csv', row.names = FALSE)
