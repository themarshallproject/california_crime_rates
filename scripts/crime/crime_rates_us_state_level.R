# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Compute crime rates and crime rate changes in each state
# Sources:  FBI UCR Program
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. SOURCE DATA ==================================================

# 1985~2014
# browseURL('https://www.ucrdatatool.gov/Search/Crime/State/StatebyState.cfm')
df.states <- read.csv("source_data/crime/ucr_crimeStatebyState_1985_2014.csv", header=FALSE)

# 2015
# browseURL('https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/tables/table-2')
df.states.2015 <- read_excel('source_data/crime/ucr_crime_in_the_us_2015_2016.xls', 
                             sheet = '16tbl02', range = 'A4:W203', col_names = TRUE)

# 2016-2017
# browseURL('https://ucr.fbi.gov/crime-in-the-u.s/2017/crime-in-the-u.s.-2017/tables/table-4')
df.states.2016.2017 <- read_excel('source_data/crime/ucr_crime_in_the_us_2016_2017.xls', 
                                  sheet = '17tbl04', range = 'A4:U203', col_names = TRUE)



# 2. CLEANING DATA ==================================================

# 2.1. 1985~2014 -----

df.states <- df.states[4:1940, ]
df.states <- filter(df.states, V1 != '')
rownames(df.states) <- NULL
names(df.states) <- unlist(df.states[2, ])
df.states <- filter(df.states, Year != 'Year')
df.states <- mutate(df.states, state = trimws(gsub('Estimated crime in ', '', df.states$Year)))
df.states <- mutate(df.states, state = gsub('^[0-9]+$', NA, df.states$state))
df.states <- mutate(df.states, state = na.locf(df.states$state))
df.states <- df.states[!grepl('Estimated', df.states$Year), ]
rownames(df.states) <- NULL

# Consistency test
stopifnot(nrow(df.states) == 30*51) # 30 years; 50 states plus D.C.

# Change the order of the columns to bring 'state' to the beginning
m <- ncol(df.states)
df.states <- df.states[c(1, m, 2:(m-1))]
rm(m)

# Transform factor columns into to numeric ones
factor_cols <- sapply(df.states, is.factor)
df.states[factor_cols] <- lapply(df.states[factor_cols], function(x) as.numeric(as.character(x)))
rm(factor_cols)

# Clean column names (eliminate spaces, turn to lowercase, etc.)
names(df.states) <- gsub(' /[12]', '', names(df.states))
names(df.states) <- gsub(' +|-', '.', names(df.states))
names(df.states) <- tolower(names(df.states))
m <- ncol(df.states)
names(df.states)[4:m] <- paste0(names(df.states)[4:m], '.total')
names(df.states) <- gsub('total.total','total', names(df.states))
rm(m)


# 2.2. 2015 -----

df.states.2015 <- select(df.states.2015, -contains('X__'))
df.states.2015 <- filter(df.states.2015, Year == 2015)
df.states.2015$Area <- trimws(gsub('[0-9]+','', df.states.2015$Area))

states <- unique(df.states$state)
df.states.2015 <- filter(df.states.2015, Area %in% states)
row.names(df.states.2015) <- NULL
rm(states)

# Change column order to match the order of the columns in df.states. Then use the col names from df.states
df.states.2015 <- df.states.2015[c(2, 1, 3:5, 7, 6, 8:13 )]
names(df.states.2015) <- names(df.states)[1:13]

# Change columns from character to numbers
df.states.2015 <- mutate_at(df.states.2015, 
                            vars(c(1, 3:13)), 
                            funs(as.numeric(.)))


# 2.3. 2016~2017 -----

df.states.2016.2017 <- select(df.states.2016.2017, -contains('X__'))
df.states.2016.2017 <- filter(df.states.2016.2017, Year %in% c(2016, 2017))
df.states.2016.2017$Area <- na.locf(df.states.2016.2017$Area)

df.states.2016.2017$Area <- trimws(gsub('[0-9]','', df.states.2016.2017$Area))

states <- unique(df.states$state)
df.states.2016.2017 <- filter(df.states.2016.2017, Area %in% states)
row.names(df.states.2016.2017) <- NULL
rm(states)

# The 2016~2017 data don't include legacy rape anymore. Because df.states does, we make a dummy column here.
df.states.2016.2017 <- mutate(df.states.2016.2017, legacy.rape = NA)

# Change column order to match the order of the columns in df.states. Then use the col names from df.states
df.states.2016.2017 <- df.states.2016.2017[c(2, 1, 3:5, 13, 6:12 )]
names(df.states.2016.2017) <- names(df.states)[1:13]

# Change columns from character to numbers
df.states.2016.2017 <- mutate_at(df.states.2016.2017, 
                                 vars(c(1, 3:13)), 
                                 funs(as.numeric(.)))



# 3. MERGING ======================================================

# Consistency test: confirm both dataframes have the same columns
stopifnot(all(names(df.states) == names(df.states.2015)))
stopifnot(all(names(df.states.2015) == names(df.states.2016.2017)))

# Confirm they both contain all the states
stopifnot(all(df.states$state %in% df.states.2015$state))
stopifnot(all(df.states.2015$state %in% df.states.2016.2017$state))
stopifnot(all(df.states.2016.2017$state %in% df.states$state))

# Merge!
df.states <- rbind(df.states, df.states.2015, df.states.2016.2017)
df.states <- arrange(df.states, state, year)

rownames(df.states) <- NULL

rm(df.states.2015, df.states.2016.2017)

# In 1 Januray 2013, the FBI made broadened its definition of rape.
# Rape data from that year therefore became incomparable with previous data. 
# We decided to strip all the rape data from the calculation of violent and total crimes rates.
# browseURL('https://ucr.fbi.gov/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/violent-crime/rape/rapemain_final.pdf')

# Comute a measure of violent crime that does not include rape
df.states <- mutate(df.states, violent.nr.crime.total = 
                      murder.and.nonnegligent.manslaughter.total + aggravated.assault.total + robbery.total)

# Add property crime + no-rape violentcrime to create the no-rape measure of total crime
df.states <- mutate(df.states, aggregate.crime.total = property.crime.total + violent.nr.crime.total)



# 4. CALCULATE CRIME RATES ========================================

# Before calculating the rates, create a copy of df.states; we will use it to aggregate to national level
df.us <- df.states # All states
df.us49 <- filter(df.states, state != 'California') # Ex-California

# Compute crime rates
m <- ncol(df.states)
df.states <- mutate_at(df.states, 
                       vars(4:m), 
                       funs(rate = ./population*100e3))
names(df.states) <- gsub('total_rate', 'rate', names(df.states))
rm(m)

# Calculate the changes in crime rates each year, for each state
df.states <- group_by(df.states, state)
df.states <- mutate_at(df.states, 
                       vars(contains('rate')), 
                       funs(chg = . - lag(.,1)))
df.states <- ungroup(df.states)
names(df.states) <- gsub('_', '.', names(df.states))

# DF for California only
df.ca <- filter(df.states, state=='California')



# 5. GENERATE NATIONAL LEVEL DATA =================================

# 5.1. All states -----

m <- ncol(df.us)

df.us <- group_by(df.us, year)
# Aggregate across states (column 'state' will disappear)
df.us <- summarise_at(df.us, vars(3:m-1), funs(sum)) # Last col is m-1 because the df is grouped
df.us <- ungroup(df.us)
# Calculate rates
df.us <- mutate_at(df.us, vars(3:m-1), funs(rate =./population*100e3))
rm(m)

names(df.us) <- gsub('total_rate', 'rate', names(df.us))

# Calculate rate changes
df.us <- mutate_at(df.us, 
                   vars(contains('rate')), 
                   funs(chg = . - lag(.,1)))
names(df.us) <- gsub('_', '.', names(df.us))

# Reintroduce 'state' column and sort columns with the same order as df.states & df.ca
df.us$state <- 'All'
df.us <- df.us[, names(df.states)]


# 5.2. Ex-California -----

m <- ncol(df.us49)

df.us49 <- group_by(df.us49, year)
# Aggregate across states (column 'state' will disappear). Last col is m-1 because the DF is grouped.
df.us49 <- summarise_at(df.us49, 
                        vars(3:m-1), 
                        funs(sum)) 
df.us49 <- ungroup(df.us49)

# Calculate rates
df.us49 <- mutate_at(df.us49, 
                     vars(3:m-1), 
                     funs(rate =./population*100e3))
rm(m)

names(df.us49) <- gsub('total_rate', 'rate', names(df.us49))

# Calculate rate changes
df.us49 <- mutate_at(df.us49, 
                     vars(contains('rate')), 
                     funs(chg = . - lag(.,1)))
names(df.us49) <- gsub('_', '.', names(df.us49))

# Reintroduce 'state' column and sort columns with the same order as df.states & df.ca
df.us49$state <- 'Ex-California'
df.us49 <- df.us49[, names(df.states)]



# 6.CONSISTENCY CHECKS ===============================================

# 6.1. Dimensions -----

# Confirm the number of rows for the different dataframes are consistent
# with the fact that we have 32 years (1985-2016) and 50 states + D.C.

# df.us
stopifnot(length(unique(df.us$year)) == 33)
stopifnot(nrow(df.us) == 33)
stopifnot(nrow(df.us49) == 33)

# df.states
stopifnot(length(unique(df.states$state)) == 51)
stopifnot(length(unique(df.states$year)) == 33)
stopifnot(nrow(df.states) == 51*33)

stopifnot(length(unique(df.states$state)) == 51)
stopifnot(length(unique(df.states$year)) == 33)
stopifnot(nrow(df.states) == 51*33)

# df.ca
stopifnot(length(unique(df.ca$state)) == 1)
stopifnot(length(unique(df.ca$year)) == 33)
stopifnot(nrow(df.ca) == 1*33)

# column names
stopifnot(all(names(df.ca) == names(df.states)))
stopifnot(all(names(df.states) == names(df.us)))
stopifnot(all(names(df.us) == names(df.us49)))
stopifnot(all(names(df.us49) == names(df.ca)))


# 6.2. Signs -----

# Are all crime numbers and crime rates non-negative?

# Each state
stopifnot(all(select(df.states, ends_with('total')) >= 0, na.rm = TRUE))
stopifnot(all(select(df.states, ends_with('rate')) >= 0, na.rm = TRUE))
# All U.S.
stopifnot(all(select(df.us, ends_with('total')) >= 0, na.rm = TRUE))
stopifnot(all(select(df.us, ends_with('rate')) >= 0, na.rm = TRUE))
# U.S. Ex-CA
stopifnot(all(select(df.us49, ends_with('total')) >= 0, na.rm = TRUE))
stopifnot(all(select(df.us49, ends_with('rate')) >= 0, na.rm = TRUE))
# California
stopifnot(all(select(df.ca, ends_with('total')) >= 0, na.rm = TRUE))
stopifnot(all(select(df.ca, ends_with('rate')) >= 0, na.rm = TRUE))


# Are changes both positive and negative?

# Each states
stopifnot(!all(select(df.states, ends_with('chg')) >= 0, na.rm = TRUE))
stopifnot(!all(select(df.states, ends_with('chg')) < 0, na.rm = TRUE))
# All U.S.
stopifnot(!all(select(df.us, ends_with('chg')) >= 0, na.rm = TRUE))
stopifnot(!all(select(df.us, ends_with('chg')) < 0, na.rm = TRUE))
# U.S. Ex-CA
stopifnot(!all(select(df.us49, ends_with('chg')) >= 0, na.rm = TRUE))
stopifnot(!all(select(df.us49, ends_with('chg')) < 0, na.rm = TRUE))
# California
stopifnot(!all(select(df.ca, ends_with('chg')) >= 0, na.rm = TRUE))
stopifnot(!all(select(df.ca, ends_with('chg')) < 0, na.rm = TRUE))


# 6.3. Maximums & minimums -----

# Maximum & minimum annual number of crimes

# Each state
min(select(df.states, ends_with('total')), na.rm = TRUE)
max(select(df.states, ends_with('total')), na.rm = TRUE) # 2,049,000 crimes commited in CA in 1992
# All U.S.
min(select(df.us, ends_with('total')), na.rm = TRUE)
max(select(df.us, ends_with('total')), na.rm = TRUE) # 14,766,290 crimes commited in the U.S. in 1991
# U.S. Ex-CA
min(select(df.us49, ends_with('total')), na.rm = TRUE)
max(select(df.us49, ends_with('total')), na.rm = TRUE) # 12,721,673 crimes commited in the ex-CA 49 states in 1991
# California
min(select(df.ca, ends_with('total')), na.rm = TRUE)
max(select(df.ca, ends_with('total')), na.rm = TRUE) # 2,049,000 crimes commited in CA in 1992


# 6.4. Other -----

# Confirm that the year 1985 contains only NA values for in the dataframes where
# we calculated the crime rates.
stopifnot(all(is.na(df.us[df.us$year == 1985, grepl('chg', names(df.us))])))
stopifnot(all(is.na(df.us49[df.us49$year == 1985, grepl('chg', names(df.us))])))
stopifnot(all(is.na(df.ca[df.ca$year == 1985, grepl('chg', names(df.ca))])))
stopifnot(all(is.na(df.states[df.states$year == 1985, grepl('chg', names(df.states))])))



# 7. EXPORT DATA =======================================================

write.csv(df.us, 'output_data/crime_rates_1985_2017_ucr_us_whole.csv', row.names = FALSE)
write.csv(df.us49, 'output_data/crime_rates_1985_2017_ucr_us_exCA.csv', row.names = FALSE)
write.csv(df.states, 'output_data/crime_rates_1985_2017_ucr_us_states.csv', row.names = FALSE)
write.csv(df.ca, 'output_data/crime_rates_1985_2017_ucr_ca_state.csv', row.names = FALSE)
