# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Having computed the crime rates by law enforcement agency, we now add data on the number
#           of personnel employed in law enforcement ("LE") and in the Criminal Justice ("CJ") system (pubic
#           defending, prosecution and paroling.)
# Sources:  - Personnel data: CA DOJ
#           - Crime data: FBI UCR
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. SOURCE DATA ==================================================

# 1.1. Importing -----

# Personnel data
# browseURL('https://openjustice.doj.ca.gov/downloads/LE_and_CJ_Personnel_2003-2017.csv')
df.personnel <- read.csv('source_data/personnel/LE_and_CJ_Personnel_2003-2017.csv', header = TRUE, sep = ',')

# Crime Data
df.crime <- read.csv('output_data/crime_rates_1985_2017_doj_ca_counties.csv', header = TRUE, sep = ',')


# 1.2. Cleaning -----

df.crime$County <- as.character(df.crime$County)
df.personnel$COUNTY <- as.character(df.personnel$COUNTY)

df.crime$County <- gsub(' County', '', df.crime$County)
df.personnel$COUNTY <- gsub(' County', '', df.personnel$COUNTY)


# 1.3. Subsetting df.crime -----

names(df.crime)[1:2] <- c('COUNTY', 'YEAR')
df.crime <- select(df.crime, COUNTY, YEAR, TOTAL_POP, Property_sum, Violent_nr_sum, TotalCrime_sum)
df.crime <- filter(df.crime, YEAR >= 2003) # the df.personnel DF only has data since 2003


# 1.4. Consistency tests -----

# Consistency between DFs
stopifnot(nrow(df.crime) == 58*15)
stopifnot(nrow(df.personnel) == 58*15)
stopifnot(all(unique(df.crime$YEAR) == unique(df.personnel$YEAR)))
stopifnot(all(unique(df.crime$COUNTY) == unique(df.personnel$COUNTY)))

# Internal consistency of df.personnel
attach(df.personnel)

stopifnot(sum(ST_TOTAL, na.rm = TRUE) == 
            sum(ST_LE_TOTAL, Prosecution, PublicDefense, ProbationDept, na.rm = TRUE))

stopifnot(sum(ST_LE_TOTAL, na.rm = TRUE) == 
            sum(ST_LE_SWORN, ST_LE_NONSWORN, na.rm = TRUE))
stopifnot(sum(ST_LE_TOTAL, na.rm = TRUE) == 
            sum(SO_FUNDED_TOTAL, PD_FUNDED_TOTAL, CHP_FUNDED_TOTAL, ST_OTH_FUNDED_TOTAL,na.rm = TRUE))
stopifnot(sum(ST_LE_SWORN, na.rm = TRUE) == 
            sum(SO_FUNDED_SWORN, PD_FUNDED_SWORN, CHP_FUNDED_SWORN, ST_OTH_FUNDED_SWORN, na.rm = TRUE))
stopifnot(sum(ST_LE_NONSWORN, na.rm = TRUE) == sum(SO_FUNDED_NONSWORN, PD_FUNDED_NONSWORN, CHP_FUNDED_NONSWORN, ST_OTH_FUNDED_NONSWORN, na.rm = TRUE))

stopifnot(sum(CNTY_TOTAL, na.rm = TRUE) == 
            sum(CNTY_LE_TOTAL, Prosecution, PublicDefense, ProbationDept, na.rm = TRUE))
stopifnot(sum(CNTY_LE_TOTAL, na.rm = TRUE) == 
            sum(CNTY_LE_SWORN, CNTY_LE_NONSWORN, na.rm = TRUE))
stopifnot(sum(CNTY_LE_TOTAL, na.rm = TRUE) == 
            sum(SO_FUNDED_TOTAL, PD_FUNDED_TOTAL, CNTY_OTH_FUNDED_TOTAL, na.rm = TRUE))
stopifnot(sum(CNTY_LE_SWORN, na.rm = TRUE) == 
            sum(SO_FUNDED_SWORN, PD_FUNDED_SWORN, CNTY_OTH_FUNDED_SWORN, na.rm = TRUE))
stopifnot(sum(CNTY_LE_NONSWORN, na.rm = TRUE) == 
            sum(SO_FUNDED_NONSWORN, PD_FUNDED_NONSWORN, CNTY_OTH_FUNDED_NONSWORN, na.rm = TRUE))

stopifnot(sum(SO_FUNDED_TOTAL, na.rm = TRUE) == 
            sum(SO_FUNDED_SWORN, SO_FUNDED_NONSWORN, na.rm = TRUE))
stopifnot(sum(PD_FUNDED_TOTAL, na.rm = TRUE) == 
            sum(PD_FUNDED_SWORN, PD_FUNDED_NONSWORN, na.rm = TRUE))
stopifnot(sum(CNTY_OTH_FUNDED_TOTAL, na.rm = TRUE) == 
            sum(CNTY_OTH_FUNDED_SWORN, CNTY_OTH_FUNDED_NONSWORN, na.rm = TRUE))

stopifnot(sum(Prosecution, na.rm = TRUE) == 
            sum(DA_Attorneys, DA_Investigators, DA_Clerical,	DA_Other, na.rm = TRUE))
stopifnot(sum(PublicDefense, na.rm = TRUE) == 
            sum(PD_Attorneys, PD_Investigators, PD_Clerical,	PD_Other, na.rm = TRUE))
stopifnot(sum(ProbationDept, na.rm = TRUE) == 
            sum(ProbationOfficers, PROB_Other, na.rm = TRUE))


# 1.5. Subsetting df.personnel -----

df.personnel <- select(df.personnel, COUNTY, YEAR, ST_LE_TOTAL, CNTY_LE_TOTAL, Prosecution, PublicDefense, ProbationDept)



# 2 JOINING =====================================================

df.counties <- left_join(df.crime, df.personnel, by = c('COUNTY', 'YEAR'))

# Standardize names
names(df.counties) <- tolower(gsub('_', '.', names(df.counties)))
names(df.counties) <- gsub('.total', '.sum', names(df.counties))
names(df.counties)[9:11] <- paste0(names(df.counties)[9:11], '.sum')

df.state <- group_by(df.counties, year)
df.state <- summarise_at(df.state, vars(total.pop, ends_with('sum')), funs(sum))
df.state <- ungroup(df.state)


# Consistency tests
stopifnot(nrow(df.counties) == 58*15)
stopifnot(all(unique(df.counties$year) %in% unique(df.crime$YEAR)))
stopifnot(all(unique(df.counties$year) %in% unique(df.personnel$YEAR)))
stopifnot(all(unique(df.counties$county) %in% unique(df.crime$COUNTY)))
stopifnot(all(unique(df.counties$county) %in% unique(df.personnel$COUNTY)))

stopifnot(nrow(df.state) == 15)
stopifnot(all(unique(df.state$year) == unique(df.crime$YEAR)))
stopifnot(sum(df.state$total.pop) == sum(df.counties$total.pop))



# 3. COMPUTING RATES ===============================================

df.counties <- mutate_at(df.counties, 
                         vars(7:ncol(df.counties)), 
                         funs(per.crime = ./totalcrime.sum*10e3))
df.state <- mutate_at(df.state, 
                      vars(6:ncol(df.state)), 
                      funs(per.crime = ./totalcrime.sum*10e3))


# 4. EXPORT DATA ==================================================

write.csv(df.state, 'output_data/personnel_rates_LE_&_CJ_2003_2017_CA.csv', row.names = FALSE)
write.csv(df.counties, 'output_data/personnel_rates_LE_&_CJ_2003_2017_by_county.csv', row.names = FALSE)
