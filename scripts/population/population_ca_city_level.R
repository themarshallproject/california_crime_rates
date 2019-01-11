# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Compute CA population down to city level
# Sources:  browseURL(http://www.dof.ca.gov/Forecasting/Demographics/Estimates/)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. 1980s ==============================

df_pop80s <- read.csv('source_data/population/90e-4.csv', na.strings = c('', NA))

names(df_pop80s) <- c('city', '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989')

df_pop80s <- df_pop80s[94:785, ]
df_pop80s <- df_pop80s[!(rowSums(is.na(df_pop80s)) == length(df_pop80s)), ]
df_pop80s <- mutate(df_pop80s, county = city)
df_pop80s[!grepl('^[^a-z]+$', df_pop80s$county), c('county')] <- NA
df_pop80s <- mutate(df_pop80s, county = na.locf(county))
df_pop80s <- mutate(df_pop80s, county = paste(county, 'County'))
df_pop80s <- df_pop80s[!grepl('^[^a-z]+$', df_pop80s$city), ]

# melt the dataframe
df_pop80s <- gather(df_pop80s, year, TOTAL_POP, -c(county, city))

# convert factor columns into character columns
factor_cols <- sapply(df_pop80s, is.factor)
df_pop80s[factor_cols] <- lapply(df_pop80s[factor_cols], function(x) as.character(x))

# convert all the county names to lower case and then capitalize
df_pop80s$county <- str_to_title(tolower(df_pop80s$county))

# eliminate unwanted white spaces in 'county' & 'city'
df_pop80s$county <- gsub('\\s+', ' ', df_pop80s$county)
df_pop80s$city <- gsub('^\\s+|\\s{2,}|\\s+$', '', df_pop80s$city)

# convert 'year' and 'TOTAL_POP' to numeric
df_pop80s$TOTAL_POP <- gsub(",", "", df_pop80s$TOTAL_POP)
df_pop80s$TOTAL_POP <- as.numeric(df_pop80s$TOTAL_POP)
df_pop80s$year <- as.numeric(df_pop80s$year)


# 2. 1990s ==============================

df_pop90s <- read.csv('source_data/population/E-4_90-00_Rpt.csv', na.strings = c('', NA))

names(df_pop90s) <- c('city', 'X', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999X', '1999')
df_pop90s <- df_pop90s[10:655, !(names(df_pop90s) %in% c('X', '1999X'))]
df_pop90s <- df_pop90s[!(rowSums(is.na(df_pop90s)) == length(df_pop90s)), ]
df_pop90s <- df_pop90s[!grepl('balance of county', tolower(df_pop90s$city)), ]

# create a column that shows each city's county
df_pop90s <- mutate(df_pop90s, county = city)
df_pop90s[rowSums(is.na(df_pop90s)) < length(df_pop90s)-2, c('county')] <- NA
df_pop90s <- mutate(df_pop90s, county = na.locf(county))
df_pop90s <- mutate(df_pop90s, county = paste(county, 'County'))

df_pop90s <- df_pop90s[!(rowSums(is.na(df_pop90s)) == length(df_pop90s)-2), ]

rownames(df_pop90s) <- NULL

# melt the dataframe
df_pop90s <- gather(df_pop90s, year, TOTAL_POP, -c(county, city))

# convert factor columns into character columns
factor_cols <- sapply(df_pop90s, is.factor)
df_pop90s[factor_cols] <- lapply(df_pop90s[factor_cols], function(x) as.character(x))

# remove unwanted white spaces in 'city'
df_pop90s$city <- gsub("^\\s+|\\s{2,}|\\s+$", "", df_pop90s$city)

# convert 'TOTAL_POP' and 'year' to numeric
df_pop90s$TOTAL_POP <- gsub(",", "", df_pop90s$TOTAL_POP)
df_pop90s$TOTAL_POP <- as.numeric(df_pop90s$TOTAL_POP)
df_pop90s$year <- as.numeric(df_pop90s$year)


# 3. 2000s ==========================

df_pop00s <- read.csv('source_data/population/E4_2000-2010_Report_Final_EOC_000.csv', na.strings = c('', NA))

names(df_pop00s) <- c('city', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009X', '2009')
df_pop00s <- df_pop00s[10:766, !(names(df_pop00s) %in% c('X', '1999', '2009X'))]
df_pop00s <- df_pop00s[!(rowSums(is.na(df_pop00s)) == length(df_pop00s)), ]
df_pop00s <- df_pop00s[!grepl('county total|balance of county', tolower(df_pop00s$city)), ]

# create a column that shows each city's county
df_pop00s <- mutate(df_pop00s, county = city)
df_pop00s[!grepl('county', tolower(df_pop00s$county)), c('county')] <- NA
df_pop00s <- mutate(df_pop00s, county = na.locf(county))

df_pop00s <- df_pop00s[!(rowSums(is.na(df_pop00s)) == length(df_pop00s)-2), ]

rownames(df_pop00s) <- NULL

# melt the dataframe
df_pop00s <- gather(df_pop00s, year, TOTAL_POP, -c(county, city))

# convert factor columns into character columns
factor_cols <- sapply(df_pop00s, is.factor)
df_pop00s[factor_cols] <- lapply(df_pop00s[factor_cols], function(x) as.character(x))

# eliminate unwanted white spaces
df_pop00s$county <- gsub("^\\s+|\\s{2,}|\\s+$", "", df_pop00s$county)
df_pop00s$city <- gsub("^\\s+|\\s{2,}|\\s+$", "", df_pop00s$city)

# convert 'TOTAL_POP' and 'year' to numeric
df_pop00s$TOTAL_POP <- gsub(",", "", df_pop00s$TOTAL_POP)
df_pop00s$TOTAL_POP <- as.numeric(df_pop00s$TOTAL_POP)
df_pop00s$year <- as.numeric(df_pop00s$year)


# 4. 2010s ==========================

df_pop10s <- read.csv('source_data/population/E-4_2018InternetVersion.csv')

names(df_pop10s) <- c('city', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017')
df_pop10s <- df_pop10s[10:771, !(names(df_pop10s) %in% c('2009'))]
df_pop10s <- df_pop10s[df_pop10s$city != '' , ]
df_pop10s <-df_pop10s[!grepl('county total|balance of county', tolower(df_pop10s$city)), ]

# create a column that shows each city's county
df_pop10s <- mutate(df_pop10s, county = city)
df_pop10s[!grepl('county', tolower(df_pop10s$county)), c('county')] <- NA
df_pop10s <- mutate(df_pop10s, county = na.locf(county))

df_pop10s <- df_pop10s[!grepl('county', tolower(df_pop10s$city)), ]

rownames(df_pop10s) <- NULL

# melt the dataframe
df_pop10s <- gather(df_pop10s, year, TOTAL_POP, -c(county, city))

# convert factor columns into character columns
factor_cols <- sapply(df_pop10s, is.factor)
df_pop10s[factor_cols] <- lapply(df_pop10s[factor_cols], function(x) as.character(x))

df_pop10s$county <- gsub("^\\s+|\\s{2,}|\\s+$", "", df_pop10s$county)
df_pop10s$city <- gsub("^\\s+|\\s{2,}|\\s+$", "", df_pop10s$city)

df_pop10s$TOTAL_POP <- gsub(",", "", df_pop10s$TOTAL_POP)
df_pop10s$year <- as.numeric(df_pop10s$year)
df_pop10s$TOTAL_POP <- as.numeric(df_pop10s$TOTAL_POP)


# 5. Bind all DFs ==============================

df_population <- rbind(df_pop80s, df_pop90s, df_pop00s, df_pop10s)


# 6. Cleaning =================================

# correct city name typos
df_population[grepl('flintridge', tolower(df_population$city)), c('city')] <- 'La Canada-Flintridge'
df_population[grepl('angels city', tolower(df_population$city)), c('city')] <- 'Angels Camp'
df_population[grepl('grover city', tolower(df_population$city)), c('city')] <- 'Grover Beach'
df_population[grepl('murietta', tolower(df_population$city)), c('city')] <- 'Murrieta'
df_population[grepl('mcfarland', tolower(df_population$city)), c('city')] <- 'McFarland'

# order
df_population <- arrange(df_population, county, city, year)


# 6. Export ==============================

write.csv(df_population, 'output_data/population_1980_2017_census_ca_cities.csv', row.names = FALSE)
