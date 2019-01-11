# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Compute CA population down to county level
# Sources:  browseURL('https://www2.census.gov/programs-surveys/popest/tables/')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# I. IMPORT SOURCE DATA ================================================

# 1. The 1980s (By far the best decade of pop rock) -----
# browseURL('https://www2.census.gov/programs-surveys/popest/tables/1980-1990/counties/totals/e8089co.txt')

column_widths = c(5,17,9,10,10,10,10)

# 1.1. 1980-1984
column_names = c('Code','Name', 'X1980', 'X1981', 'X1982', 'X1983', 'X1984')
df_pop8084 <- read_fwf('source_data/population/e8089co.txt', skip=458, n_max=62, fwf_widths(column_widths, col_names=column_names))

#clean up the counties that oddly break across two lines
df_pop8084 <- df_pop8084 %>% 
  mutate(NEW_NAME = ifelse(X1980=="895016", "San Bernardino Co.", 
                           ifelse(X1980=="678974", "San Francisco Co.", 
                                  ifelse(X1980=="155435", "San Luis Obispo Co.",
                                         ifelse(X1980=="298694", "Santa Barbara Co.", Name))))) %>% 
  mutate(COUNTY = gsub("Co.$", "County", NEW_NAME)) %>% #format the name to match other population records
  select(COUNTY, X1980, X1981, X1982, X1983, X1984, X1984) %>% 
  filter(!is.na(X1980)) %>% #filter out the lines missing data
  gather(TOTAL_POP, key=YEAR, -COUNTY) %>% 
  mutate(YEAR = str_sub(YEAR, 2, str_length(YEAR))) %>% 
  mutate(YEAR = as.numeric(YEAR))


# 1.2. 1985-1989
column_names = c('Code','Name', 'X1985', 'X1986', 'X1987', 'X1988', 'X1989')
df_pop8589 <- read_fwf('source_data/population/e8089co.txt', skip=524, n_max=62, fwf_widths(column_widths, col_names=column_names))

#clean up the counties that oddly break across two lines
df_pop8589 <- df_pop8589 %>% 
  mutate(NEW_NAME = ifelse(X1985=="1072242", "San Bernardino Co.", 
                           ifelse(X1985=="727977", "San Francisco Co.", 
                                  ifelse(X1985=="185248", "San Luis Obispo Co.",
                                         ifelse(X1985=="338569", "Santa Barbara Co.", Name))))) %>% 
  mutate(COUNTY = gsub("Co.$", "County", NEW_NAME)) %>% #format the name to match other population records
  select(COUNTY, X1985, X1986, X1987, X1988, X1989, X1989) %>% 
  filter(!is.na(X1985)) %>% #filter out the lines missing data
  gather(TOTAL_POP, key=YEAR, -COUNTY) %>% 
  mutate(YEAR = str_sub(YEAR, 2, str_length(YEAR))) %>% 
  mutate(YEAR = as.numeric(YEAR))

# Bind the 80s
df_pop80s <- rbind(df_pop8084, df_pop8589)
rm(df_pop8084, df_pop8589)


# 2. The 1990s -----
# browseURL('https://www2.census.gov/programs-surveys/popest/tables/1990-2000/estimates-and-change-1990-2000/2000c8_06.txt')

column_widths = c(2,3,6,11,12,12,12,12,12,12,12,12,12,12,11,35)
column_names = c('Block', 'State', 'County', 'X2000', 'X1999', 'X1998', 'X1997', 'X1996', 'X1995', 'X1994', 'X1993', 'X1992', 'X1991', 'X1990est', 'X1990','Name')

df_pop90s <- read_fwf('source_data/population/2000c8_06.txt', skip=20, n_max=58, fwf_widths(column_widths, col_names=column_names))

df_pop90s <- df_pop90s %>% 
  select(Name, starts_with('X'), -X1990est) %>% 
  gather(TOTAL_POP, key=YEAR, -Name) %>% 
  mutate(YEAR = str_sub(YEAR, 2, str_length(YEAR))) %>% 
  rename(COUNTY=Name) %>% 
  filter(YEAR != 2000)


# 3. The 2000s -----
# browseURL('https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-06.csv')

pops00_colnames = c('County', 'Census00', '2000_estimate', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', 'Census10', '2013')
df_pop00s <- read.csv('source_data/population/co-est00int-01-06.csv', skip=4, col.names = pops00_colnames, nrows=58)

df_pop00s <- df_pop00s %>% 
  mutate(County_name = str_sub(County, 2, str_length(County))) %>% 
  select(County_name, Census00, starts_with('X'), Census10, -X2013, -X2000_estimate) %>%
  rename(X2000 = Census00, X2010 = Census10, COUNTY = County_name) %>% 
  gather(TOTAL_POP, key=YEAR, -COUNTY) %>% 
  mutate(YEAR = str_sub(YEAR, 2, str_length(YEAR))) %>% 
  mutate(TOTAL_POP = as.numeric(gsub(",", "", TOTAL_POP)), YEAR = as.numeric(YEAR))


# 4. The 2010s -----
# browseURL('https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/counties/asrh/')
df_pop10s <-read.csv('source_data/population/cc-est2017-alldata-06.csv') %>% 
  filter(AGEGRP == 0) %>% 
  select(COUNTY, STNAME, CTYNAME, YEAR, AGEGRP, TOT_POP)

df_pop10s <- df_pop10s %>% filter(YEAR>3) %>% 
  mutate(YEAR_CLEAN = ifelse(YEAR==4, 2011,
                             ifelse(YEAR==5, 2012,
                                    ifelse(YEAR==6,2013,
                                           ifelse(YEAR==7, 2014,
                                                  ifelse(YEAR==8, 2015,
                                                         ifelse(YEAR==9, 2016,
                                                                ifelse(YEAR==10, 2017, 0)))))))) %>% 
  select(CTYNAME, YEAR_CLEAN, TOT_POP) %>% 
  rename(COUNTY=CTYNAME, YEAR=YEAR_CLEAN, TOTAL_POP=TOT_POP)



# II. BIND =======================================================

df_population <- 
  rbind(df_pop80s, df_pop90s, df_pop00s, df_pop10s) %>% 
  arrange(COUNTY, YEAR) %>% 
  mutate(YEAR = as.numeric(YEAR))

rm(df_pop80s, df_pop90s, df_pop00s, df_pop10s)
rm(column_names, column_widths, pops00_colnames)



# III. CONSISTENCY CHECK =======================================================

stopifnot(length(unique(df_population$YEAR)) == 38) # 38 years (1980-2017)
stopifnot(length(unique(df_population$COUNTY)) == 58) # 58 counties

stopifnot(all(table(df_population$YEAR) == 58)) # Each year appears 58 times; one per county
stopifnot(all(table(df_population$COUNTY) == 38)) # Each county appears 37 times; one per year

stopifnot(nrow(df_population) == 38*58)



# IV. EXPORT ====================================================================

write.csv(df_population, 'output_data/population_1980_2017_census_ca_counties.csv', row.names = FALSE)
