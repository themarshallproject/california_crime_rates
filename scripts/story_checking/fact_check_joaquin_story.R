# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Fact-check all the numbers quoted in the San Joaquin story.
# Sources:  Output files from the following scripts:
#           - crime_rates_ca_counties.R
#           - crime_rates_ca_agencies.R
#           - personnel_rates.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# "How One County Became a Lab for California’s Prison Reform"
browseURL('https://www.themarshallproject.org/2018/12/21/how-one-county-became-a-lab-for-california-s-prison-reform')



# I. Data =========================================================================

# I.1. Importing -----

# Crime 
df.crime.california <- read.csv('output_data/crime_rates_1985_2017_doj_ca_state.csv')
df.crime.counties <- read.csv('output_data/crime_rates_1985_2017_doj_ca_counties.csv')
df.crime.agencies <- read.csv('output_data/crime_rates_1985_2017_doj_ca_agencies.csv')

# Personnel
df.personnel.counties <- read.csv('output_data/personnel_rates_LE_&_CJ_2003_2017_by_county.csv')


# I.2. Subsetting -----

# San Joaquin
df.crime.joaquin <- filter(df.crime.counties, County == 'San Joaquin County')
df.personnel.joaquin <- filter(df.personnel.counties, county == 'San Joaquin')

# Stockton
df.crime.stockton <- filter(df.crime.agencies, NCICCode == 'Stockton')



# II. Story fact-checking =============================================================

# "There were already signs that a recent dip in crime might be coming to an end in this Central 
# Valley county east of San Francisco. Homicides were up by nearly 40 percent from the previous year."

df.plot.data <- select(df.crime.joaquin, Year, Homicide_sum, Homicide_rate)
df.plot.data <- mutate_at(df.plot.data, 
                          vars(starts_with('Homicide')), 
                          funs(pct_chg = ./lag(.,1)-1))
df.plot.data <- filter(df.plot.data, Year > 2000)

filter(select(df.plot.data, Year, Homicide_sum_pct_chg, Homicide_rate_pct_chg), Year == 2011)


png('plots/Crime_Sums_2000_2017_Joaquin_homicide.png', height = 600, width = 1200)
g <- ggplot(df.plot.data, aes(x = Year, y = Homicide_sum)) +
  geom_line() +
  geom_point() +
  # Font size
  theme(text=element_text(size=20)) +
  # Labels
  geom_text(aes(label = comma(round(Homicide_sum))),
            size = 4, vjust = -2) +
  labs(title = 'Homicides - San Joaquin',
       caption = 'Source: CA DOJ',
       x = 'Year',
       y = 'Number of Homicides') +
  scale_y_continuous(labels = scales::comma) +
  # Background
  theme(panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid.major = element_line(colour = 'gray91')) +
  coord_cartesian(ylim = c(0.95*min(df.plot.data$Homicide_sum),
                           1.05*max(df.plot.data$Homicide_sum)))
print(g)
dev.off()


png('plots/Crime_Rates_2000_2017_Joaquin_homicide.png', height = 600, width = 1200)
g <- ggplot(df.plot.data, aes(x = Year, y = Homicide_rate)) +
  geom_line() +
  geom_point() +
  # Font size
  theme(text=element_text(size=20)) +
  # Labels
  geom_text(aes(label = comma(round(Homicide_rate))),
            size = 4, vjust = -2) +
  labs(title = 'Homicide Rates - San Joaquin',
       caption = 'Source: CA DOJ',
       x = 'Year',
       y = 'Homicides per 100K') +
  scale_y_continuous(labels = scales::comma) +
  # Background
  theme(panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid.major = element_line(colour = 'gray91')) +
  coord_cartesian(ylim = c(0.95*min(df.plot.data$Homicide_rate),
                           1.05*max(df.plot.data$Homicide_rate)))
print(g)
dev.off()



# "And in the midst of a financial crisis, the county and local cities were laying off police and prosecutors."

rm(df.plot.data)
df.plot.data <- select(df.personnel.joaquin, 
                       year, cnty.le.sum, prosecution.sum, publicdefense.sum, probationdept.sum)
df.plot.data <- filter(df.plot.data, year <= 2011)
names(df.plot.data) <- c('year', 'law.enforcement', 'prosecution', 'public.defense', 'probation')
df.plot.data <- gather(df.plot.data, 'department', 'total.personnel', -year)


png('plots/Crime_Rates_2003_2011_Joaquin_personnel.png', height = 600, width = 1200)
g <- ggplot(df.plot.data, aes(x = year, y = total.personnel, colour = department)) +
  geom_line() +
  geom_point() +
  # Font size
  theme(text=element_text(size=20)) +
  # Labels
  geom_text(aes(label = comma(round(total.personnel))),
            size = 4, vjust = -2) +
  labs(title = 'Personnel - San Joaquin',
       caption = 'Source: CA DOJ',
       x = 'Year',
       y = 'Total Employees') +
  scale_y_continuous(labels = scales::comma) +
  # Background
  theme(panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid.major = element_line(colour = 'gray91')) +
  coord_cartesian(ylim = c(0.95*min(df.plot.data$total.personnel),
                           1.05*max(df.plot.data$total.personnel)))
print(g)
dev.off()



# "While overall crime in California increased slightly after 2011, San Joaquin 
# County’s dropped 20 percent..."

df.temp1 <- select(df.crime.california, Year, TotalCrime_sum)
df.temp2 <- select(df.crime.joaquin, Year, TotalCrime_sum)

df.temp1 <- mutate(df.temp1, region = 'California')
df.temp2 <- mutate(df.temp2, region = 'San Joaquin')

df.temp <- rbind(df.temp1, df.temp2)
rm(df.temp1, df.temp2)

df.temp <- filter(df.temp, Year %in% c(2011, 2017))
df.temp <- arrange(df.temp, region, Year)
df.temp <- group_by(df.temp, region)
df.temp <- mutate_at(df.temp,
                     vars(starts_with('TotalCrime')),
                     funs(pct.chg = ./lag(.,1)-1))
df.temp



# "... and hit a decades-old low last year."

rm(df.plot.data)
df.plot.data <- select(df.crime.joaquin, Year, TotalCrime_sum)

png('plots/Crime_Sums_1985_2017_Joaquin_overall.png', height = 600, width = 1200)
g <- ggplot(df.plot.data, aes(x = Year, y = TotalCrime_sum)) +
  geom_line() +
  geom_point() +
  # Font size
  theme(text=element_text(size=20)) +
  # Labels
  geom_text(aes(label = comma(round(TotalCrime_sum))),
            size = 4, vjust = -2) +
  labs(title = 'Overall Crime - San Joaquin',
       caption = 'Source: CA DOJ',
       x = 'Year',
       y = 'Number of Crimes') +
  scale_y_continuous(labels = scales::comma) +
  # Background
  theme(panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid.major = element_line(colour = 'gray91')) +
  coord_cartesian(ylim = c(0.95*min(df.plot.data$TotalCrime_sum),
                           1.05*max(df.plot.data$TotalCrime_sum)))
print(g)
dev.off()


# "San Joaquin County officials acknowledge there is more to do. Although the county’s crime rate has 
# almost halved since the mid-2000s..."

rm(df.temp)
df.temp <- select(df.crime.joaquin, Year, TotalCrime_rate)
df.temp <- filter(df.temp, Year >= 2000)
df.temp <- filter(df.temp, TotalCrime_rate == max(TotalCrime_rate) | TotalCrime_rate == min(TotalCrime_rate))
df.temp <- arrange(df.temp, Year)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('rate')),
                     funs(pct.chg = ./lag(.,1)-1))
df.temp


# ... it remains among the highest in the state."

rm(df.temp)
df.temp <- filter(df.crime.counties, Year == 2017)
df.temp <- select(df.temp, Year, County, TOTAL_POP, TotalCrime_rate)
head(arrange(df.temp, -TotalCrime_rate)) # Number 6 by total crime rate



# "And most of that reduction was in property crime, not violent offenses..."

rm(df.temp)
df.temp <- select(df.crime.joaquin, Year, TotalCrime_rate, Property_rate, Violent_nr_rate)
df.temp <- filter(df.temp, Year >= 2000)
df.temp <- filter(df.temp, TotalCrime_rate == max(TotalCrime_rate) | TotalCrime_rate == min(TotalCrime_rate))
df.temp <- arrange(df.temp, Year)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('rate')),
                     funs(chg = .-lag(.,1)))
df.temp <- select(df.temp, Year, ends_with('chg'))
df.temp <- mutate_at(df.temp, 
                     vars(ends_with('chg')),
                     funs(contrib = ./TotalCrime_rate_chg))
df.temp



# "... which increased in the last few years before dipping slightly in 2017."

rm(df.plot.data)
df.plot.data <- select(df.crime.joaquin, Year, Violent_nr_rate)
df.plot.data <- filter(df.plot.data, Year >= 2010)

png('plots/Crime_Rates_2010_2017_Joaquin_violent.png', height = 600, width = 1200)
g <- ggplot(df.plot.data, aes(x = Year, y = Violent_nr_rate)) +
  geom_line(colour = 'firebrick') +
  geom_point(colour = 'firebrick') +
  # Font size
  theme(text=element_text(size=20)) +
  # Labels
  geom_text(aes(label = comma(round(Violent_nr_rate))),
            size = 4, vjust = -2) +
  labs(title = 'Violent Crime Rates - San Joaquin',
       caption = 'Source: CA DOJ',
       x = 'Year',
       y = 'Crimes per 100K People') +
  scale_y_continuous(labels = scales::comma) +
  # Background
  theme(panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid.major = element_line(colour = 'gray91')) +
  coord_cartesian(ylim = c(0.95*min(df.plot.data$Violent_nr_rate),
                           1.05*max(df.plot.data$Violent_nr_rate)))
print(g)
dev.off()



# "To her relief, the county’s homicides fell by nearly 50 percent in 2013."

rm(df.temp)
df.temp <- select(df.crime.joaquin, Year, Homicide_sum, Homicide_rate)
df.temp <- mutate_at(df.temp, 
                     vars(starts_with('Homicide')), 
                     funs(pct_chg = ./lag(.,1)-1))
df.temp <- filter(df.temp, Year == 2013)
df.temp



# "Jones has also been able to increase his department’s ranks – as have other local police agencies. 
# After dipping to a low in 2011 of 866 sworn officers, the county added nearly 150 more officers by 2017. " 

rm(df.temp)
df.temp <- select(df.personnel.joaquin, year, cnty.le.sum)
df.temp <- filter(df.temp, year %in% c(2011, 2017))
df.temp <- arrange(df.temp, year)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('sum')),
                     funs(chg = . - lag(.,1)))
df.temp
# Note: this total increase of 204  the total increase in officers (sworn + civilians)

