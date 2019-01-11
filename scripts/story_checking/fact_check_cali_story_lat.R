# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:   Manuel Villa
# Task:     Fact-check all the statistics quoted in the LAT California story:
# Sources:  Output files from the following scripts:
#           - Crime_Rates_ca_counties.R
#           - Crime_Rates_all_states.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# "California transformed its justice system. But now crime is up, and critics want rollbacks"
browseURL('https://www.latimes.com/local/lanow/la-me-california-reforms-crime-20181220-story.html')



# I. Data =========================================================================

df.ucr.us <- read.csv('output_data/Crime_Rates_1985_2017_ucr_us_states.csv')
df.ucr.us49 <- read.csv('output_data/Crime_Rates_1985_2017_ucr_us_exCA.csv')
df.ucr.ca <- read.csv('output_data/Crime_Rates_1985_2017_ucr_ca_state.csv')
df.doj.ca <- read.csv('output_data/Crime_Rates_1985_2017_doj_ca_state.csv')
df.doj.counties <- read.csv('output_data/Crime_Rates_1985_2017_doj_ca_counties.csv')



# II. Story fact-checking =============================================================


# "An analysis by the Marshall Project and the Los Angeles Times found that California’s crime rates 
# remain near historic lows, but overall crime spiked in both 2012 and 2015,"

df.plot.data <- select(df.doj.ca, Year, TotalCrime_rate)

png('plots/Crime_Rates_1985_2017_California_overall_LAT.png',
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = Year, y = TotalCrime_rate)) +
  geom_line(colour = 'darkgoldenrod4') +
  geom_point(colour = 'darkgoldenrod4') +
  geom_text(aes(label = comma(round(TotalCrime_rate))), size = 3, vjust = -2) +
  labs(title = 'Overall Crime Rates - California',
       caption="Source: CA DOJ") +
  scale_y_continuous(name = 'Number of Crimes per 100K', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$TotalCrime_rate, na.rm = TRUE), 
                           1.025 * max(df.plot.data$TotalCrime_rate, na.rm = TRUE)))
print(p)
dev.off()



# "Those jumps were mainly driven by increases in property crimes, particularly thefts from motor vehicles."

# Property and violent contributions to the jumps
df.temp <- select(df.doj.ca, Year, TotalCrime_rate_chg, Property_rate_chg, Violent_nr_rate_chg)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('chg')),
                     funs(contrib = ./TotalCrime_rate_chg))
df.temp <- filter(df.temp, Year %in% c(2012, 2015))
df.temp <- gather(df.temp, crime, contribution, -Year)
df.temp <- arrange(df.temp, Year)
df.temp <- filter(df.temp, grepl('contrib', df.temp$crime))
df.temp

# Contribution of the sub-types of crimes to the jump
rm(df.temp)
df.temp <- select(df.doj.ca, Year, TotalCrime_rate_chg, AggAssault_rate_chg,	
                  Robbery_rate_chg,	ForRape_rate_chg,	Homicide_rate_chg, 
                  LTtotal_rate_chg,	Burglary_rate_chg,	VehicleTheft_rate_chg)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('chg')),
                     funs(contrib = ./TotalCrime_rate_chg))
df.temp <- filter(df.temp, Year %in% c(2012, 2015))
df.temp <- gather(df.temp, crime, contribution, -Year)
df.temp <- arrange(df.temp, Year)
df.temp <- filter(df.temp, grepl('contrib', df.temp$crime))
df.temp <- arrange(df.temp, Year, -contribution)
df.temp

# Contribution of the sub-sub-types of crimes to the jump
rm(df.temp)
df.temp <- select(df.doj.ca, Year, TotalCrime_rate_chg, Homicide_rate_chg, RAPact_rate_chg, ARAPact_rate_chg, 
                  HROBnao_rate_chg, MROBnao_rate_chg, CHROBnao_rate_chg, RROBnao_rate_chg, CROBnao_rate_chg, 
                  GROBnao_rate_chg, BROBnao_rate_chg, HASSact_rate_chg, OASSact_rate_chg, FASSact_rate_chg, 
                  KASSact_rate_chg, UBURact_rate_chg, FEBURact_rate_chg, MVTact_rate_chg, TMVTact_rate_chg, 
                  OMVTact_rate_chg, AOLARnao_rate_chg, MVLARnao_rate_chg, FBLARnao_rate_chg, SLLARnao_rate_chg, 
                  MVPLARnao_rate_chg, BILARnao_rate_chg, PPLARnao_rate_chg, PSLARnao_rate_chg, COMLARnao_rate_chg)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('chg')),
                     funs(contrib = ./TotalCrime_rate_chg))
df.temp <- filter(df.temp, Year %in% c(2012, 2015))
df.temp <- gather(df.temp, crime, contribution, -Year)
df.temp <- arrange(df.temp, Year)
df.temp <- filter(df.temp, grepl('contrib', df.temp$crime))
df.temp <- arrange(df.temp, Year, -contribution)
df.temp

sum(df.temp$contribution)


# Principal Component Analysis - Overall crime
rm(pca.input)
pca.input <- select(df.doj.ca, Year, Property_rate_chg, Violent_nr_rate_chg)
pca.input <- filter(pca.input, Year >= 2011)
pca.input <- arrange(pca.input, Year)
rownames(pca.input) <- pca.input$Year
pca.input <- select(pca.input, -Year)

pca <- prcomp(pca.input)

pca.eig <- get_eigenvalue(pca)
pca.eig

pca.var <- get_pca_var(pca)
pca.var$contrib


# Principal Component Analysis - Inside Property Crime
rm(pca.input)
pca.input <- select(df.doj.ca, Year, LTtotal_rate_chg, Burglary_rate_chg, VehicleTheft_rate_chg)
pca.input <- filter(pca.input, Year >= 2011)
pca.input <- arrange(pca.input, Year)
rownames(pca.input) <- pca.input$Year
pca.input <- select(pca.input, -Year)

pca <- prcomp(pca.input)

pca.eig <- get_eigenvalue(pca)
pca.eig

pca.var <- get_pca_var(pca)
pca.var$contrib


# Principal Component Analysis - Inside larceny
rm(pca.input)
pca.input <- select(df.doj.ca, Year, 
                    AOLARnao_rate_chg,
                    MVLARnao_rate_chg,
                    FBLARnao_rate_chg,
                    SLLARnao_rate_chg,
                    MVPLARnao_rate_chg,
                    BILARnao_rate_chg,
                    PPLARnao_rate_chg,
                    PSLARnao_rate_chg,
                    COMLARnao_rate_chg)
pca.input <- filter(pca.input, Year >= 2011)
pca.input <- arrange(pca.input, Year)
rownames(pca.input) <- pca.input$Year
pca.input <- select(pca.input, -Year)

pca <- prcomp(pca.input)

pca.eig <- get_eigenvalue(pca)
pca.eig

pca.var <- get_pca_var(pca)
pca.var$contrib



# "After decades of mirroring national downward trends in violent crime, California saw a 12% increase
# from 2014 to 2017, while the violent crime rate in the other 49 states together increased only 3%"

rm(df.temp)
df.temp1 <- select(df.ucr.ca, year, state, violent.nr.crime.rate)
df.temp2 <- select(df.ucr.us49, year, state, violent.nr.crime.rate)
df.temp <- rbind(df.temp1, df.temp2)
rm(df.temp1, df.temp2)

df.temp <- filter(df.temp, year %in% c(2014, 2017))  
df.temp <- arrange(df.temp, state, year)
df.temp <- group_by(df.temp, state)  
df.temp <- mutate_at(df.temp,
                     vars(ends_with('rate')),
                     funs(pct.chg = ./lag(.,1)-1))  
df.temp  



# "California’s property crime rate fell slightly in the last two years..."

rm(df.temp)
df.temp <- select(df.doj.ca, Year, Property_rate, Property_rate_chg)
df.temp <- filter(df.temp, Year >= 2010)
df.temp



# "... but remains 2 percent higher than it was in 2014." 

rm(df.temp)
df.temp <- select(df.doj.ca, Year, Property_rate)
df.temp <- filter(df.temp, Year %in% c(2014, 2017))
df.temp <- arrange(df.temp, Year)
df.temp <- mutate_at(df.temp, 
                     vars(ends_with('rate')), 
                     funs(chg = ./lag(.,1)-1))
df.temp



# "By contrast, the rate of property crimes in the rest of the nation has dropped by 10% over the same period."

rm(df.temp)
df.temp <- select(df.ucr.us49, year, property.crime.rate)
df.temp <- filter(df.temp, year %in% c(2014, 2017))
df.temp <- arrange(df.temp, year)
df.temp <- mutate_at(df.temp, 
                     vars(ends_with('rate')), 
                     funs(chg = ./lag(.,1)-1))
df.temp



# "Crime trends vary dramatically from county to county. Thirty-one of the state’s 58 counties saw an 
# increase in violent crime last year, while 22 saw an increase in property crimes."

rm(df.temp)
df.temp <- select(df.doj.counties, Year, County, Property_rate_chg, Violent_nr_rate_chg)
df.temp <- filter(df.temp, Year == 2017)
# Number of counties that had in increase in violent crime in 2017
sum(df.temp$Violent_nr_rate_chg > 0)
# Number of counties that had in increase in property crime in 2017
sum(df.temp$Property_rate_chg > 0)



# "What single factor can explain the fact that violent crime went up 6% last year in Los Angeles but 
# fell 6% in Sacramento?"

rm(df.temp)
df.temp <- select(df.doj.counties, Year, County, Violent_nr_rate)
df.temp <- filter(df.temp,
                  County %in% c('Los Angeles County', 'Sacramento County'),
                  Year %in% c(2016, 2017))
df.temp <- arrange(df.temp, County, Year)
df.temp <- group_by(df.temp, County)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('rate')),
                     funs(pct.chg = ./lag(.,1)-1))  
df.temp



# "To complicate matters, specific crimes come with their own caveats. Reports of rape have 
# increased nationally since 2013,"

rm(df.plot.data)
df.plot.data <- select(df.ucr.us, year, state, legacy.rape.total, revised.rape.total)
df.plot.data <- group_by(df.plot.data, year)
df.plot.data <- summarise_at(df.plot.data, 
                             vars(ends_with('total')),
                             funs(sum))
df.plot.data <- filter(df.plot.data, year >= 2000)
df.plot.data <- gather(df.plot.data, variable, value, -year)

png(paste0('plots/Crime_Sums_2000_2017_US_rape_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = year, y = value, colour = variable)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = comma(round(value))), size = 3, vjust = -2) +
  labs(title = 'Rape Rates - Uniteds States',
       caption="Source: FBI") +
  scale_y_continuous(name = 'Number of Crimes per 100K', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$value, na.rm = TRUE), 
                           1.025 * max(df.plot.data$value, na.rm = TRUE)))
print(p)
dev.off()



# "Reports of aggravated assaults in California also have increased..."

rm(df.plot.data)
df.plot.data <- select(df.doj.ca, Year, AggAssault_sum)
df.plot.data <- filter(df.plot.data, Year >= 2000)

png(paste0('plots/Crime_Sums_2000_2017_California_aggAssault_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = Year, y = AggAssault_sum)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = comma(round(AggAssault_sum))), size = 3, vjust = -2) +
  labs(title = 'Aggravated Assaults - California',
       caption="Source: DOJ") +
  scale_y_continuous(name = 'Number of Assaults', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$AggAssault_sum, na.rm = TRUE), 
                           1.025 * max(df.plot.data$AggAssault_sum, na.rm = TRUE)))
print(p)
dev.off()


# "... (2014) the year with the fewest crimes reported in the state since the 1960s..."

rm(df.plot.data)
df.plot.data <- select(df.doj.ca, Year, TotalCrime_sum)

png(paste0('plots/Crime_Sums_1985_2017_California_total_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = Year, y = TotalCrime_sum)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste(comma(round(TotalCrime_sum/1e6, 2)), 'M')), size = 3, vjust = -2) +
  labs(title = 'Total Crime - California',
       caption="Source: CA DOJ") +
  scale_y_continuous(name = 'Number of Crimes', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$TotalCrime_sum, na.rm = TRUE), 
                           1.025 * max(df.plot.data$TotalCrime_sum, na.rm = TRUE)))
print(p)
dev.off()



# "Californians for Safety and Justice, a group that co-authored Proposition 47, points out that several 
# states saw larger increases in violent crime than California from 2016 to 2017. (An analysis by The Times 
# and the Marshall Project found 20 states with larger increases in violent crime rates.)"

rm(df.temp)
df.temp <- select(df.ucr.us, state, year, violent.nr.crime.rate)
df.temp <- filter(df.temp, year %in% c(2016, 2017))
df.temp <- arrange(df.temp, state, year)
df.temp <- group_by(df.temp, state)
df.temp <- mutate_at(df.temp, 
                     vars(ends_with('rate')),
                     funs(pct.chg = ./lag(.,1) - 1))
df.temp <- na.omit(df.temp)
df.temp <- arrange(df.temp, -pct.chg)
head(df.temp, n = 20)



# "After national crime data for 2017 released this fall showed California departed from the 
# national trend — violent crime in California ticked up slightly while it fell slightly across
# the 49 other states taken together..."

rm(df.temp)
df.temp1 <- select(df.ucr.ca, year, state, violent.nr.crime.rate, violent.nr.crime.rate.chg)
df.temp2 <- select(df.ucr.us49, year, state, violent.nr.crime.rate, violent.nr.crime.rate.chg)
df.plot.data <- rbind(df.temp1, df.temp2)
df.plot.data <- filter(df.plot.data, year >= 2010)
rm(df.temp1, df.temp2)

# Check that indeed in 2017 CA increased and the 49 decreased
filter(df.plot.data, year == 2017)

# Now confirm that during the previous years they moved in the same direction
png(paste0('plots/Crime_Rates_2000_2017_California_v_49_violent_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = year, y = violent.nr.crime.rate, colour = state)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = comma(round(violent.nr.crime.rate))), size = 3, vjust = -2) +
  labs(title = 'Violent Crime Rates - California Vs 49 States',
       caption="Source: UCR") +
  scale_y_continuous(name = 'Number of Crimes per 100K', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$violent.nr.crime.rate, na.rm = TRUE), 
                           1.025 * max(df.plot.data$violent.nr.crime.rate, na.rm = TRUE)))
print(p)
dev.off()



# "California’s robbery rate jumped 14% from 2014 to 2017; the rest of the country saw a 7% drop."

rm(df.temp)
df.temp1 <- select(df.ucr.ca, year, state, robbery.rate)
df.temp2 <- select(df.ucr.us49, year, state, robbery.rate)
df.temp <- rbind(df.temp1, df.temp2)
rm(df.temp1, df.temp2)

df.temp <- filter(df.temp, year %in% c(2014, 2017))
df.temp <- arrange(df.temp, state, year)
df.temp <- group_by(df.temp, state)
df.temp <- mutate_at(df.temp,
                     vars(ends_with('rate')),
                     funs(pct.chg = ./lag(.,1)-1))
df.temp



# "San Francisco’s struggle with car break-ins illustrates the difficulty of trying to determine if 
# the reforms are linked to changes in local crimes. Thefts from vehicles nearly tripled in the city 
# from 2011 to 2017, when they hit 29,851."

rm(df.temp)
df.temp <- select(df.doj.counties, County, Year, MVLARnao_sum)
df.temp <- filter(df.temp, 
                  Year %in% c(2011, 2017),
                  County == 'San Francisco County')
df.temp



# "San Francisco police Lt. Michael Nevin, who until November oversaw car break-in investigations in a 
# district that includes many of the tourist hot spots, said he had noticed a sharp spike before Proposition 47."

rm(df.plot.data)
df.plot.data <- select(df.doj.counties, County, Year, MVLARnao_rate)
df.plot.data <- filter(df.plot.data, 
                       Year >= 2010, 
                       County == 'San Francisco County')

png(paste0('plots/Crime_Rates_2010_2017_Francisco_mvlar_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = Year, y = MVLARnao_rate)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = comma(round(MVLARnao_rate))), size = 3, vjust = -2) +
  labs(title = 'Theft from Vehicle Rates - San Francisco',
       caption="Source: DOJ") +
  scale_y_continuous(name = 'Number of Crimes per 100K', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$MVLARnao_rate, na.rm = TRUE), 
                           1.025 * max(df.plot.data$MVLARnao_rate, na.rm = TRUE)))
print(p)
dev.off()



# "Shoplifting soared in many counties in 2015, with Los Angeles and San Bernardino seeing 
# the most dramatic increases. But then it dropped statewide over the next two years, falling 
# to its lowest level in a decade." 

rm(df.temp)
df.temp <- select(df.doj.counties, Year, County, SLLARnao_sum_chg)
df.temp <- filter(df.temp, Year == 2015)
df.temp <- arrange(df.temp, -SLLARnao_sum_chg)
head(df.temp)


# "But then it dropped statewide over the next two years, falling to its lowest level in a decade."

rm(df.plot.data)
df.plot.data <- select(df.doj.ca, Year, SLLARnao_sum)
df.plot.data <- filter(df.plot.data, Year >= 2000)

png(paste0('plots/Crime_Sums_2000_2017_California_sllar_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = Year, y = SLLARnao_sum)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = comma(round(SLLARnao_sum))), size = 3, vjust = -2) +
  labs(title = 'Shoplifting - California',
       caption="Source: DOJ") +
  scale_y_continuous(name = 'Number of Crimes', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$SLLARnao_sum, na.rm = TRUE), 
                           1.025 * max(df.plot.data$SLLARnao_sum, na.rm = TRUE)))
print(p)
dev.off()



# "In San Joaquin County, which has long had one of the state’s highest crimes rates, property crimes
# have fallen by 25% since 2011."

rm(df.temp)
df.temp <- select(df.doj.counties, County, Year, Property_sum)
df.temp <- filter(df.temp,
                  County == 'San Joaquin County',
                  Year %in% c(2011, 2017))
df.temp <- mutate_at(df.temp, 
                     vars(ends_with('sum')), 
                     funs(pct.chg = ./lag(., 1)-1))
df.temp


# "In 2017, (San Joaquin) saw the lowest number of shoplifting reports in 15 years."

rm(df.plot.data)
df.plot.data <- select(df.doj.counties, County, Year, SLLARnao_sum)
df.plot.data <- filter(df.plot.data,
                       County == 'San Joaquin County', 
                       Year >= 2000)

png(paste0('plots/Crime_Sums_2000_2017_Joaquin_sslar_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = Year, y = SLLARnao_sum)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = comma(round(SLLARnao_sum))), size = 3, vjust = -2) +
  labs(title = 'Shoplifting - San Joaquin',
       caption="Source: DOJ") +
  scale_y_continuous(name = 'Number of Crimes', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$SLLARnao_sum, na.rm = TRUE), 
                           1.025 * max(df.plot.data$SLLARnao_sum, na.rm = TRUE)))
print(p)
dev.off()



# San Joaquin auto thefts.

rm(df.plot.data)
df.plot.data <- select(df.doj.counties, County, Year, VehicleTheft_sum)
df.plot.data <- filter(df.plot.data,
                       County == 'San Joaquin County', 
                       Year >= 2000)

png(paste0('plots/Crime_Sums_2000_2017_Joaquin_vehicleTheft_LAT.png'),
    width = 900, height = 450)
p <- ggplot(df.plot.data, aes(x = Year, y = VehicleTheft_sum)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = comma(round(VehicleTheft_sum))), size = 3, vjust = -2) +
  labs(title = 'Vehicle Thefts - San Joaquin',
       caption="Source: DOJ") +
  scale_y_continuous(name = 'Number of Crimes', labels = scales::comma) +
  coord_cartesian(ylim = c(min(df.plot.data$VehicleTheft_sum, na.rm = TRUE), 
                           1.025 * max(df.plot.data$VehicleTheft_sum, na.rm = TRUE)))
print(p)
dev.off()

