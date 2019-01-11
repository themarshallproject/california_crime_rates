
![The Marshall Project](https://github.com/themarshallproject/california_crime_rates/blob/master/assets/TheMarshallProject_Logo_Primary_TrueBlack.png)


By [Manuel Villa](https://www.themarshallproject.org/staff/manuel-villa)

# Background

California’s state prisons faced severe overcrowding during the first decade of the century. In May 2011, the U.S. Supreme Court upheld a ruling ordering the state to reduce its prison population from close to 190 percent to 137.5 percent of design capacity, which at the time meant a reduction of almost 40,000 inmates.

The state responded by enacting Assembly Bill 109, known as Public Safety Realignment or AB 109, on October 1, 2011. This bill shifted incarceration and supervision responsibility for many lower-level felons from the state prison and parole systems to county jail and probation departments, based on the idea that locals can do a better job. Billions in state money were assigned to the Golden State's 58 counties, with few requirements on how to spend it, essentially creating 58 different criminal justice experiments.

In the following years, California passed other measures aimed at reducing prison overpopulation. In 2014 voters supported Proposition 47, which reclassified several non-violent felonies into misdemeanors, thus reducing the number of prison entries. And in 2016, voters overhauled the state parole system by backing Proposition 57, which gave thousands of inmates the chance to earn an earlier release from prison

The aim of these policies was not to prevent crime—which is at historically low levels in California—but the reduction of overcrowding of state prisons. And amid these changes, crime has increased in recent years. This has generated much debate about the causes and emerged as a central issue in a new effort to roll back some of the reforms.


# The Project

The Marshall Project, in partnership with the Los Angeles Times, is publishing a series of stories called [The California Experiment](https://www.themarshallproject.org/tag/the-california-experiment), dedicated to the effects the law changes in the Golden State have had for its criminal justice. Some of the questions we try to address are:

- What happened in California with crime, arrests, recidivism and several other aspects related to criminal justice after the new laws?
- Were these changes uniform across California's 58 countries? Are there clusters of counties that show similar trends?
- How did the rest of the country change in comparison?
- Who supports or rejects the reforms, and why?
- Are the claims of either group supported by data?

Data is an important component of this project and we publish all aspects of our analysis in this repository.

We gather national and state-level data on crime, arrests, law enforcement personnel and several other aspects of the penal system and we standardize them by computing measures such as crime rates—defined as number of crimes per 100,000 people.


# Data Sources
All of our data came from the following sources:

1. Crime data from the **California Department of Justice [Open Justice](https://openjustice.doj.ca.gov/data)** portal. The direct link to download the data is [here](https://openjustice.doj.ca.gov/downloads/Crimes_and_Clearances_with_Arson-1985-2017.csv).
1. Crime data from the **FBI's Uniform Crime Reporting** program ([1985-2014](https://www.ucrdatatool.gov/Search/Crime/State/StatebyState.cfm), [2015](https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/tables/table-2) and [2016-2017](https://ucr.fbi.gov/crime-in-the-u.s/2017/crime-in-the-u.s.-2017/topic-pages/tables/table-4))
1. Population data from the **U.S. Census** ([1980s](https://www2.census.gov/programs-surveys/popest/tables/1980-1990/counties/totals/e8089co.txt), [1990s](https://www2.census.gov/programs-surveys/popest/tables/1990-2000/estimates-and-change-1990-2000/2000c8_06.txt), [1990s](https://www2.census.gov/programs-surveys/popest/tables/1990-2000/counties/totals/99c8_06.txt), [2000s](https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-06.csv) and [2010s](https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/counties/asrh/))

These databases represent the ideal conditions for a data journalist: We did not need to spend months sending FOIA requests to law enforcement agencies across the country to ask for information—agencies who always have different degrees of cooperability and who can send data in all kinds of formats. And we did not have to immerse in the code-intensive task of cleaning, standardizing it and consolidating the information into a uniform database.

Instead, we had access to structured databases, readily downloadable and accompanied with clear data dictionaries. This allowed us to jump into the analysis from the beginning. (In other words, data heaven.) [Not all of our projects have run so smoothly](https://www.themarshallproject.org/2017/03/09/how-we-crunched-california-s-pay-to-stay-data).


# Tools

All of our analysis was done using [R](https://www.r-project.org/about.html), an Open Source language and environment for statistical computing and graphics. Whereas [Python](https://www.python.org/) is the language of choice among many data journalists to create databases from loosely existing information, R tends to be the preferred tool with structured databases.


# Repository Structure

### Folders
There are four folders in the repo, some containing subfolders named after the subject they address:
1. **source_data**. Original public data used. Mostly CSV and TXT files.
	- **crime** Crime data obtained from the FBI UCR and the California Department of Justice.
	- **population** Census data for the state of California, its counties and counties and cities.
	- **personnel** Number of people employed in California's law enforcement, pubic defending, prosecution and paroling systems.
1. **scripts**. Contains the scripts of R code that operate on the source data.
	- **crime** Scripts that compute crime rates.
	- **population** Scripts that process the population data.
	- **personnel** Scripts that analyse California's criminal justice system personnel numbers.
	- **story_checking** Scripts that fact-check the numbers and statistics quoted in the published stories.
1. **output_data**. Contains CSV files produced by the scripts.
1. **plots**. Contains plots and visualizations produced by the scripts.

The file named **build.R** is the master script that runs all other scripts in the appropriate order. To understand the flow of the whole repository, it is recommended to start by reading it.

# Methodology

We started the analysis with an outside-in approach. We plotted the annual crime rates for California versus the rest of the states going back to the 1980s. One aspect immediately stood out: For most of this century, crime rates in California and the rest of the country were headed down, but in 2012 and in 2015 there were two clear spikes in crime in the Golden State, while the rest of the 49 states taken together continued their downward path uninterrupted.


![Crime Rates - California Vs. Other 49 States](https://github.com/themarshallproject/california_crime_rates/blob/master/assets/crime_rates_1985_2017_CA_v_49_overall.png)


A natural question was: Was it a particular type of crime that spiked those years? To find out, we drilled down, dissecting crime into its highest-level components: property and violent. This suggested the spikes were mostly composed of jumps in property crime, not so much in violent crime.

Property crime has three major subclassifications: larceny, robbery and vehicle theft. The next logical question was: Were any of them the main engine of the jumps? Drilling down again into the data revealed larceny was the highest contributor the increases.

We also carried out regional analysis, trying to identify if any counties stood out in terms of crime. That is how we found out that San Francisco had seen a dramatic jump in vehicle break-ins, for example, while San Joaquin had experienced one of the largest decreases in crime over the last decade.

These discoveries and on-the-ground reporting fed each other. On occasions the data provided guides as to where to report or who to talk to; sometimes, the reporting suggested where else in the data we should be looking for more clues.

# Questions, Comments? 

We love feedback: mvilla@themarshallproject.com

If you wish to receive our roundups of the best criminal justice news from around the web, please [suscribe to our daily newsletter](https://themarshallproject.us3.list-manage.com/subscribe?u=a92567c13cca06b470824aead&id=5e02cdad9d).
