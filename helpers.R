personalincomepercapita <- read_csv("personalincomepercapita.csv")
personalincomepercapita <- tibble(personalincomepercapita)

educationpercentage <- read_csv("education_percentage.csv")
educationpercentage <- tibble(educationpercentage)

population <- read_csv("population.csv")
population <- tibble(population)

employment <- read_csv("employment.csv")
employment <- tibble(employment)

#I choose 2021 (and not 2022) because the education and employment data are only up to 2021. The 2022 data for these other tables don't exist, so it's fairer to plot 2021's data vs. 2021's data.
part1_incomepercapita <- select(personalincomepercapita, GeoFips, GeoName, `2021`)
part1_incomepercapita$GeoName <- gsub("\\s\\*", "", part1_incomepercapita$GeoName)
part1_incomepercapita$GeoName <- trimws(part1_incomepercapita$GeoName, which = "right")
part1_incomepercapita <- part1_incomepercapita %>% rename(incomepercapita_2021 = `2021`)

#we use right join because inside part1_incomepercapita we can see non-state values under GeoName.
part1_merged <- right_join(part1_incomepercapita, educationpercentage, by = c("GeoName"="Name"))

part1_merged <- part1_merged %>% rename(collegecompletion_2017_2021 = `2017-2021`)

part1_merged <- select(part1_merged, GeoFips, GeoName, incomepercapita_2021, collegecompletion_2017_2021)

#now we do similar cleaning steps but for employment and population, since we need to know the percentage of employment divided by population
part1_employment <- select(employment, GeoFips, GeoName, `2021`)
part1_employment$GeoName <- trimws(part1_employment$GeoName, which = "right")

part1_employment <- part1_employment %>% rename(employment_2021 = `2021`)

part1_population <- select(population, GeoFips, GeoName, `2021`)
part1_population$GeoName <- gsub("\\s\\*", "", part1_population$GeoName)
part1_population$GeoName <- trimws(part1_population$GeoName, which = "right")
part1_population <- part1_population %>% rename(population_2021 = `2021`)

part1_employment_population <- left_join(part1_employment, part1_population, by = c("GeoFips"="GeoFips"))

part1_employment_population <- part1_employment_population %>% mutate(employment_percentage_2021 = employment_2021/population_2021)

#now we merge all the data we need, so we can have one final source for the two scatterplots
part1_merged <- left_join(part1_merged, part1_employment_population, by = c("GeoFips"="GeoFips")) %>% select(GeoFips, GeoName, incomepercapita_2021, collegecompletion_2017_2021, employment_percentage_2021)

# we need to use slice() to eliminate the country from the rows. The idea is that we will only need to visualize the states.
part1_merged <- slice(part1_merged, -1)

#let's change the GeoFips code to just 2 digits, and assign the correct code to Puerto Rico
part1_merged <- part1_merged %>%
  mutate(GeoFips = sprintf("%02d", floor(GeoFips / 1000)))

part1_merged <- part1_merged %>%
  mutate(GeoFips = ifelse(GeoName == "Puerto Rico", "72", GeoFips))

#puerto rico has missing values, let's not include it this time
part1_merged <- part1_merged[ifelse(part1_merged$GeoName == "Puerto Rico", FALSE, TRUE), ]

#let's clean up personalincomepercapita
part2_earlydata <- personalincomepercapita
part2_earlydata$GeoName <- gsub("\\s\\*", "", part2_earlydata$GeoName)
part2_earlydata$GeoName <- trimws(part2_earlydata$GeoName, which = "right")

#let's get only all the correct states and their codes
part2_statesonly <- educationpercentage
part2_statesonly <- slice(part2_statesonly, -1)

part2_statesonly <- left_join(part2_statesonly, part2_earlydata, by = c("Name"="GeoName"))
part2_statesonly <- select(part2_statesonly, GeoFips, Name)

#now let's try to get income per capita numbers and the years, before we eliminate the non-state values
part2_incomepercapita <- personalincomepercapita %>% slice(-1) 

part2_incomepercapita$GeoName <- gsub("\\s\\*", "", part2_incomepercapita$GeoName) 
part2_incomepercapita$GeoName <- trimws(part2_incomepercapita$GeoName, which = "right")

part2_incomepercapita <- part2_incomepercapita %>% pivot_longer(names_to = "year", values_to = "income_per_capita", `1970`:`2022`)

part2_map1 <- left_join(part2_statesonly, part2_incomepercapita, by = c("GeoFips"="GeoFips"))

#duplicate columns for state names, let's only keep one of them
part2_map1 <- select(part2_map1, -Name)

#let's change the GeoFips code to just 2 digits. No need to change GeoFips data type, we need it as chr to merge it with U.S. states geojson data later
part2_map1 <- part2_map1 %>%
  mutate(GeoFips = sprintf("%02d", floor(GeoFips / 1000)))

#there's one blank row, let's delete it
part2_map1 <- part2_map1[-nrow(part2_map1), ]

#it turns out the "year" column was written as chr, not num. let's change that.
part2_map1$Year <- as.numeric(part2_map1$year)
part2_map1 <- select(part2_map1, -year)
part2_map1 <- part2_map1 %>% rename(year = Year)

#let's rearrange the columns order
part2_map1 <- part2_map1 %>% select(GeoFips, GeoName, year, income_per_capita)