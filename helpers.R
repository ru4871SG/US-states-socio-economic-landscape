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

#let's create a similar vertical data but for education percentage
part2_map2 <- educationpercentage
part2_map2 <- slice(part2_map2, -1)

#since we need the GeoFips to be consistently 2 digits, let's use the same mutate() but this time we target part2_statesonly. Then, we use left_join to part2_map2
part2_statesonly <- part2_statesonly %>% mutate(GeoFips = sprintf("%02d", floor(GeoFips / 1000)))

part2_map2 <- left_join(part2_map2, part2_statesonly, by = c("Name"="Name"))

#Just like before, we have a lot of empty data for Puerto Rico, let's not visualize it this time.
part2_map2 <- part2_map2[-nrow(part2_map2), ]

#let's rename the columns that include multiple years, we can put a footnote later in the visualization
part2_map2 <- part2_map2 %>% rename(`2012` = `2008-2012`, `2021` = `2017-2021`)

#lastly, we need to make the data vertical
part2_map2 <- part2_map2 %>% pivot_longer(names_to = "year", values_to = "college_degree", `1970`:`2021`)

#let's rearrange the columns order
part2_map1 <- part2_map1 %>% select(GeoFips, GeoName, year, income_per_capita)

part2_map2 <- part2_map2 %>% rename(GeoName = Name) %>% select(GeoFips, GeoName, year, college_degree)

# we don't need part3, I decided to merge them with the map pages, using the same data.
# part3_incomepercapita <- filter(part2_map1, year == "2022")
# part3_education <- filter(part2_map2, year == "2021")

#let's repeat similar steps like part 2 but we include the country itself as one of the selections so we can visualize it in the line chart later
part4_earlydata <- personalincomepercapita
part4_earlydata$GeoName <- gsub("\\s\\*", "", part4_earlydata$GeoName)
part4_earlydata$GeoName <- trimws(part4_earlydata$GeoName, which = "right")
part4_statesonly <- educationpercentage

part4_statesonly <- left_join(part4_statesonly, part4_earlydata, by = c("Name"="GeoName"))
part4_statesonly <- select(part4_statesonly, GeoFips, Name)

part4_incomepercapita <- personalincomepercapita

part4_incomepercapita$GeoName <- gsub("\\s\\*", "", part4_incomepercapita$GeoName) 
part4_incomepercapita$GeoName <- trimws(part4_incomepercapita$GeoName, which = "right")

part4_incomepercapita <- part4_incomepercapita %>% pivot_longer(names_to = "year", values_to = "income_per_capita", `1970`:`2022`)

part4_incomeline <- left_join(part4_statesonly, part4_incomepercapita, by = c("GeoFips"="GeoFips"))

#duplicate columns for state names, let's only keep one of them
part4_incomeline <- select(part4_incomeline, -Name)

#let's change the GeoFips code to just 2 digits
part4_incomeline <- part4_incomeline %>%
  mutate(GeoFips = sprintf("%02d", floor(GeoFips / 1000)))

#there's one blank row, let's delete it
part4_incomeline <- part4_incomeline[-nrow(part4_incomeline), ]

#it turns out the "year" column was written as chr, not num. let's change that.
part4_incomeline$Year <- as.numeric(part4_incomeline$year)
part4_incomeline <- select(part4_incomeline, -year)
part4_incomeline <- part4_incomeline %>% rename(year = Year)

part4_educationline <- educationpercentage

part4_statesonly <- part4_statesonly %>% mutate(GeoFips = sprintf("%02d", floor(GeoFips / 1000)))

part4_educationline <- left_join(part4_educationline, part4_statesonly, by = c("Name"="Name"))

part4_educationline <- part4_educationline[-nrow(part4_educationline), ]

part4_educationline <- part4_educationline %>% rename(`2012` = `2008-2012`, `2021` = `2017-2021`)

part4_educationline <- part4_educationline %>% pivot_longer(names_to = "year", values_to = "college_degree", `1970`:`2021`)

#let's rearrange the columns order
part4_incomeline <- part4_incomeline %>% select(GeoFips, GeoName, year, income_per_capita)
part4_educationline <- part4_educationline %>% rename(GeoName = Name) %>% select(GeoFips, GeoName, year, college_degree)


part4_incomeline <- part4_incomeline[ifelse(part4_incomeline$GeoName == "United States", FALSE, TRUE), ]
part4_educationline <- part4_educationline[ifelse(part4_educationline$GeoName == "United States", FALSE, TRUE), ]