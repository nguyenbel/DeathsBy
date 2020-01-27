install.packages("here")
install.packages('tidyverse')
library(tidyverse)
library(here)

# Import files death by zip code by age, death by zip code by gender, and Bay Area zip codes
# Data is saved to desktop, but if downloaded to another place, change 'Desktop' to said place
deathByAge <- read_csv(here('Desktop','DeathsBy','death-by-zip-by-age-group2013-2016.csv'))
deathByGender <- read_csv(here('Desktop', 'DeathsBy', 
                               'deaths-by-zip-code-by-gender-2012-current.csv'))
bayAreaZip <- read_csv(here('Desktop', 'DeathsBy', 'bayarea_zipcodes.csv'))

# Subset death by zip code by age by Bay Area zip codes
deathByAge_BA <- deathByAge %>%
  subset(Zip %in% bayAreaZip$ZIP)

# Subset death by zip code by gender by Bay Area zip codes and make years 2013-2016
deathByGender_BA <- deathByGender %>%
  subset(ZipCode %in% bayAreaZip$ZIP) %>%
  filter(Year != 2012)

# Spread data in deathByAge
deathByAge_BA <- deathByAge_BA %>%
  spread(key = AgeGroup, value = Total) %>%
  # Organize age columns to be in numerical order
  select(Year, Zip,`Under 1`,`15-24`, `25-34`, `35-44`,`45-54`, `55-64`,`65-74`, `75-84`, `85+`)

# Create Under_35 age column
deathByAge_BA <- deathByAge_BA %>%
  mutate(Under_35 = rowSums(deathByAge_BA[,c(3:5)], na.rm = T)) %>%
  # Reorganize age columns to be in numerical order and drop age groups under 35 years old
  select(Year, Zip, Under_35, `35-44`,`45-54`, `55-64`,`65-74`, `75-84`, `85+`)

# Create deathByAge_BA_wide to make GrandTotal column
deathByAge_BA_wide <- deathByAge_BA %>%
  mutate(GrandTotal = rowSums(deathByAge_BA[,c(3:9)], na.rm = T))

# Sort deathbyAge_BA_wide by GrandTotal
deathByAge_BA_wide <- deathByAge_BA_wide %>%
  arrange(Zip)

# Make NA to 0
deathByAge_BA[is.na(deathByAge_BA)] <- 0
deathByAge_BA_wide[is.na(deathByAge_BA_wide)] <- 0

# Change deathByAge_BA to long data
deathByAge_BA <- deathByAge_BA %>%
  gather(key = AgeGroup, value = Total, -Year, -Zip) %>%
  # Sort deathByAge_BA by year and zip
  arrange(Year, Zip)

#Compute total number of deaths grouped by Zip
deathTotal_Age <- deathByAge_BA %>% group_by(Zip) %>% 
  summarize(AllDeaths = sum(Total)) %>%
  #arrange in descending order to see which Zip had the most deaths
  arrange(desc(AllDeaths))

# Filter deathByAge_BA to get top 20 zip codes with most total deaths
top20_age <- deathTotal_Age[1:20,]
deathByAge_BA_top20 <- deathByAge_BA %>%
  subset(Zip %in% top20_age$Zip)

# Change Zip to factor in order to reorder Zip code by total
deathByAge_BA_top20$Zip <- as.factor(deathByAge_BA_top20$Zip)
deathByAge_BA_top20 %>% 
  mutate(Zip = fct_reorder(Zip, Total, .fun = sum)) %>%
  # Create a bar chart where Total is a function Zip, with fill as Age Group
  ggplot(aes(x = Zip, y = Total, fill = AgeGroup)) +
  geom_col(position = 'dodge', color = 'black')
# Facet wrap by year
  facet_wrap(~Year)

# Compute total number of deaths grouped by Zip to ensure that both top 20s are the same
deathTotal_Gender <- deathByGender_BA %>%
  group_by(ZipCode) %>%
  summarize(AllDeaths = sum(Count)) %>%
  #arrange in descending order to see which Zip had the most deaths
  arrange(desc(AllDeaths))

# Create top 20 zip codes for gender dataset
top20_gender <- deathTotal_Gender[1:20,]

# Filter deathByGender_BA to get top 20 zip codes with most total deaths
deathByGender_BA_top20 <- deathByGender_BA %>%
  subset(ZipCode %in% top20$Zip)

# Change Zip to factor in order to reorder Zip code by total
deathByGender_BA_top20$ZipCode <- as.factor(deathByGender_BA_top20$ZipCode)
deathByGender_BA_top20 %>% 
  mutate(ZipCode = fct_reorder(ZipCode, Count, .fun = sum)) %>%
  # Create a bar chart where Total is a function Zip, with fill as Age Group
  ggplot(aes(x = ZipCode, y = Count, fill = Gender)) +
  geom_col(position = 'dodge') +
  facet_wrap(~Year)
