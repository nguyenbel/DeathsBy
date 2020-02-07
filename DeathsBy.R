install.packages("here", 'tidyverse', 'lemon')
library(tidyverse)
library(here)
library(lemon)

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

# Add in cities to corresponding zip codes to both datasets and remove extra columns for both datasets
deathByAge_BA <- merge(deathByAge_BA, bayAreaZip, by.x= 'Zip', by.y = 'ZIP') %>% 
  select(-the_geom, -STATE, -Area__, -Length__)

deathByGender_BA <- merge(deathByGender_BA, bayAreaZip, by.x = 'ZipCode', by.y = 'ZIP') %>%
  select(Year, ZipCode, PO_NAME, Gender, Count)

# Spread data in deathByAge
deathByAge_BA <- deathByAge_BA %>%
  spread(key = AgeGroup, value = Total) %>%
  # Organize age columns to be in numerical order
  select(Year, Zip, PO_NAME,`Under 1`,`15-24`, `25-34`, `35-44`,`45-54`, `55-64`,`65-74`, `75-84`, `85+`)

# Create Under_35 age column
deathByAge_BA <- deathByAge_BA %>%
  mutate(Under_35 = rowSums(deathByAge_BA[,c(4:6)], na.rm = T)) %>%
  # Reorganize age columns to be in numerical order and drop age groups under 35 years old
  select(Year, Zip, PO_NAME, Under_35, `35-44`,`45-54`, `55-64`,`65-74`, `75-84`, `85+`)
  

# Make NA to 0
deathByAge_BA[is.na(deathByAge_BA)] <- 0

# Change deathByAge_BA to long data
deathByAge_BA <- deathByAge_BA %>%
  gather(key = AgeGroup, value = Total, -Year, -Zip, -PO_NAME) %>%
  # Sort deathByAge_BA by year and zip
  arrange(Year, Zip)

#Reorder levels for AgeGroup by changing AgeGroup to factor
deathByAge_BA$AgeGroup <- factor(deathByAge_BA$AgeGroup,
  levels= c('Under_35', '35-44','45-54', '55-64','65-74', '75-84', '85+'))

#Compute total number of deaths grouped by Zip
deathTotal_Age <- deathByAge_BA %>% group_by(Zip) %>% 
  summarize(AllDeaths = sum(Total)) %>%
  #arrange in descending order to see which Zip had the most deaths
  arrange(desc(AllDeaths))

# Filter deathByAge_BA to get top 10 zip codes with most total deaths
top10_age <- deathTotal_Age[1:10,]

deathByAge_BA_top10 <- deathByAge_BA %>%
  subset(Zip %in% top10_age$Zip)

# Add in corresponding cities to zip codes by creating Zip Code and City column
deathByAge_BA_top10 <- deathByAge_BA_top10 %>%
  unite('Zip_City',c(Zip, PO_NAME), sep = '\n')

# Change Zip to factor in order to reorder Zip code by total
deathByAge_BA_top10 %>% 
  mutate(Zip_City = fct_rev(fct_reorder(factor(Zip_City), Total, .fun = sum))) %>%
  
  # Create a bar chart where Total is a function Zip, fill as Age Group
  ggplot(aes(x = Zip_City, y = Total, fill = AgeGroup)) +
  geom_col(position = 'dodge', color = 'black') +
  
  # Add labels of total for each column and angle the text for readability
  geom_text(aes(label = Total), position=position_dodge(width=0.9), hjust = -0.2, 
            size = 2.25, angle = 90) +
  
  # Facet wrap by year and have axis ticks repeat
  facet_rep_wrap(~Year, repeat.tick.labels = T) +
  
  # Change size of x-axis text
  theme(axis.text.x = element_text(size = 5.2)) +
  
  # Add title and rename axis
  labs(title ='Death By Zip Code by Age Group', subtitle ='Top 10 Bay Area Zip Codes') +
  xlab('Zip Codes and Cities') +
  ylab('Total Death Count')

# Compute total number of deaths grouped by Zip to ensure that both top 20s are the same
deathTotal_Gender <- deathByGender_BA %>%
  group_by(ZipCode) %>%
  summarize(AllDeaths = sum(Count)) %>%
  
  #arrange in descending order to see which Zip had the most deaths
  arrange(desc(AllDeaths))

# Create top 20 zip codes for gender dataset
top10_gender <- deathTotal_Gender[1:10,]

# Filter deathByGender_BA to get top 10 zip codes with most total deaths
deathByGender_BA_top10 <- deathByGender_BA %>%
  subset(ZipCode %in% top10_gender$ZipCode)

# Add in corresponding cities to zip codes by creating City and Zip Code column
deathByGender_BA_top10 <- deathByGender_BA_top10 %>%
  unite(Zip_City, c(ZipCode, PO_NAME), sep = '\n')

# Change Zip to factor in order to reorder Zip code by total
deathByGender_BA_top10 %>% 
  mutate(Zip_City = fct_rev(fct_reorder(factor(Zip_City), Count, .fun = sum))) %>%
  
  # Create a bar chart where Total is a function Zip, fill as Gender
  ggplot(aes(x = Zip_City, y = Count, fill = Gender)) +
  geom_col(position = 'dodge', color = 'black') +
  
  # Add labels for count for each column
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.4, size = 3) +
  
  # Facet wrap by year, have axis ticks repeat
  facet_rep_wrap(~Year, repeat.tick.labels = T) +
  
  # Change size of x-axis text
  theme(axis.text.x = element_text(size = 5.2)) +
  
  #Add title, subtitle, and rename axis
  labs(title = 'Death by Zip Code by Gender', subtitle = 'Top 10 Zip Codes in Bay Area') +
  xlab('Zip Codes and Cities') +
  ylab('Total Death Count')
