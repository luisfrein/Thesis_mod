# Load packages -----------------------------------------------------------
#Load packages. If you haven't install them, please do so with the function install.packages('package name')
library(tidyverse)
library(plm) 

# Import data -------------------------------------------------------------
internet_users_raw <- read.csv('share-of-individuals-using-the-internet.csv') %>% 
  #janitor::clean_names() takes the column names and modifies them to a more friendly format
  janitor::clean_names()

#The 'skip' arguments skips a number of rows from the original file, 5 rows in this case
idh_raw <- read_csv('Human Development Index (HDI).csv', 
                    skip = 5) %>% 
  janitor::clean_names()

indicators_raw <- read_csv('world_development_indicators.csv') %>% 
  janitor::clean_names()

wgi <- readxl::read_xlsx('wgidataset.xlsx',
                         sheet = 'RuleofLaw', 
                         skip = 13)

gdp_growth <- read_csv('gdp_growth_rate.csv',
                       skip = 4) 

gdp_pc <- readxl::read_xlsx('WEO_Data.xlsx')

# Internet users wrangle --------------------------------------------------

#Filter the internet_users_raw dataset for the countries we need
internet_users_raw %>% 
  filter(year >= 2010 & year < 2018,
         entity %in% c('Argentina', 'Ecuador', 'Uruguay', 'Venezuela')) -> internet_users

# idh wrangle -------------------------------------------------------------

#Clean and wrangle idh_raw
idh_raw %>% 
  #Drop the columns full of NAs
  select_if(~!all(is.na(.))) %>% 
  #Select the country column and the columns with the years of study
  select(2, 23:30) %>%
  #Takes the dataset from wide to long format
  pivot_longer(2:9, 
               names_to = 'year', 
               values_to = 'idh') %>% 
  #The country observations have white space, let's remove that. The year observations got an x prefix, let's remove that too
  mutate(country = str_remove(country, '\\(Bolivarian Republic of\\)'),
         #str_trim() removes white space
         country = str_trim(country),
         #str_remove() removes a string pattern
         year = as.numeric(str_remove(year, 'x')),
         idh = as.numeric(idh)) %>% 
  filter(country %in% c('Argentina', 'Ecuador', 'Uruguay', 'Venezuela')) %>% 
  #Bind our modified idh dataset with internet_users dataset. Binded by columns
  bind_cols(internet_users) -> model_data_raw

# development indicators wrangling ----------------------------------------

indicators_raw %>% 
  filter(series_name == 'Adjusted net enrollment rate, primary (% of primary school age children)') %>%  
  select(-2, -(3:8), -(17:19)) %>% 
  pivot_longer(2:9, 
               names_to = 'year',
               values_to = 'enrollment_rate') %>% 
  mutate(year = as.numeric(str_sub(year, start = 9)),
         enrollment_rate = as.numeric(enrollment_rate)) %>% 
  bind_cols(model_data_raw) -> model_data_raw


# wgi wrangling -----------------------------------------------------------

wgi %>% 
  #Select the first column and the columns that contain the word 'Estimate'
  select(1, 
         where(~any(grepl('Estimate', .)))) %>%
  #Drop the first row
  slice(-1) %>% 
  pivot_longer(2:23, 
               names_to = 'year', 
               values_to = 'rule') %>%
  #Subsets the first 4 characters of the string
  mutate(year = str_sub(year, 1, 4),
         rule = as.numeric(rule)) %>% 
  #Rename column 1
  rename(country = ...1) %>% 
  filter(year >= 2010 & year < 2018,
         country %in% c('Argentina', 'Ecuador', 'Uruguay', 'Venezuela, RB')) %>% 
  bind_cols(model_data_raw) -> model_data_raw

# gdp growth wrangling ----------------------------------------------------

gdp_growth %>% 
  filter(`Country Name` %in% c('Argentina', 'Ecuador', 'Uruguay', 'Venezuela, RB')) %>% 
  select(-(2:54), 
         -(63:65)) %>% 
  pivot_longer(2:9,
               names_to = 'year',
               values_to = 'gdp_growth') %>% 
  bind_cols(model_data_raw) -> model_data_raw


# gdp_pc wrangling --------------------------------------------------------
gdp_pc %>% 
  select(-(2:3)) %>% 
  pivot_longer(2:9, 
               names_to = 'year', 
               values_to = 'pcgdp') %>% 
  bind_cols(model_data_raw) -> model_data_raw

# Finishing touches for model_data_raw ------------------------------------
model_data_raw %>% 
  rename(year = year...2) %>% 
  #Drop redundant columns
  select(-contains(c('country', 'code', '...')),
         -c(5, 7, 10)) %>% 
  relocate(entity) %>% 
  relocate(idh, .before = gdp_growth) %>% 
  mutate(entity = as_factor(entity)) -> model_data

#write_csv(model_data, 'model_data.csv')
