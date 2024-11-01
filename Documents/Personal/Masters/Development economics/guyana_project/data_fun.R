#install packages
#install.packages("striprtf")
install.packages("ggplot2")
#install.packages("Synth")
install.packages("httr")
install.packages("jsonlite")

#read packages
library(tidyr)
library(dplyr)
library(striprtf)
library(ggplot2)
library(stringr)
library(Synth)
library(httr)
library(jsonlite)

setwd("/Users/aphrasmith/Documents/Personal/Masters/Development economics/guyana_project")

# Replace with the actual API endpoint
url <- "https://data.un.org/ws/rest/data/UIS,DF_UNData_UIS,1.1/all/ALL/?detail=full&dimensionAtObservation=TIME_PERIOD"

# Send GET request
response <- GET(url)

# Check the status code
if (status_code(response) == 200) {
  # Parse JSON content
  data <- content(response, "text")
  parsed_data <- fromJSON(data)



latin_america <- read.csv(".data/latin_american_iso_codes.csv", header = FALSE)

gdp_pc <- read.csv("./Data/gdp_per_capita.csv") %>% 
  select(-c(Country.Name, Indicator.Name, Indicator.Code)) %>% 
  filter(Country.Code %in% latin_america$V2)

gdp <- read.csv("./Data/gdp.csv") %>% 
  select(-c(Country.Name, Indicator.Name, Indicator.Code)) %>% 
  filter(Country.Code %in% latin_america$V2)

# Clean GDP per capita data
gdp_pc <- drop_na(gdp_pc[, -c(2:11)]) %>% 
  filter(Country.Code %in% latin_america$V2) %>%
  pivot_longer(cols = -c("Country.Code"), names_to = "year", values_to = "value")

gdp_pc$country <- gdp_pc$Country.Code
gdp_pc$year <- as.numeric(str_sub(gdp_pc$year, 2))
gdp_pc$treatment <- ifelse(gdp_pc$Country.Code == "GUY" & gdp_pc$year >= 2015, 1, 0)

# Only select necessary columns
gdp_pc <- gdp_pc %>% select(value, country, year)

# Clean GDP data
gdp <- drop_na(gdp[, -c(2:11)]) %>% 
  pivot_longer(cols = -c("Country.Code"), names_to = "year", values_to = "value")

gdp$country <- gdp$Country.Code
gdp$year <- as.numeric(str_sub(gdp$year, 2))  # Fix here
gdp <- gdp %>% select(country, year, value)

# join
# data <- gdp_pc %>% left_join(gdp, by = c("country", "year"), suffix = c("_pc", "_gdp"))


ggplot(filter(gdp,country=="GUY"), aes(x = year, y = value)) +
  geom_line() +
  labs(x = "Year", y = "GDP", color = "Country") + 
  scale_x_continuous() +  
  geom_vline(xintercept = 2015, color = "grey", linetype = "dashed", linewidth = 1) +
  theme_minimal()


exclude <- c("HTI")

gdp_pc<- gdp_pc %>% filter(!country %in% exclude)

ggplot(filter(gdp_pc,country=="GUY"), aes(x = year, y = value)) +
  geom_line() +
  labs(x = "Year", y = "GDP per Capita", color = "Country") + 
  scale_x_continuous() +  
  geom_vline(xintercept = 2015, color = "grey", linetype = "dashed", size = 1) +
  theme_minimal()

ggplot(gdp_pc, aes(x = year, y = value, group = country, color = country)) +
  geom_line() +
  labs(x = "Year", y = "GDP per Capita", color = "Country") + 
  scale_x_continuous() +  
  geom_vline(xintercept = 2015, color = "grey", linetype = "dashed", size = 1) +
  theme_minimal()

country_summary <- gdp_pc %>%
  group_by(country) %>%                     
  summarise(
    mean_gdp = mean(log_gdp_pc, na.rm = TRUE),  
    median_gdp = median(log_gdp_pc, na.rm = TRUE),  
    min_gdp = min(log_gdp_pc, na.rm = TRUE),     
    max_gdp = max(log_gdp_pc, na.rm = TRUE),      
    sd_gdp = sd(log_gdp_pc, na.rm = TRUE), 
    
    mean_yr = mean(year, na.rm = TRUE),  
    median_yr = median(year, na.rm = TRUE),  
    min_yr = min(year, na.rm = TRUE),     
    max_yr = max(year, na.rm = TRUE),      
    sd_yr = sd(year, na.rm = TRUE), 
    
    n = n()                                          
  )


gdp_pc<- as.data.frame(gdp_pc)

non_treated_ids <- unique(gdp_pc %>% filter(id != 12) %>% select(id))

dataprep.out <- dataprep(
  foo = gdp_pc,
  predictors = "value",
  dependent = "value", # predictors for the synthetic control
  predictors.op = "mean",      # option for how to aggregate predictors
  time.predictors.prior = 1970:2014, # time periods used to predict outcomes
  time.optimize.ssr = 2015:2023,     # time periods for optimization
  time.variable = "year",       # column with the time variable
  unit.variable = "id",
  treatment.identifier = 12,
  controls.identifier =  non_treated_ids %>% pull(id)  
)

dataprep.out$X1
dataprep.out$Z1

synth.out <- synth(dataprep.out)

path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "GDP per capita", Xlab = "Year",
          Main = "Synthetic Control Analysis for GDP per Capita")

