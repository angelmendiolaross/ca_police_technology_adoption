
### Clear global environment
rm(list=ls())

library(pacman)
pacman::p_unload(all)

pacman::p_load(
  tidyverse, #dplyr, readr, etc.
  magrittr, #%<>% operator
  ggthemes, #for pretty charts
  readxl, #read excel
  ggplot2 #plotting
)

# load the data
dt <- read_csv("Dropbox/Smart_City_Policing/ca_police_technology_adoption/data/atlas_of_surveillance/Atlas of Surveillance-California-20210701.csv")
dt <- read_csv("../data/atlas_of_surveillance/Atlas of Surveillance-California-20210701.csv")

# fixing a few errors I found
dt %<>%
  mutate(City = ifelse(City=="Culver", "Culver City", City))
dt %<>%
  mutate(City = ifelse(Agency=="Windsor Police Department", "Windsor", City))

# creating new df on municipalities
aos <- dt %>%
  subset(`Type of Juris`=="Municipal") %>%
  arrange(City)

# stripping down dataframe
aos <- aos[c(1:10)]

# creating indicator variables for each technology
#install.packages("fastDummies")
library(fastDummies)

aos <- dummy_cols(aos, select_columns = "Technology")

aos <- aos[c(2,11:21)]

# reduce each city to one row
aos %<>%
  group_by(City) %>%
  summarise_each(funs(sum))

# a few cities are listed more than once for a given technology... changing these to 1 instead of 2
aos %<>%
  mutate(across(where(is.numeric), ~ ifelse(. > 1, 1, .)))

# fixing name errors in the data for future join
aos %<>%
  mutate(City = ifelse(City=="Carmel-By-The-Sea", "Carmel-by-the-Sea", City),
         City = ifelse(City=="Paso Robles", "El Paso de Robles (Paso Robles)", City),
         City = ifelse(City=="Carmel By The Sea", "Carmel-by-the-Sea", City),
         City = ifelse(City=="Belvedere Tiburon", "Tiburon", City),
         City = ifelse(City=="Freemont", "Fremont", City),
         City = ifelse(City=="Morgan HIll", "Morgan Hill", City),
         City = ifelse(City=="Ventura", "San Buenaventura (Ventura)", City))

# dropping duplicates
aos_sum <- aos %>%
  group_by(City) %>%                     # Group by City
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

# getting list of 482 CA cities
load("../data/ivs_final.RData") # independent variables

# creating df of only cities and years
cities <- ivs_final %>%
  select(City) %>%  distinct(City, .keep_all = TRUE)

# joining with aos data
temp <- left_join(x = cities,
                  y = aos_sum,
                  by = "City")

# assigning NAs (i.e. cities NOT in  AoS as 0, or not adopted)
temp %<>% replace(is.na(.), 0)

# renaming variables
aos_dvs <- temp %>%
  rename(
    ALPR             = `Technology_Automated License Plate Readers`,
    BWC              = `Technology_Body-worn Cameras`,
    cam_registry     = `Technology_Camera Registry`,
    cell_simulator   = `Technology_Cell-site Simulator`,
    drones           = Technology_Drones,
    face_recog       = `Technology_Face Recognition`,
    gunshot_detec    = `Technology_Gunshot Detection`,
    pred_pol         = `Technology_Predictive Policing`,
    crime_cntr       = `Technology_Real-Time Crime Center`,
    ring             = `Technology_Ring/Neighbors Partnership`,
    video_analytics  = `Technology_Video Analytics`
  )

# saving out a final AOS dataset
save(aos_dvs, file = "Dropbox/Smart_City_Policing/ca_police_technology_adoption/data/aos_dvs.RData")
save(aos_dvs, file = "../data/aos_dvs.RData")