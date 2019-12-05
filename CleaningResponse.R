library(tidyverse)
MSAlist <- read_csv("inputs/MSA list.csv")

responserates <-  read.delim("Inputs/participationrates2010.txt", sep = "|") %>% 
  select(-X, -X.1, -X.2, -X.3) %>%
  rename(Geography = "American.Indian.Area..Reservation.") %>% 
  rename(Name = "Big.Lagoon") %>% 
  rename(GEOID = "X0240") %>% 
  rename(response2000 = "X0.60") %>% 
  rename(response2010 = "X0.00")

countyresponse <- responserates  %>% 
  filter(Geography == "County ") %>% 
  mutate(County = as.character(County),
         GEOID = as.character(GEOID)) %>% 
  filter(str_detect(County, ", LA"))

tractresponse <- responserates %>% 
  filter(Geography == "Tract ") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  mutate(GEOID2 = substring(GEOID, 1, 2),
         Geography = as.character(Geography)) %>%  
  filter(GEOID2 == "22") %>% 
  select(-Name)  

# The census defines cities according to their legally-defined boundaries. These are not metro areas. 
cityresponse <- responserates %>%
  filter(grepl("city, LA", Name))

# For the same reason we can't aggregate tract-level rates up to neighborhoods, we cannot aggregate parish-level rates up to metros. This data table labels each parish with the metro to which it belongs. 
metroresponse <-  countyresponse %>%
  mutate(FIPS_5 = as.numeric(GEOID)) %>%
  left_join(MSAlist,by = "FIPS_5") %>%
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area") %>%
  select(-(8:13)) %>%
  rename(Metro = "CBSA Title")


countyresponse %>% 
write.csv("countyresponse.csv")

tractresponse %>% 
  write.csv("tractresponse.csv")

cityresponse %>% 
  write.csv("cityresponse.csv")

metroresponse %>% 
  write.csv("metroresponse.csv")
  
  
  
  # data checked at: https://www.census.gov/cgi-bin/census2010/staterates.cgi

