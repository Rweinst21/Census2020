library(tidyverse)

responserates <-  read.delim("Inputs/participationrates2010.txt", sep = "|")

countyresponse <- responserates %>% 
  rename(Geography = "American.Indian.Area..Reservation.") %>% 
  rename(County = "Big.Lagoon") %>% 
  rename(GEOID5 = "X0240") %>% 
  rename(Countyresponse2000 = "X0.60") %>% 
  rename(Countyresponse2010 = "X0.00") %>% 
  filter(Geography == "County ") %>% 
  mutate(County = as.character(County),
         GEOID5 = as.character(GEOID5)) %>% 
  filter(str_detect(County, ", LA")) %>%
  select(GEOID5, County, Geography, Countyresponse2000, Countyresponse2010)


countyforjoin <- countyresponse %>% 
  select(GEOID5, County)

tractresponse <- responserates %>% 
  rename(Geography = "American.Indian.Area..Reservation.") %>% 
  rename(County = "Big.Lagoon") %>% 
  rename(GEOID11 = "X0240") %>% 
  rename(response2000 = "X0.60") %>% 
  rename(response2010 = "X0.00") %>% 
  filter(Geography == "Tract ") %>% 
  mutate(GEOID11 = as.character(GEOID11)) %>% 
  mutate(GEOID2 = substring(GEOID11, 1, 2),
         Geography = as.character(Geography)) %>%  
  filter(GEOID2 == "22") %>% 
  select(GEOID11, GEOID2, Geography, response2000, response2010)  

countyresponse %>% 
write.csv("countyresponse.csv")

tractresponse %>% 
  write.csv("tractresponse.csv")
  
  
  
  # data checked at: https://www.census.gov/cgi-bin/census2010/staterates.cgi

