library(tidyverse)
library(tm)

census2000raw <- read.csv("inputs/co-est00int-alldata-22.csv") 

census2000 <- census2000raw %>% 
  filter(AGEGRP == "99") %>% 
  filter(YEAR == "1") %>% 
  mutate(Year = 2000,
         Pop = as.numeric(as.character(TOT_POP)), 
         White = as.numeric(as.character(NHWA_MALE)) + as.numeric(as.character(NHWA_FEMALE)),  
         Black = as.numeric(as.character(NHBA_MALE)) + as.numeric(as.character(NHBA_FEMALE)), 
         AI = as.numeric(as.character(NHIA_MALE)) + as.numeric(as.character(NHIA_FEMALE)),
         Asian =  as.numeric(as.character(NHAA_MALE)) +  as.numeric(as.character(NHAA_FEMALE)),
         Hawaiian = as.numeric(as.character(NHNA_MALE)) + as.numeric(as.character(NHNA_FEMALE)),
         Two =   as.numeric(as.character(NHTOM_MALE)) + as.numeric(as.character(NHTOM_FEMALE)),
         Hispanic = as.numeric(as.character(H_MALE)) + as.numeric(as.character(H_FEMALE)),
         Year = as.character(Year)) %>% 
  select(CTYNAME, Year, Pop, White, Black, AI, Asian, Hawaiian, Two, Hispanic)

compare2010_2018raw <- read.csv("inputs/compare2010_2018.csv")

compare2010_2018h <- compare2010_2018raw %>% 
  filter(Year.display.label == "April 1, 2010 Census" | Year.display.label == "1-Jul-18") %>% 
  filter(Sex.id == "totsex") %>% 
  filter(Hisp.id == "hisp") %>% 
  rename(Hispanic = "totpop") %>% 
  select(Year.id, GEO.display.label, Hispanic)

compare2010_2018t <- compare2010_2018raw %>% 
  filter(Year.display.label == "April 1, 2010 Census" | Year.display.label == "1-Jul-18") %>% 
  filter(Sex.id == "totsex") %>% 
  filter(Hisp.id == "tothisp") %>% 
  rename(Pop = "totpop") %>% 
  select(Year.id, GEO.display.label, Pop)

compare2010_2018 <- compare2010_2018raw %>% 
  filter(Year.display.label == "April 1, 2010 Census" | Year.display.label == "1-Jul-18") %>% 
  filter(Sex.id == "totsex") %>% 
  filter(Hisp.id == "nhisp") %>% 
  select(Year.id, GEO.display.label, wa, ba, ia, aa, na, tom) %>% 
  merge(., compare2010_2018h, by = c("Year.id", "GEO.display.label")) %>% 
  merge(., compare2010_2018t, by = c("Year.id", "GEO.display.label")) %>% 
  rename(White = "wa",
         Black = "ba",
         AI = "ia",
         Asian = "aa",
         Hawaiian = "na",
         Two = "tom",
         Year = "Year.id",
         CTYNAME = "GEO.display.label") %>% 
  mutate(White = as.numeric(as.character(White)),
         Black = as.numeric(as.character(Black)),
         AI = as.numeric(as.character(AI)),
         Asian = as.numeric(as.character(Asian)),
         Hawaiian = as.numeric(as.character(Hawaiian)),
         Two = as.numeric(as.character(Two)),
         Hispanic = as.numeric(as.character(Hispanic)),
         CTYNAME = as.character(CTYNAME),
         Year = as.character(Year),
         Pop = as.numeric(as.character(Pop))) %>% 
  substr(CTYNAME, 1, -1) %>% 
  bind_rows(census2000) %>% 
  pivot_longer(-CITYNAME, names_to = "Race", values_to = "Percent")



write.csv(compare2010_2018, file = "outputs/Attempt2_count.csv")


compare2010_2018_percent <- compare2010_2018 %>% 
  mutate(Whitepct = White/Pop,
         Blackpct = Black/Pop,
         AIpct = AI/Pop,
         Asianpct = Asian/Pop,
         Hawaiianpct = Hawaiian/Pop, 
         Twopct = Two/Pop,
         Hispanicpct = Hispanic / Pop) %>% 
  select(CTYNAME, Year, Whitepct, Blackpct, AIpct, Asianpct, Hawaiianpct, Twopct, Hispanicpct)

write.csv(compare2010_2018_percent, file = "outputs/Attempt2_percent.csv")
