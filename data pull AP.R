library(censusapi)
library(tidyverse)
library(RODBC)
library(scales)

county_crosswalk <-read.csv("inputs/county_crosswalk.csv") %>% 
  mutate(county = as.numeric(as.character(county)))

np.pull.parish <- function(variables, names = variables, year=2017, survey = "acs/acs5"){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  tract <- getCensus(name = survey, 
                     vintage = year, 
                     key = censuskey, 
                     vars = variables, 
                     region = "county:*", 
                     regionin = "state:22")
  colnames(tract) <- c("state", "county", names) 
  return(tract)
}

##calculates MOE for aggregated estimates
##moe = sqrt(sum(estimateMOE^2))
##input: dataframe of estimates' MOEs (i.e. use cbind)
##output: column of MOEs
moeagg <- function(estimateMOE){  
  squares <- matrix(0, dim(estimateMOE)[1], dim(estimateMOE)[2])
  for(i in 1:dim(estimateMOE)[2]){
    squares[,i] <- t(estimateMOE[,i]*estimateMOE[,i])
  }
  sumsquares <- apply(squares, 1, sum)
  return(sqrt(sumsquares))
}

##calculates MOE for proportions
##p = x/y
##moe = sqrt(moex^2 - p^2 * moey^2)/y
##for ratio, +
##input: columns of measures y, moex, moey, p
moeprop <- function(y, moex, moey, p){
  mp <- matrix(0, length(y))
  for(i in 1:length(y)){
    if((moex[i]*moex[i] - p[i]*p[i]*moey[i]*moey[i]) < 0){
      mp[i] <- (sqrt(moex[i]*moex[i] + p[i]*p[i]*moey[i]*moey[i]))/y[i]
    } else {
      mp[i] <- (sqrt(moex[i]*moex[i] - p[i]*p[i]*moey[i]*moey[i]))/y[i]
    }
  }
  return(mp)
}

##stat testing for 2000 vs 201* data
##input: columns of estimates and their MOEs (zeros for Census 2000)
##output: column of yes or no if significant
stattest <- function(x, moex = matrix(0, length(x)), y, moey){
  significant <- matrix(0, length(x))
  v <- abs((x-y)/sqrt((moex/1.645)^2+(moey/1.645)^2))
  significant <- ifelse(v>1.645,"yes","no")
  return(as.list(significant))
}



## 2017
#race.vars <-c("B03002_001E","B03002_003E","B03002_004E","B03002_005E","B03002_006E","B03002_007E","B03002_008E","B03002_009E","B03002_012E","B03002_001M","B03002_003M","B03002_004M","B03002_005M","B03002_006M","B03002_007M","B03002_008M","B03002_009M","B03002_012M")
#race.names <-c("pop2017", "white2017","black2017","native2017","asian2017","island2017","other2017","two2017","hisp2017","popMOE", "whiteMOE","blackMOE","nativeMOE","asianMOE","islandMOE","otherMOE","twoMOE","hispMOE")
#raceRaw <- np.pull.parish(variables=race.vars, names=race.names) 
#save(raceRaw, file = "inputs/raceRaw.RData")


race2010.vars <- c("P005001", "P005003","P005004","P005005","P005006","P005007","P005008","P005009","P004003")
race2010.names <-c("pop2010", "white2010", "black2010", "native2010", "asian2010", "island2010", "other2010", "two2010", "hisp2010")
race2010Raw <- np.pull.parish(variables=race2010.vars, names=race2010.names, year = 2010, survey = "dec/sf1") 

## 2000
race2000.vars <-c("P008001","P008003","P008004","P008005","P008006","P008007","P008008","P008009","P008010")
race2000.names <-c("pop2000","white2000","black2000","native2000","asian2000","island2000","other2000","two2000","hisp2000")
race2000Raw <- np.pull.parish(variables=race2000.vars, names=race2000.names, year = 2000, survey = "sf1") 
#save(race2000Raw, file = "inputs/race2000Raw.RData")



####### PEP PULL 2018


allparishesRaw <- pullDataPEP(charagegroupsVars, 
                              api = "pep/charagegroups", 
                              year = 2017, 
                              counties = mycounties)
save(allparishesRaw, file = "inputs/allparishesRaw.Rdata")


raceCodeName <- 
  c("Total",
    "White alone",
    "Black or African American alone",
    "American Indian and Alaska Native alone",
    "Asian alone",
    "Native Hawaiian and Other Pacific Islander alone",
    "Two or more races",
    "White combo",
    "Black or African American combo",
    "American Indian and Alaska Native combo",
    "Asian combo",
    "Native Hawaiian and Other Pacific Islander combo")
raceCode <- data.frame(RACE=as.character(0:11),raceCodeName)

hispCodeName <- c("Total","Not Hispanic","Hispanic")
hispCode <- data.frame(HISP=as.character(0:2),hispCodeName)


popestVars <- c("POP","DATE_DESC","DATE_CODE", "GEONAME", "HISP", "RACE")
popestVars2000 <- c("POP","DATE_DESC","DATE_", "GEONAME", "HISP")
listCensusMetadata("pep/int_charagegroups", vintage = 2000, type = "variables")
race2018Raw <- getCensus(name = "pep/charagegroups", # most recent
                         vintage = 2018, 
                         key = "530ce361defc2c476e5b5d5626d224d8354b9b9a", 
                         vars = popestVars, 
                         region = "county: *", 
                         regionin = "state:22") %>% 
  filter(DATE_DESC == "7/1/2018 population estimate")

race2018 <- race2018Raw %>% 
  left_join(raceCode) %>%
  left_join(hispCode) %>%
  mutate(place = GEONAME) %>% 
  mutate(POP = as.numeric(POP), 
         place = GEONAME,
         place = ifelse(!is.na(county),
                        str_sub(GEONAME, 1, nchar(GEONAME) - 18),
                        GEONAME)) %>% 
  merge(., county_crosswalk, by = "county") %>% 
  select(name, DATE_DESC, hispCodeName,  raceCodeName, POP) %>% 
  rename(hisp = hispCodeName,
         race = raceCodeName, 
         population = POP,
         date = DATE_DESC) %>% 
  #filter(hisp != "Hispanic" & race != "Total" ) %>% 
  filter(!grepl("combo", race)) %>% 
  mutate(raceSimple = "other2018", # make variable base on race alone that matches Who Lives races. 
         raceSimple = ifelse(race == "Total" & hisp == "Total", "pop2018", raceSimple),
         raceSimple = ifelse(race == "White alone" & hisp == "Not Hispanic", "white2018", raceSimple),
         raceSimple = ifelse(race == "Black or African American alone" & hisp == "Not Hispanic", "black2018", raceSimple),
         raceSimple = ifelse(race == "Asian alone" & hisp == "Not Hispanic", "asian2018", raceSimple),
         raceSimple = ifelse(race == "American Indian and Alaska Native alone" & hisp == "Not Hispanic", "native2018", raceSimple),
         raceSimple = ifelse(race == "Two or more races" & hisp == "Not Hispanic", "two2018", raceSimple),
         raceSimple = ifelse(race == "Total" & hisp == "Hispanic", "hisp2018", raceSimple),
         raceSimple = ifelse(hisp == "Hispanic" & race != "Total", "NA", raceSimple),
         raceSimple = ifelse(hisp == "Total" & race != "Total", "NA", raceSimple),
         raceSimple = ifelse(hisp == "Not Hispanic" & race == "Total", "NA", raceSimple)) %>% 
  filter(raceSimple != "NA") %>% 
  select(name, population, raceSimple) %>% 
  group_by(name, raceSimple) %>% 
  summarise_all(funs(sum)) %>% 
  pivot_wider(., id_cols = name, values_from = population, names_from= raceSimple) %>% 
  mutate(   
    blackpct2018 = black2018 / pop2018,
    whitepct2018 = white2018 / pop2018,
    asianpct2018 = asian2018 / pop2018,
    nativepct2018 = native2018 / pop2018,
    twopct2018 = two2018 / pop2018,
    hisppct2018 = hisp2018 / pop2018,
    otherpct2018 = other2018 /pop2018)



race2010 <- race2010Raw %>% 
  mutate(county = as.numeric(county)) %>% 
  merge(., county_crosswalk, by = "county") %>% 
  mutate(   
    blackpct2010 = black2010 / pop2010,
    whitepct2010 = white2010 / pop2010,
    asianpct2010 = asian2010 / pop2010,
    nativepct2010 = native2010 / pop2010,
    twopct2010 = two2010 / pop2010,
    hisppct2010 = hisp2010 / pop2010,
    other2010 = island2010 + other2010,
    otherpct2010 = other2010 /pop2010)


race2000 <- race2000Raw %>% 
  mutate(county = as.numeric(county)) %>% 
  merge(., county_crosswalk, by = "county") %>% 
  mutate(   
    blackpct2000 = black2000 / pop2000,
    whitepct2000 = white2000 / pop2000,
    asianpct2000 = asian2000 / pop2000,
    nativepct2000 = native2000 / pop2000,
    twopct2000 = two2000 / pop2000,
    hisppct2000 = hisp2000 / pop2000,
    other2000 = island2000 + other2000,
    otherpct2000 = other2000 /pop2000)

race <-merge(race2000, race2010, by = "name") %>% 
  merge(., race2018, by = "name")
#  mutate(blackSIG = stattest(x=blackpct2010, y=blackpct2017, moey = blackMOEprop2017),
#         whiteSIG = stattest(x=whitepct2010, y=whitepct2017, moey = whiteMOEprop2017),
#         asianSIG =stattest(x=asianpct2010, y=asianpct2017, moey = asianMOEprop2017),
#         nativeSIG = stattest(x=nativepct2010, y=nativepct2017, moey=nativeMOEprop2017),
#         otherSIG = stattest(x=otherpct2010, y=otherpct2017, moey = otherMOEprop2017),
#         twoSIG = stattest( x=twopct2010, y=twopct2017, moey = twoMOEprop2017),
#         hispSIG = stattest(x=hisppct2010, y = hisppct2017, moey = hispMOEprop2017))





tablepercent2018 <-  race2018 %>% 
  dplyr::rename("Black or African American" = "blackpct2018",
                "White" = "whitepct2018",
                "Asian" = "asianpct2018",
                "American Indian" = "nativepct2018", 
                "Hispanic (any race)" = "hisppct2018",
                "Other" = "otherpct2018",
                "2 race categories" = "twopct2018") %>% 
  select(name, "Black or African American", "White", "Asian", "American Indian", "Hispanic (any race)",  "2 race categories", "Other") %>% 
  pivot_longer(-name, names_to = "Race", values_to = "y2018") %>% 
  mutate(y2018 = percent(y2018)) %>% 
  rename("name" = "name")

tablecount2018 <-  race2018 %>% 
  dplyr::rename("Black or African American" = "black2018",
                "White" = "white2018",
                "Asian" = "asian2018",
                "American Indian" = "native2018", 
                "Hispanic (any race)" = "hisp2018",
                "Other" = "other2018",
                "2 race categories" = "two2018") %>% 
  dplyr::select("name", "Black or African American", "White", "Asian", "American Indian", "Hispanic (any race)",  "2 race categories", "Other") %>% 
  pivot_longer(-name, names_to = "Race", values_to = "y2018") %>% 
  mutate(y2018 = comma(y2018)) %>% 
  rename("name" = "name")




### Adding all together and printing


poptableCount <- bind_rows(blacktable , whitetable , asiantable , nativetable , twotable , hisptable , othertable ) %>% 
  select(name, Race, y2000, y2010) %>% 
  merge(., tablecount2018, by = c("name", "Race"))

write.csv(poptableCount, "outputs/Parish_Demographics_Count.csv")














racetablepct <- race %>% 
  select(name, 
         blackpct2000, blackpct2010, blackpct2018, 
         whitepct2000, whitepct2010, whitepct2018,
         asianpct2000, asianpct2010, asianpct2018,
         nativepct2000, nativepct2010, nativepct2018,
         otherpct2000, otherpct2010, otherpct2018,
         twopct2000, twopct2010, twopct2018,
         hisppct2000, hisppct2010, hisppct2018)



blacktablepct <- racetablepct %>% 
  mutate(Race = "Black or African American") %>% 
  select(name, Race, blackpct2000, blackpct2010, blackpct2018)%>% 
  rename(y2000 = "blackpct2000",
         y2010 = "blackpct2010",
         y2018 = "blackpct2018") %>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

whitetablepct <- racetablepct %>% 
  mutate(Race = "White") %>% 
  select(name, Race, whitepct2000, whitepct2010, whitepct2018) %>% 
  rename(y2000 = "whitepct2000",
         y2010 = "whitepct2010",
         y2018 = "whitepct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

asiantablepct <- racetablepct %>% 
  mutate(Race = "Asian") %>% 
  select(name, Race, asianpct2000, asianpct2010, asianpct2018)%>% 
  rename(y2000 = "asianpct2000",
         y2010 = "asianpct2010",
         y2018 = "asianpct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

nativetablepct <- racetablepct %>% 
  mutate(Race = "American Indian") %>% 
  select(name, Race, nativepct2000, nativepct2010, nativepct2018)%>% 
  rename(y2000 = "nativepct2000",
         y2010 = "nativepct2010",
         y2018 = "nativepct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

othertablepct <- racetablepct %>% 
  mutate(Race = "Other") %>% 
  select(name, Race, otherpct2000, otherpct2010, otherpct2018) %>% 
  mutate(Race = "Other") %>% 
  rename(y2000 = "otherpct2000",
         y2010 = "otherpct2010",
         y2018 = "otherpct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

twotablepct <- racetablepct %>% 
  mutate(Race = "2 race categories") %>% 
  select(name, Race, twopct2000, twopct2010, twopct2018)%>% 
  rename(y2000 = "twopct2000",
         y2010 = "twopct2010",
         y2018 = "twopct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

hisptablepct <- racetablepct %>% 
  mutate(Race = "Hispanic (any race)") %>% 
  select(name, Race, hisppct2000, hisppct2010, hisppct2018)%>% 
  rename(y2000 = "hisppct2000",
         y2010 = "hisppct2010",
         y2018 = "hisppct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))


poptable <- bind_rows(blacktablepct , whitetablepct , asiantablepct , nativetablepct , twotablepct , hisptablepct , othertablepct ) %>% 
  select(name, Race, y2000, y2010, y2018) 

write.csv(poptable, "outputs/Parish_Demographics_Percent.csv")








#### FOr count




blacktable <- race %>% 
  mutate(Race = "Black or African American") %>% 
  select(name, Race, black2000, black2010, black2018)%>% 
  rename(y2000 = "black2000",
         y2010 = "black2010",
         y2018 = "black2018") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2018 = comma(y2018))

whitetable <- race %>% 
  mutate(Race = "White") %>% 
  select(name, Race, white2000, white2010, white2018) %>% 
  rename(y2000 = "white2000",
         y2010 = "white2010",
         y2018 = "white2018") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2018 = comma(y2018))

asiantable <- race %>% 
  mutate(Race = "Asian") %>% 
  select(name, Race, asian2000, asian2010, asian2018)%>% 
  rename(y2000 = "asian2000",
         y2010 = "asian2010",
         y2018 = "asian2018") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2018 = comma(y2018))

nativetable <- race %>% 
  mutate(Race = "American Indian") %>% 
  select(name, Race, native2000, native2010, native2018)%>% 
  rename(y2000 = "native2000",
         y2010 = "native2010",
         y2018 = "native2018") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2018 = comma(y2018))

othertable <- race %>% 
  mutate(Race = "Other") %>% 
  select(name, Race, other2000, other2010, other2018) %>% 
  rename(y2000 = "other2000",
         y2010 = "other2010",
         y2018 = "other2018") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2018 = comma(y2018))

twotable <- race %>% 
  mutate(Race = "2 race categories") %>% 
  select(name, Race, two2000, two2010, two2018)%>% 
  rename(y2000 = "two2000",
         y2010 = "two2010",
         y2018 = "two2018") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2018 = comma(y2018))

hisptable <- race %>% 
  mutate(Race = "Hispanic (any race)") %>% 
  select(name, Race, hisp2000, hisp2010, hisp2018)%>% 
  rename(y2000 = "hisp2000",
         y2010 = "hisp2010",
         y2018 = "hisp2018") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2018 = comma(y2018))




poptablecount <- bind_rows(blacktable , whitetable , asiantable , nativetable , twotable , hisptable , othertable ) %>% 
  select(name, Race, y2000, y2010, y2018) 

write.csv(poptablecount, "outputs/Parish_Demographics_Count.csv")


















#### No longer need

### 2017

#race2017 <- raceRaw %>% 
#  mutate(county = as.numeric(county)) %>% 
#  merge(., county_crosswalk, by = "county") %>% 
#  mutate(   
#    blackpct2017 = black2017 / pop2017,
#    whitepct2017 = white2017 / pop2017,
#    asianpct2017 = asian2017 / pop2017,
#    nativepct2017 = native2017 / pop2017,
#    twopct2017 = two2017 / pop2017,
#    hisppct2017 = hisp2017 / pop2017,
#    other2017 = island2017 + other2017,
#    otherpct2017 = other2017 /pop2017,
#    
#    othermoeagg2017 = moeagg(cbind(islandMOE, otherMOE)),
#    popMOE = ifelse(popMOE < 0, 0, popMOE),
#    hispMOE = ifelse(hispMOE < 0, 0, hispMOE),
#    
#    blackMOEprop2017 = moeprop(y= pop2017, moex = blackMOE, moey = popMOE, p = blackpct2017),
#    whiteMOEprop2017 = moeprop(y= pop2017, moex = whiteMOE, moey = popMOE, p = whitepct2017),
#    asianMOEprop2017 = moeprop(y= pop2017, moex = asianMOE, moey = popMOE, p = asianpct2017),
#    nativeMOEprop2017 = moeprop(y= pop2017, moex = nativeMOE, moey = popMOE, p = nativepct2017),
#    otherMOEprop2017 = moeprop(y= pop2017, moex = othermoeagg2017, moey = popMOE, p = otherpct2017),
#    twoMOEprop2017 = moeprop(y= pop2017, moex = twoMOE, moey = popMOE, p = twopct2017),
#    hispMOEprop2017 = moeprop(y= pop2017, moex = hispMOE, moey = popMOE, p = hisppct2017)
#  
#    )
#

