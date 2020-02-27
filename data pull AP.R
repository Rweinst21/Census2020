library(censusapi)
library(tidyverse)
library(RODBC)
library(scales)
library(ggrepel)
library(sf)
library(tidycensus)
library(grid)


themeDC_map <- function() {
  require(ggthemes)
  theme_map() +
    theme(text = element_text(family = "Asap"), # Change to Asap if necessary  
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.caption = element_text(hjust = 0),
          strip.text = element_text(color = "grey20", face = "bold"),
          strip.background = element_blank())
}

DCcolor.p1darkblue <- "#002F45"
DCcolor.p1grayblue <- "#4A6576"
DCcolor.p1mediumblue <- "#6892AB"
DCcolor.p1skyblue <- "#ABE1FA"
DCcolor.p1lightskyblue <- "#D4EFFC"
DCcolor.p2blue <- "#166E95"
DCcolor.p2teal <- "#35A39B"
DCcolor.p2green <- "#5D893C"
DCcolor.p2limegreen <- "#9EB23B"
DCcolor.p2yellow <- "#F1C62B"
DCcolor.p2orange <- "#EF812C"
DCcolor.p2orangered <- "#E65E3F"
DCcolor.p2magenta <- "#E61C43"
DCcolor.p2violet <- "#B13F80"
DCcolor.p2purple <- "#71266E"

county_crosswalk <-read.csv("inputs/county_crosswalk.csv") %>% 
  mutate(county = as.numeric(as.character(county)))

np.pull.parish <- function(variables, names = variables, year=2018, survey = "acs/acs5"){
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

np.pull.tract <- function(variables, names = variables, year=2018, survey = "acs/acs5"){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  tract <- getCensus(name = survey, 
                     vintage = year, 
                     key = censuskey, 
                     vars = variables, 
                     region = "tract:*", 
                     regionin = "state:22+county:*")
  colnames(tract) <- c("state", "county", "tract", names) 
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



## 2018 Parish
raceparish.vars <-c("B03002_001E","B03002_003E","B03002_004E","B03002_005E","B03002_006E","B03002_007E","B03002_008E","B03002_009E","B03002_012E","B03002_001M","B03002_003M","B03002_004M","B03002_005M","B03002_006M","B03002_007M","B03002_008M","B03002_009M","B03002_012M")
raceparish.names <-c("popparish2018", "whiteparish2018","blackparish2018","nativeparish2018","asianparish2018","islandparish2018","otherparish2018","twoparish2018","hispparish2018","popparishMOE", "whiteparishMOE","blackparishMOE","nativeparishMOE","asianparishMOE","islandparishMOE","otherparishMOE","twoparishMOE","hispparishMOE")
raceparishRaw <- np.pull.parish(variables=raceparish.vars, names=raceparish.names) 
save(raceparishRaw, file = "inputs/raceparishRaw.RData")

## 2018 tract
race.vars <-c("B03002_001E","B03002_003E","B03002_004E","B03002_005E","B03002_006E","B03002_007E","B03002_008E","B03002_009E","B03002_012E","B03002_001M","B03002_003M","B03002_004M","B03002_005M","B03002_006M","B03002_007M","B03002_008M","B03002_009M","B03002_012M")
race.names <-c("pop2018", "white2018","black2018","native2018","asian2018","island2018","other2018","two2018","hisp2018","popMOE", "whiteMOE","blackMOE","nativeMOE","asianMOE","islandMOE","otherMOE","twoMOE","hispMOE")
racetractRaw <- np.pull.tract(variables=race.vars, names=race.names) 
save(racetractRaw, file = "inputs/racetractRaw.RData")


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
                              year = 2018, 
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
#  mutate(blackSIG = stattest(x=blackpct2010, y=blackpct2018, moey = blackMOEprop2018),
#         whiteSIG = stattest(x=whitepct2010, y=whitepct2018, moey = whiteMOEprop2018),
#         asianSIG =stattest(x=asianpct2010, y=asianpct2018, moey = asianMOEprop2018),
#         nativeSIG = stattest(x=nativepct2010, y=nativepct2018, moey=nativeMOEprop2018),
#         otherSIG = stattest(x=otherpct2010, y=otherpct2018, moey = otherMOEprop2018),
#         twoSIG = stattest( x=twopct2010, y=twopct2018, moey = twoMOEprop2018),
#         hispSIG = stattest(x=hisppct2010, y = hisppct2018, moey = hispMOEprop2018))





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
  select(county.x, 
         name, 
         blackpct2000, blackpct2010, blackpct2018, 
         whitepct2000, whitepct2010, whitepct2018,
         asianpct2000, asianpct2010, asianpct2018,
         nativepct2000, nativepct2010, nativepct2018,
         otherpct2000, otherpct2010, otherpct2018,
         twopct2000, twopct2010, twopct2018,
         hisppct2000, hisppct2010, hisppct2018)



blacktablepct <- racetablepct %>% 
  mutate(Race = "Black or African American") %>% 
  select(county.x, name, Race, blackpct2000, blackpct2010, blackpct2018)%>% 
  rename(y2000 = "blackpct2000",
         y2010 = "blackpct2010",
         y2018 = "blackpct2018") %>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

whitetablepct <- racetablepct %>% 
  mutate(Race = "White") %>% 
  select(county.x, name, Race, whitepct2000, whitepct2010, whitepct2018) %>% 
  rename(y2000 = "whitepct2000",
         y2010 = "whitepct2010",
         y2018 = "whitepct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

asiantablepct <- racetablepct %>% 
  mutate(Race = "Asian") %>% 
  select(county.x, name, Race, asianpct2000, asianpct2010, asianpct2018)%>% 
  rename(y2000 = "asianpct2000",
         y2010 = "asianpct2010",
         y2018 = "asianpct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

nativetablepct <- racetablepct %>% 
  mutate(Race = "American Indian") %>% 
  select(county.x, name, Race, nativepct2000, nativepct2010, nativepct2018)%>% 
  rename(y2000 = "nativepct2000",
         y2010 = "nativepct2010",
         y2018 = "nativepct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

othertablepct <- racetablepct %>% 
  mutate(Race = "Other") %>% 
  select(county.x, name, Race, otherpct2000, otherpct2010, otherpct2018) %>% 
  mutate(Race = "Other") %>% 
  rename(y2000 = "otherpct2000",
         y2010 = "otherpct2010",
         y2018 = "otherpct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

twotablepct <- racetablepct %>% 
  mutate(Race = "2 race categories") %>% 
  select(county.x, name, Race, twopct2000, twopct2010, twopct2018)%>% 
  rename(y2000 = "twopct2000",
         y2010 = "twopct2010",
         y2018 = "twopct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))

hisptablepct <- racetablepct %>% 
  mutate(Race = "Hispanic (any race)") %>% 
  select(county.x, name, Race, hisppct2000, hisppct2010, hisppct2018)%>% 
  rename(y2000 = "hisppct2000",
         y2010 = "hisppct2010",
         y2018 = "hisppct2018")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2018 = percent(y2018))


poptable <- bind_rows(blacktablepct , whitetablepct , asiantablepct , nativetablepct , twotablepct , hisptablepct , othertablepct ) %>% 
  select(county.x, name, Race, y2000, y2010, y2018) 

write.csv(poptable, "outputs/Parish_Demographics_Percent.csv")








#### FOr count




blacktable <- race %>% 
  mutate(Race = "Black or African American") %>% 
  select(county.x, name, Race, black2000, black2010, black2018)%>% 
  rename(y2000 = "black2000",
         y2010 = "black2010",
         y2018 = "black2018") 

whitetable <- race %>% 
  mutate(Race = "White") %>% 
  select(county.x, name, Race, white2000, white2010, white2018) %>% 
  rename(y2000 = "white2000",
         y2010 = "white2010",
         y2018 = "white2018") 

asiantable <- race %>% 
  mutate(Race = "Asian") %>% 
  select(county.x, name, Race, asian2000, asian2010, asian2018)%>% 
  rename(y2000 = "asian2000",
         y2010 = "asian2010",
         y2018 = "asian2018") 

nativetable <- race %>% 
  mutate(Race = "American Indian") %>% 
  select(county.x, name, Race, native2000, native2010, native2018)%>% 
  rename(y2000 = "native2000",
         y2010 = "native2010",
         y2018 = "native2018") 

othertable <- race %>% 
  mutate(Race = "Other") %>% 
  select(county.x, name, Race, other2000, other2010, other2018) %>% 
  rename(y2000 = "other2000",
         y2010 = "other2010",
         y2018 = "other2018")

twotable <- race %>% 
  mutate(Race = "2 race categories") %>% 
  select(county.x, name, Race, two2000, two2010, two2018)%>% 
  rename(y2000 = "two2000",
         y2010 = "two2010",
         y2018 = "two2018") 

hisptable <- race %>% 
  mutate(Race = "Hispanic (any race)") %>% 
  select(county.x, name, Race, hisp2000, hisp2010, hisp2018)%>% 
  rename(y2000 = "hisp2000",
         y2010 = "hisp2010",
         y2018 = "hisp2018") 




poptablecount <- bind_rows(blacktable , whitetable , asiantable , nativetable , twotable , hisptable , othertable ) %>% 
  select(county.x, name, Race, y2000, y2010, y2018) 

write.csv(poptablecount, "outputs/Parish_Demographics_Count.csv")




acs.parishes_sf <- get_acs(geography = "county",
                           key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                           variables = c(medincome = "B19013_001"), # This is median HH income
                           shift_geo = FALSE, # This downloads the geog in Albers
                           geometry = TRUE)%>%
  filter(str_sub(GEOID,1,2)=="22") 


parishes_map <- acs.parishes_sf %>% 
  mutate(County = as.numeric(str_sub(GEOID,3,5)))

CountforMap <- poptablecount %>% 
  rename(County = "county.x") %>% 
  mutate(d00_10 = y2010 - y2000,
         d10_18 = y2018 - y2010,
         d00_18 = y2018 - y2000)


###Black Map

blackmap <- CountforMap %>% 
  filter(Race == "Black or African American") %>% 
  mutate(ChangeType = "Neither",
         ChangeType = ifelse(d00_18 >1000 & d10_18 <1000, "Post-Katrina Growth", "Neither"),
         ChangeType = ifelse(d00_18 > 1000 & d10_18 >1000, "Post-Katrina and post-2010 Growth", ChangeType)) %>% 
  mutate(d00_18c = comma(d00_18),
         decd = paste("+", d00_18c, sep = "")) %>% 
  mutate(Description =(paste(name, decd, sep = "\n"))) %>% 
  filter(ChangeType != "Neither") 

write.csv(blackmap, "outputs/blacktable.csv")

blackmap$ChangeType <- factor(blackmap$ChangeType , levels=c("Post-Katrina Growth", "Post-Katrina and post-2010 Growth"))


Black_LAmap <- parishes_map %>%
  left_join(blackmap , by= "County")  %>%
  ggplot() +
  geom_sf(fill = "gray90") +
  geom_sf(aes(fill=ChangeType)) +
  ggrepel::geom_label_repel(aes(label=(Description), geometry = geometry), stat = "sf_coordinates", size =3, 
                           fontface = "bold", nudge_x = .05, nudge_y = .01, segment.size = .2, 
                           arrow = arrow(type = "closed", ends = "last", length = unit(0.01, "npc")), box.padding = unit(1, "lines"))+
  scale_fill_manual(values = c("Post-Katrina Growth" = "#9EB5C6", "Post-Katrina and post-2010 Growth" = "#6892AB"))+
 # ggrepel::geom_text_repel(aes(label=d00_18, geometry = geometry), stat = "sf_coordinates", size =3)+
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
 # scale_fill_manual(values = lacroix_palette("Berry", n = 11, type = "continuous"),  na.value = "gray70")+  #
  #scale_fill_paletteer_c("pals::ocean.curl", na.value = "gray70") + # "Redmonder::dPBlPuGn" "LaCroixColoR::Lemon" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
 # scale_fill_brewer(palette = "PRGn", direction = -1,  na.value = "gray70") +
  #scale_fill_viridis_d(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  coord_sf(datum = NA) +
  themeDC_map() +
  theme(legend.position = "right") +
  theme(title =element_text(size=12, face='bold'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  labs(title = "Parishes in Louisiana that gained over 1,000 Black residents from 2000 to 2018", fill = "When population growth occured")


#### Hispanic Map


hispanicmap <- CountforMap %>% 
  filter(Race == "Hispanic (any race)") %>% 
  mutate(Growth = "Neither",
         Growth = ifelse(d00_18 > 1000 & d00_18 < 2500, "1,000-2,499", "Neither"),
         Growth = ifelse(d00_18 > 2500 & d00_18 < 10000, "2,500-9,999", Growth),
         Growth = ifelse(d00_18 > 10000, "+10,000", Growth)) %>% 
  mutate(d00_18c = comma(d00_18),
         decd = paste("+", d00_18c, sep = "")) %>% 
  mutate(Description =(paste(name, decd, sep = "\n"))) %>% 
  filter(Growth != "Neither")

write.csv(hispanicmap, "outputs/hispanictable.csv")

hispanicmap$Growth <- factor(hispanicmap$Growth , levels=c("1,000-2,499", "2,500-9,999", "+10,000"))

Hispanic_LAmap <- parishes_map %>%
  left_join(hispanicmap , by= "County")  %>%
  ggplot() +
  geom_sf(fill = "gray90") +
  geom_sf(aes(fill=Growth)) +
  ggrepel::geom_label_repel(aes(label=(Description), geometry = geometry), stat = "sf_coordinates", size =3, 
                            fontface = "bold", nudge_x = .05, nudge_y = .01, segment.size = .2, 
                            arrow = arrow(type = "closed", ends = "last", length = unit(0.01, "npc")), box.padding = unit(1, "lines"))+
  scale_fill_manual(values = c("1,000-2,499" = "#C48471", "2,500-9,999" = "#B66653", "+10,000" = "#A84837"))+
  # ggrepel::geom_text_repel(aes(label=d00_18, geometry = geometry), stat = "sf_coordinates", size =3)+
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  # scale_fill_manual(values = lacroix_palette("Berry", n = 11, type = "continuous"),  na.value = "gray70")+  #
  #scale_fill_paletteer_c("pals::ocean.curl", na.value = "gray70") + # "Redmonder::dPBlPuGn" "LaCroixColoR::Lemon" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  # scale_fill_brewer(palette = "PRGn", direction = -1,  na.value = "gray70") +
  #scale_fill_viridis_d(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  coord_sf(datum = NA) +
  themeDC_map() +
  theme(legend.position = "right") +
  theme(title =element_text(size=12, face='bold'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  labs(title = "Parishes in Louisiana that gained over 1,000 Hispanic residents from 2000 to 2018", fill = "Growth 2000-2018")


### Asian


asianmap <- CountforMap %>% 
  filter(Race == "Asian") %>% 
  mutate(Growth = "None",
         Growth = ifelse(d00_18 > 300 & d00_18 < 1000, "300-999", Growth),
         Growth = ifelse(d00_18 > 1000 & d00_18 < 2500, "1,000-2,499", Growth),
         Growth = ifelse(d00_18 > 2500, "+2,500", Growth)) %>% 
  mutate(d00_18c = comma(d00_18),
         decd = paste("+", d00_18c, sep = "")) %>% 
  mutate(Description =(paste(name, decd, sep = "\n"))) %>% 
  filter(Growth != "None")

asianmap$Growth <- factor(asianmap$Growth , levels=c("300-999", "1,000-2,499", "+2,500"))

Asian_LAmap <- parishes_map %>%
  left_join(asianmap , by= "County")  %>%
  ggplot() +
  geom_sf(fill = "gray90") +
  geom_sf(aes(fill=Growth)) +
  scale_fill_manual(values = c("300-999" = "#D5A797", "1,000-2,499" = "#C48471", "+2,500" = "#B66653"))+
  ggrepel::geom_label_repel(aes(label=(Description), geometry = geometry), stat = "sf_coordinates", size =3, 
                            fontface = "bold", nudge_x = .05, nudge_y = .01, segment.size = .2, 
                            arrow = arrow(type = "closed", ends = "last", length = unit(0.01, "npc")), box.padding = unit(1, "lines"))+
  # ggrepel::geom_text_repel(aes(label=d00_18, geometry = geometry), stat = "sf_coordinates", size =3)+
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  # scale_fill_manual(values = lacroix_palette("Berry", n = 11, type = "continuous"),  na.value = "gray70")+  #
  #scale_fill_paletteer_c("pals::ocean.curl", na.value = "gray70") + # "Redmonder::dPBlPuGn" "LaCroixColoR::Lemon" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  # scale_fill_brewer(palette = "PRGn", direction = -1,  na.value = "gray70") +
  #scale_fill_viridis_d(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  coord_sf(datum = NA) +
  themeDC_map() +
  theme(legend.position = "right") +
  theme(title =element_text(size=12, face='bold'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  labs(title = "Parishes in Louisiana that gained over 300 Asian residents from 2000 to 2018", fill = "Growth 2000-2018")

write.csv(asianmap, "outputs/asiantable.csv")



## American Indian Map

AImap <- CountforMap %>% 
  filter(Race == "American Indian") %>% 
  mutate(Growth = "None",
         Growth = ifelse(d00_18 > 300, "300+", Growth)) %>% 
  mutate(d00_18c = comma(d00_18),
         decd = paste("+", d00_18c, sep = "")) %>% 
  mutate(Description =(paste(name, decd, sep = "\n"))) %>% 
  filter(Growth != "None")

  write.csv(AImap, "outputs/AItable.csv")

AI_LAmap <- parishes_map %>%
  left_join(AImap , by= "County")  %>%
  ggplot() +
  geom_sf(fill = "gray90")+
  geom_sf(aes(fill=Growth)) +
  scale_fill_manual(values = c("300+" = "#35A39B"))+
  ggrepel::geom_label_repel(aes(label=(Description), geometry = geometry), stat = "sf_coordinates", size =3, 
                            fontface = "bold", nudge_x = .05, nudge_y = .01, segment.size = .2, 
                            arrow = arrow(type = "closed", ends = "last", length = unit(0.01, "npc")), box.padding = unit(1, "lines"))+
  # ggrepel::geom_text_repel(aes(label=d00_18, geometry = geometry), stat = "sf_coordinates", size =3)+
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  # scale_fill_manual(values = lacroix_palette("Berry", n = 11, type = "continuous"),  na.value = "gray70")+  #
  #scale_fill_paletteer_c("pals::ocean.curl", na.value = "gray70") + # "Redmonder::dPBlPuGn" "LaCroixColoR::Lemon" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  # scale_fill_brewer(palette = "PRGn", direction = -1,  na.value = "gray70") +
  #scale_fill_viridis_d(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  coord_sf(datum = NA) +
  themeDC_map() +
  theme(title =element_text(size=12, face='bold'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  theme(legend.position = "right") +
  labs(title = "Parishes in Louisiana that gained more than 300 Native American residents from 2000 to 2018", fill = "Growth 2000-2018")



#### No longer need

### 2018




raceparish2018 <- raceparishRaw %>% 
  mutate(county = as.numeric(county)) %>% 
  merge(., county_crosswalk, by = "county") 

racetract2018 <- racetractRaw %>% 
  mutate(county = as.numeric(county)) %>% 
  merge(., county_crosswalk, by = "county") %>% 
  merge(., raceparish2018, by = "county") %>% 
  mutate(   
    blackpct2018 = black2018 / blackparish2018,
  #  whitepct2018 = white2018 / whiteparish2018,
    asianpct2018 = asian2018 / asianparish2018,
    nativepct2018 = native2018 / nativeparish2018,
   # twopct2018 = two2018 / twoparish2018,
    hisppct2018 = hisp2018 / hispparish2018,
  #  other2018 = island2018 + other2018,
  #  otherpct2018 = other2018 / (otherparish2018 + islandparish2018),
    
  #  othermoeagg2018 = moeagg(cbind(islandMOE, otherMOE)),
    popMOE = ifelse(popMOE < 0, 0, popMOE),
    hispMOE = ifelse(hispMOE < 0, 0, hispMOE),
    hispparishMOE = ifelse(hispparishMOE < 0, 0, hispparishMOE),
    asianpct2018 = ifelse(asianpct2018 == "NaN", 0, asianpct2018),
    nativepct2018 = ifelse(nativepct2018 == "NaN", 0, nativepct2018),
    
    blackMOEprop2018 = moeprop(y= blackparish2018, moex = blackMOE, moey = blackparishMOE, p = blackpct2018),
   # whiteMOEprop2018 = moeprop(y= whiteparish2018, moex = whiteMOE, moey = whiteparishMOE, p = whitepct2018),
    asianMOEprop2018 = moeprop(y= asianparish2018, moex = asianMOE, moey = asianparishMOE, p = asianpct2018),
    nativeMOEprop2018 = moeprop(y= nativeparish2018, moex = nativeMOE, moey = nativeparishMOE, p = nativepct2018),
  #  otherMOEprop2018 = moeprop(y= otherparish2018, moex = othermoeagg2018, moey = otherparishMOE, p = otherpct2018),
   # twoMOEprop2018 = moeprop(y= twoparish2018, moex = twoMOE, moey = twoparishMOE, p = twopct2018),
    hispMOEprop2018 = moeprop(y= hispparish2018, moex = hispMOE, moey = hispparishMOE, p = hisppct2018)
  
    )

    
racetractpropprint <- racetract2018 %>% 
  select(name.y, tract, blackpct2018, blackMOEprop2018, hisppct2018, hispMOEprop2018, asianpct2018, asianMOEprop2018, nativepct2018, nativeMOEprop2018 )


write.csv(racetractpropprint, "outputs/raceprop.csv")
