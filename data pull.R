library(censusapi)
library(tidyverse)
library(RODBC)



###The variables we are using for your reference are 
#found here:https://datacenterresearch.sharepoint.com/:x:/g/EURVXu8-_rNJgB2x-nDadH0BYK8bUfMV_BZqWsmrjmyZqg?rtime=Hh-7zc2S10g



#### set up IDS 

IDS <- odbcConnect("DC2 IDS", uid = "rweinstein", pwd = "December20!")

#### pull census data from IDS
censusData <- sqlQuery(IDS, "SELECT * FROM acsraw.NBHDProfiles WHERE Year = 2017")

#### MAP ####  

tracts.la <- sf::st_read(here("inputs/tl_2010_22_tract10/tl_2010_22_tract10.shp"))

water.051 <- sf::st_read(here("inputs/tl_2018_22051_areawater/tl_2018_22051_areawater.shp"))
water.057 <- sf::st_read(here("inputs/tl_2018_22057_areawater/tl_2018_22057_areawater.shp"))
water.071 <- sf::st_read(here("inputs/tl_2018_22071_areawater/tl_2018_22071_areawater.shp"))
water.075 <- sf::st_read(here("inputs/tl_2018_22075_areawater/tl_2018_22075_areawater.shp"))
water.087 <- sf::st_read(here("inputs/tl_2018_22087_areawater/tl_2018_22087_areawater.shp"))
water.089 <- sf::st_read(here("inputs/tl_2018_22089_areawater/tl_2018_22089_areawater.shp"))


#### FUNCTIONS ####
np.pull <- function(variables, names = variables, year=2017, survey = "acs/acs5"){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  tract <- getCensus(name = survey, 
                     vintage = year, 
                     key = censuskey, 
                     vars = variables, 
                     region = "tract:*", 
                     regionin = "state:22+county:071") %>% select(-state, -county)
  colnames(tract) <- c("place", names) 
  return(tract)
}

np.pull15 <- function(variables, names = variables, year=2015, survey = "acs/acs5"){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  tract <- getCensus(name = survey, 
                     vintage = year, 
                     key = censuskey, 
                     vars = variables, 
                     region = "tract:*", 
                     regionin = "state:22+county:071") %>% select(-state, -county)
  colnames(tract) <- c("place", names) 
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
##output: column of MOEs
moeprop <- function(y, moex, moey, p, ratio = FALSE){
  mp <- matrix(0, length(y))
  for(i in 1:length(y)){
    ifelse(is.nan(y[i])|is.nan(p[i]),
           mp[i] <- NaN,
           if((moex[i]*moex[i] - p[i]*p[i]*moey[i]*moey[i]) < 0 | ratio == TRUE){
             mp[i] <- (sqrt(moex[i]*moex[i] + p[i]*p[i]*moey[i]*moey[i]))/y[i]
           } else {
             mp[i] <- (sqrt(moex[i]*moex[i] - p[i]*p[i]*moey[i]*moey[i]))/y[i]
           })
  }
  return(mp)
}


#### LOW RESPONSE SCORE ####

### Low Response Score available in Census Planning Database
### only years 15-16 available in API
### download tract-level spreadsheet in zip file below
### https://www2.census.gov/adrm/PDB/2019/pdb2019trv3_us.zip

#C:/Users/jenna/Documents/pdb2019censusLRS.csv

LRS <- read_csv('C:/Users/jenna/Documents/pdb2019censusLRS.csv')%>%
  select(GEOID = GIDTR, State, State_name, County, County_name, LAND_AREA, Tot_Population_ACS_13_17,Tot_Population_ACSMOE_13_17, Low_Response_Score) %>%
  filter(State == "22")
write.csv(LRS, file = "LRS2020.csv")


###### DATA IN IDS PULL #####

#### YOUNG CHILDREN ####

### Count of children 4 and under
ageRaw <- censusData %>%
  select(PlaceCode,pop, popMOE,hh,hhMOE, m_under5,f_under5,m_under5MOE,f_under5MOE)

#ageRaw[ageRaw == -555555555] <- 0  
age <- ageRaw %>%
  mutate(t4less = (m_under5 + f_under5), 
         t4lessmoeagg = sqrt(m_under5MOE^2 + f_under5MOE^2),
         tract = str_pad(PlaceCode,6, pad= "0"),
         GEOID = paste0("22071", tract)) %>%
  #mutate(parish = "Orleans") %>%
  select(GEOID, tract, t4less, t4lessmoeagg)
write.csv(age,file = "below52020.csv")

#### LOW ENGLISH PROFICIENCY ####

# The most straighforward measure of English ability is probably the English ability question from the ACS.
# 
# A paper published at the Census Bureau reports "that the English-ability question, despite being a self-assessment, does a good job of measuring English ability."
# https://www.census.gov/newsroom/blogs/research-matters/2015/10/how-well-do-you-speak-english-assessing-the-validity-of-the-american-community-survey-english-ability-question.html

# In neighborhood profiles, we group together people who speak english "well" or "very well" and people who speak English "not well" or "not at all," with the latter group representing people for whom English is a barrier.  

langRaw <- censusData %>%
  select("PlaceCode", "langtot", "nat.onlyeng","nat.span.vwell","nat.span.well","nat.span.notwell","nat.span.notatall","nat.euro.vwell","nat.euro.well","nat.euro.notwell","nat.euro.notatall","nat.asian.vwell","nat.asian.well","nat.asian.notwell","nat.asian.notatall","nat.other.vwell","nat.other.well",
          "nat.other.notwell","nat.other.notatall","forbor.onlyeng","forbor.span.vwell","forbor.span.well","forbor.span.notwell","forbor.span.notatall","forbor.euro.vwell","forbor.euro.well","forbor.euro.notwell","forbor.euro.notatall","forbor.asian.vwell","forbor.asian.well",
          "forbor.asian.notwell","forbor.asian.notatall","forbor.other.vwell","forbor.other.well","forbor.other.notwell","forbor.other.notatall","langtotMOE","nat.onlyengMOE","nat.span.vwellMOE","nat.span.wellMOE","nat.span.notwellMOE","nat.span.notatallMOE","nat.euro.vwellMOE",
          "nat.euro.wellMOE","nat.euro.notwellMOE","nat.euro.notatallMOE","nat.asian.vwellMOE","nat.asian.wellMOE","nat.asian.notwellMOE","nat.asian.notatallMOE","nat.other.vwellMOE","nat.other.wellMOE","nat.other.notwellMOE","nat.other.notatallMOE","forbor.onlyengMOE",
          "forbor.span.vwellMOE","forbor.span.wellMOE","forbor.span.notwellMOE","forbor.span.notatallMOE","forbor.euro.vwellMOE","forbor.euro.wellMOE","forbor.euro.notwellMOE","forbor.euro.notatallMOE","forbor.asian.vwellMOE","forbor.asian.wellMOE","forbor.asian.notwellMOE",
          "forbor.asian.notatallMOE","forbor.other.vwellMOE","forbor.other.wellMOE","forbor.other.notwellMOE","forbor.other.notatallMOE")

langRaw[langRaw == -555555555] <- 0  

lang <- langRaw %>% 
  mutate(engwelltot = (nat.onlyeng + forbor.onlyeng + nat.euro.vwell + nat.span.vwell + nat.span.well + forbor.span.vwell + forbor.span.well + nat.euro.well + nat.asian.vwell + nat.asian.well + nat.other.vwell + nat.other.well + forbor.euro.vwell + forbor.euro.well + forbor.asian.vwell + forbor.asian.well + forbor.other.vwell + forbor.other.well),
         engnotwelltot = (nat.span.notwell + nat.span.notatall + forbor.span.notwell + forbor.span.notatall + nat.euro.notwell + nat.euro.notatall + nat.asian.notwell + nat.asian.notatall + nat.other.notwell + nat.other.notatall + forbor.euro.notwell + forbor.euro.notatall + forbor.other.notwell + forbor.asian.notwell + forbor.asian.notatall),
         
         engwellpct = (nat.onlyeng + forbor.onlyeng + nat.euro.vwell + nat.span.vwell + nat.span.well + forbor.span.vwell + forbor.span.well + nat.euro.well + nat.asian.vwell + nat.asian.well + nat.other.vwell + nat.other.well + forbor.euro.vwell + forbor.euro.well + forbor.asian.vwell + forbor.asian.well + forbor.other.vwell + forbor.other.well) / langtot,
         engnotwellpct = (nat.span.notwell + nat.span.notatall + forbor.span.notwell + forbor.span.notatall + nat.euro.notwell + nat.euro.notatall + nat.asian.notwell + nat.asian.notatall + nat.other.notwell + nat.other.notatall + forbor.euro.notwell + forbor.euro.notatall + forbor.other.notwell + forbor.asian.notwell + forbor.asian.notatall) / langtot,
         
         engwellMOE = moeagg(cbind(nat.onlyengMOE, forbor.onlyengMOE, nat.euro.vwellMOE, nat.span.vwellMOE, nat.span.wellMOE, forbor.span.vwellMOE, forbor.span.wellMOE, nat.euro.wellMOE, nat.asian.vwellMOE, nat.asian.wellMOE, nat.other.vwellMOE, nat.other.wellMOE, forbor.euro.vwellMOE, forbor.euro.wellMOE, forbor.asian.vwellMOE, forbor.asian.wellMOE, forbor.other.vwellMOE, forbor.other.wellMOE)),
         engnotwellMOE = moeagg(cbind(nat.span.notwellMOE, nat.span.notatallMOE, forbor.span.notwellMOE, forbor.span.notatallMOE,nat.euro.notwellMOE, nat.euro.notatallMOE, nat.asian.notwellMOE, nat.asian.notatallMOE, nat.other.notwellMOE, nat.other.notatallMOE, forbor.euro.notwellMOE, forbor.euro.notatallMOE, forbor.other.notwellMOE, forbor.asian.notwellMOE, forbor.asian.notatallMOE)),
         
         engwellMOEprop = moeprop(y= langtot, moex = engwellMOE, moey = langtotMOE, p = engwellpct),
         engnotwellMOEprop = moeprop(y= langtot, moex = engnotwellMOE, moey = langtotMOE, p = engnotwellpct),
         tract = str_pad(PlaceCode,6, pad= "0"),
         GEOID = paste0("22071", tract)) %>%
  select(GEOID, tract,  engnotwellpct, engnotwellMOEprop) #%>%
  #mutate(parish = "Orleans")
write.csv(lang,file="lang2020.csv")
#### TABULAR DATA ####

Census2020IDS <- censusData %>% 
  select(PlaceCode, pop, popMOE, hh, hhMOE, m_under5, m_under5MOE, f_under5, f_under5MOE, racepop, racepopMOE, white, whiteMOE, black, blackMOE, asian, asianMOE, hisp, hispMOE, renter, renterMOE, occupied, occupiedMOE) %>% 
  mutate(Population = pop,
         Population_MOE = popMOE,
         Total_Households = hh,
         Total_Households_MOE = hhMOE,
         Population_Under_5 = m_under5 + f_under5,
         Population_Under_5_MOE = moeagg(cbind(m_under5MOE, f_under5MOE)),
         White_percent = white/racepop,
         White_MOEprop = moeprop( y=racepop, moex =whiteMOE, moey = racepopMOE, p =White_percent),
         Black_percent = black/racepop,
         Black_MOEprop = moeprop( y=racepop, moex =blackMOE, moey = racepopMOE, p =Black_percent),
         Asian_percent = asian/racepop,
         Asian_MOEprop = moeprop( y=racepop, moex =asianMOE, moey = racepopMOE, p =Asian_percent),
         Hispanic_percent = hisp/racepop,
         Hispanic_MOEprop = moeprop( y=racepop, moex =hispMOE, moey = racepopMOE, p =Hispanic_percent),
         Renter_percent = renter/occupied,
         Renter_MOEprop = moeprop( y=occupied, moex =renterMOE, moey = occupiedMOE, p =Renter_percent)
         ) %>%
  select(PlaceCode, Population = pop, Population_MOE, Total_Households, Total_Households_MOE, White_percent, White_MOEprop, Black_percent, Black_MOEprop,Asian_percent,Asian_MOEprop,Hispanic_percent,Hispanic_MOEprop,Renter_percent,Renter_MOEprop)
write.csv(Census2020IDS,file = "tabular_fromIDS_2020.csv")
###### DATA NOT ALREADY IN IDS PULL #####

##Internet variable is in Who Lives but comes from ACS1. I am using same variable names but its different code because pulling from ACS5
##The variable households with children under 18 is not the same as who lives because in who lives it is households with OWN children under 18. 
##### Maybe in future change who lives to "hwoc" (o for own) so this one can be in IDS as "hwc"
fresh.vars <-c("B19013_001E", "B19013_001M", "B11005_002E", "B11005_002E", "B28002_001E", "B28002_001M", "B28002_012E", "B28002_012M", "B28002_013E", "B28002_013M" )
fresh.names <-c("medhhinc", "medhhincMOE","hhunder18","hhunder18MOE", "totinta", "totaintMOE", "NoSubscript","NoSubscriptMOE","NoAccess","NoAccessMOE")
freshRaw <- np.pull(variables=fresh.vars, names=fresh.names) 

fresh <- freshRaw %>% 
  mutate(Median_household_income = medhhinc,
         Median_household_incomeMOE = medhhincMOE,
         Households_with_people_under18 = hhunder18,
         Households_with_people_under18MOE = hhunder18MOE,
         No_home_internet_access_percent = (NoSubscript + NoAccess)/totinta,
         No_home_internet_access_MOEagg = moeagg(cbind(NoSubscriptMOE, NoAccessMOE)),
         No_home_internet_access_MOEprop = moeprop( y=totinta, moex =No_home_internet_access_MOEagg, moey = totaintMOE, p =No_home_internet_access_percent)) %>%
  select(parish, tract, Median_household_income, Median_household_incomeMOE, Households_with_people_under18, Households_with_people_under18MOE, No_home_internet_access_percent, No_home_internet_access_MOEagg, No_home_internet_access_MOEprop)

write.csv(fresh,file = "tabular_fromCB_2020.csv")


###I Kept the work I did for 2017 because the variables are different in 2015 in case we figure that out
#### This data set not available past 2015
### Pulled 2015 and found way to identify max in each row
## Next step would be to find a way to do ifelse statement that pulls the column header that identifies the top language spoken based on the max number I pulled
##The spreadsheet I used to identify and organize these variables is "langspoenvars" and it in this repository

langspoken17.vars <-c("B16001_001E","B16001_005E","B16001_008E","B16001_011E","B16001_014E","B16001_017E","B16001_020E","B16001_023E","B16001_026E","B16001_029E",
                    "B16001_032E","B16001_035E","B16001_038E","B16001_041E","B16001_044E","B16001_047E","B16001_050E","B16001_053E","B16001_056E","B16001_059E",
                    "B16001_062E","B16001_065E","B16001_068E","B16001_071E","B16001_074E","B16001_077E","B16001_080E","B16001_083E","B16001_086E","B16001_089E",
                    "B16001_092E","B16001_095E","B16001_098E","B16001_101E","B16001_104E","B16001_107E","B16001_110E","B16001_113E","B16001_116E","B16001_119E",
                    "B16001_122E","B16001_125E","B16001_128E","B16001_001M","B16001_005M","B16001_008M","B16001_011M","B16001_014M","B16001_017M","B16001_020M",
                    "B16001_023M","B16001_026M","B16001_029M","B16001_032M","B16001_035M","B16001_038M","B16001_041M","B16001_044M","B16001_047M","B16001_050M",
                    "B16001_053M","B16001_056M","B16001_059M","B16001_062M","B16001_065M","B16001_068M","B16001_071M","B16001_074M","B16001_077M","B16001_080M",
                    "B16001_083M","B16001_086M","B16001_089M","B16001_092M","B16001_095M","B16001_098M","B16001_101M","B16001_104M","B16001_107M","B16001_110M",
                    "B16001_113M","B16001_116M","B16001_119M","B16001_122M","B16001_125M","B16001_128M")
langspoken17.names <-c("Tot","Spanish","FrenchCajun","Haitian","Italian","Portuguese","German","Yiddish","Greek","Russian","Polish","SerboCroatian","Ukrainian",
                     "Armenian","Persian","Gujarati","Hindi","Urdu","Punjabi","Bengali","Nepali","IndoEuro","Telugu","Tamil","Malayalam","Chinese","Japanese",
                     "Korean","Hmong","Vietnamese","khmer","Thai","OtherAsia","Tagalog","Hawaiian","Arabic","Hebrew","Afroasia","WestAfrica","Swahili","Navajo",
                     "Native","Other", "TotMOE","SpanishMOE","FrenchCajunMOE","HaitianMOE","ItalianMOE","PortugueseMOE","GermanMOE","YiddishMOE","GreekMOE","RussianMOE",
                     "PolishMOE","SerboCroatianMOE","UkrainianMOE","ArmenianMOE","PersianMOE","GujaratiMOE","HindiMOE","UrduMOE","PunjabiMOE","BengaliMOE","NepaliMOE",
                     "IndoEuroMOE","TeluguMOE","TamilMOE","MalayalamMOE","ChineseMOE","JapaneseMOE","KoreanMOE","HmongMOE","VietnameseMOE","khmerMOE","ThaiMOE",
                     "OtherAsiaMOE","TagalogMOE","HawaiianMOE","ArabicMOE","HebrewMOE","AfroasiaMOE","WestAfricaMOE","SwahiliMOE","NavajoMOE","NativeMOE","OtherMOE")
langspoken17Raw <- np.pull(variables=langspoken17.vars, names=langspoken17.names) 

langspoken15.vars <-c("B16001_001E","B16001_005E","B16001_008E","B16001_011E","B16001_014E","B16001_017E","B16001_020E","B16001_023E","B16001_026E","B16001_029E",
                      "B16001_032E","B16001_035E","B16001_038E","B16001_041E","B16001_044E","B16001_047E","B16001_050E","B16001_053E","B16001_056E","B16001_059E",
                      "B16001_062E","B16001_065E","B16001_068E","B16001_071E","B16001_074E","B16001_077E","B16001_080E","B16001_083E","B16001_086E","B16001_089E",
                      "B16001_092E","B16001_095E","B16001_098E","B16001_101E","B16001_104E","B16001_107E","B16001_110E","B16001_113E","B16001_116E","B16001_119E",
                      "B16001_001M","B16001_005M","B16001_008M","B16001_011M","B16001_014M","B16001_017M","B16001_020M","B16001_023M","B16001_026M","B16001_029M",
                      "B16001_032M","B16001_035M","B16001_038M","B16001_041M","B16001_044M","B16001_047M","B16001_050M","B16001_053M","B16001_056M","B16001_059M",
                      "B16001_062M","B16001_065M","B16001_068M","B16001_071M","B16001_074M","B16001_077M","B16001_080M","B16001_083M","B16001_086M","B16001_089M",
                      "B16001_092M","B16001_095M","B16001_098M","B16001_101M","B16001_104M","B16001_107M","B16001_110M","B16001_113M","B16001_116M","B16001_119M")
langspoken15.names <-c("tot","spanish","french","frenchcreole","italian","porteguese","german","yiddish","otherwestgerm","scan","greek","russian","polish","serb",
                       "otherslav","armenian","persian","gujarati","hindi","urdu","otherindic","otherindoeur","chinese","japanese","korean","monkhmer","hmong",
                       "thai","lao","vietnamese","otherasian","tagalo","pacific","navajo","othernorthern","hungarian","arabic","hebrew","african","other", "totMOE",
                       "spanishMOE","frenchMOE","frenchcreoleMOE","italianMOE","portegueseMOE","germanMOE","yiddishMOE","otherwestgermMOE","scanMOE","greekMOE",
                       "russianMOE","polishMOE","serbMOE","otherslavMOE","armenianMOE","persianMOE","gujaratiMOE","hindiMOE","urduMOE","otherindicMOE","otherindoeurMOE",
                       "chineseMOE","japaneseMOE","koreanMOE","monkhmerMOE","hmongMOE","thaiMOE","laoMOE","vietnameseMOE","otherasianMOE","tagaloMOE","pacificMOE",
                       "navajoMOE","othernorthernMOE","hungarianMOE","arabicMOE","hebrewMOE","africanMOE","otherMOE")
langspoken15Raw <- np.pull15(variables=langspoken15.vars, names=langspoken15.names) 


langspoken15 <- langspoken15Raw %>% 
  mutate(langMAXnum = apply(.[3:41], 1, max))


langspoken15sums <- langspoken15Raw %>% 
  select(-place) %>% 
  summarize_all(funs(sum))








