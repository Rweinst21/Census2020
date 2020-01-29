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
race.vars <-c("B03002_001E","B03002_003E","B03002_004E","B03002_005E","B03002_006E","B03002_007E","B03002_008E","B03002_009E","B03002_012E","B03002_001M","B03002_003M","B03002_004M","B03002_005M","B03002_006M","B03002_007M","B03002_008M","B03002_009M","B03002_012M")
race.names <-c("pop2017", "white2017","black2017","native2017","asian2017","island2017","other2017","two2017","hisp2017","popMOE", "whiteMOE","blackMOE","nativeMOE","asianMOE","islandMOE","otherMOE","twoMOE","hispMOE")
raceRaw <- np.pull.parish(variables=race.vars, names=race.names) 
#save(raceRaw, file = "inputs/raceRaw.RData")


race2010.vars <- c("P005001", "P005003","P005004","P005005","P005006","P005007","P005008","P005009","P004003")
race2010.names <-c("pop2010", "white2010", "black2010", "native2010", "asian2010", "island2010", "other2010", "two2010", "hisp2010")
race2010Raw <- np.pull.parish(variables=race2010.vars, names=race2010.names, year = 2010, survey = "dec/sf1") 

## 2000
race2000.vars <-c("P008001","P008003","P008004","P008005","P008006","P008007","P008008","P008009","P008010")
race2000.names <-c("pop2000","white2000","black2000","native2000","asian2000","island2000","other2000","two2000","hisp2000")
race2000Raw <- np.pull.parish(variables=race2000.vars, names=race2000.names, year = 2000, survey = "sf1") 
#save(race2000Raw, file = "inputs/race2000Raw.RData")







### 2017

race2017 <- raceRaw %>% 
  mutate(county = as.numeric(county)) %>% 
  merge(., county_crosswalk, by = "county") %>% 
  mutate(   
    blackpct2017 = black2017 / pop2017,
    whitepct2017 = white2017 / pop2017,
    asianpct2017 = asian2017 / pop2017,
    nativepct2017 = native2017 / pop2017,
    twopct2017 = two2017 / pop2017,
    hisppct2017 = hisp2017 / pop2017,
    other2017 = island2017 + other2017,
    otherpct2017 = other2017 /pop2017,
    
    othermoeagg2017 = moeagg(cbind(islandMOE, otherMOE)),
    popMOE = ifelse(popMOE < 0, 0, popMOE),
    hispMOE = ifelse(hispMOE < 0, 0, hispMOE),
    
    blackMOEprop2017 = moeprop(y= pop2017, moex = blackMOE, moey = popMOE, p = blackpct2017),
    whiteMOEprop2017 = moeprop(y= pop2017, moex = whiteMOE, moey = popMOE, p = whitepct2017),
    asianMOEprop2017 = moeprop(y= pop2017, moex = asianMOE, moey = popMOE, p = asianpct2017),
    nativeMOEprop2017 = moeprop(y= pop2017, moex = nativeMOE, moey = popMOE, p = nativepct2017),
    otherMOEprop2017 = moeprop(y= pop2017, moex = othermoeagg2017, moey = popMOE, p = otherpct2017),
    twoMOEprop2017 = moeprop(y= pop2017, moex = twoMOE, moey = popMOE, p = twopct2017),
    hispMOEprop2017 = moeprop(y= pop2017, moex = hispMOE, moey = popMOE, p = hisppct2017)
  
    )




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

race <-merge(race2000, race2010, by = "county") %>% 
  merge(., race2017, by = "county") %>% 
  mutate(blackSIG = stattest(x=blackpct2010, y=blackpct2017, moey = blackMOEprop2017),
         whiteSIG = stattest(x=whitepct2010, y=whitepct2017, moey = whiteMOEprop2017),
         asianSIG =stattest(x=asianpct2010, y=asianpct2017, moey = asianMOEprop2017),
         nativeSIG = stattest(x=nativepct2010, y=nativepct2017, moey=nativeMOEprop2017),
         otherSIG = stattest(x=otherpct2010, y=otherpct2017, moey = otherMOEprop2017),
         twoSIG = stattest( x=twopct2010, y=twopct2017, moey = twoMOEprop2017),
         hispSIG = stattest(x=hisppct2010, y = hisppct2017, moey = hispMOEprop2017))


racetablepct <- race %>% 
  select(name, 
         blackpct2000, blackpct2010, blackpct2017, blackMOEprop2017, blackSIG,
         whitepct2000, whitepct2010, whitepct2017, whiteMOEprop2017, whiteSIG,
         asianpct2000, asianpct2010, asianpct2017, asianMOEprop2017, asianSIG,
         nativepct2000, nativepct2010, nativepct2017, nativeMOEprop2017, nativeSIG,
         otherpct2000, otherpct2010, otherpct2017, otherMOEprop2017, otherSIG,
         twopct2000, twopct2010, twopct2017, twoMOEprop2017, twoSIG,
         hisppct2000, hisppct2010, hisppct2017, hispMOEprop2017, hispSIG)



blacktablepct <- racetablepct %>% 
  mutate(Race = "Black or African American") %>% 
  select(name, Race, blackpct2000, blackpct2010, blackpct2017, blackMOEprop2017, blackSIG)%>% 
  rename(y2000 = "blackpct2000",
         y2010 = "blackpct2010",
         y2017 = "blackpct2017",
         MOE = "blackMOEprop2017",
         Signigicant = "blackSIG") %>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2017 = percent(y2017),
         MOE = percent(MOE))

whitetablepct <- racetablepct %>% 
  mutate(Race = "White") %>% 
  select(name, Race, whitepct2000, whitepct2010, whitepct2017, whiteMOEprop2017, whiteSIG) %>% 
  rename(y2000 = "whitepct2000",
         y2010 = "whitepct2010",
         y2017 = "whitepct2017",
         MOE = "whiteMOEprop2017",
         Signigicant = "whiteSIG")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2017 = percent(y2017),
         MOE = percent(MOE))

asiantablepct <- racetablepct %>% 
  mutate(Race = "Asian") %>% 
  select(name, Race, asianpct2000, asianpct2010, asianpct2017, asianMOEprop2017, asianSIG)%>% 
  rename(y2000 = "asianpct2000",
         y2010 = "asianpct2010",
         y2017 = "asianpct2017",
         MOE = "asianMOEprop2017",
         Signigicant = "asianSIG")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2017 = percent(y2017),
         MOE = percent(MOE))

nativetablepct <- racetablepct %>% 
  mutate(Race = "American Indian") %>% 
  select(name, Race, nativepct2000, nativepct2010, nativepct2017, nativeMOEprop2017, nativeSIG)%>% 
  rename(y2000 = "nativepct2000",
         y2010 = "nativepct2010",
         y2017 = "nativepct2017",
         MOE = "nativeMOEprop2017",
         Signigicant = "nativeSIG")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2017 = percent(y2017),
         MOE = percent(MOE))

othertablepct <- racetablepct %>% 
  mutate(Race = "2 race categories") %>% 
  select(name, Race, otherpct2000, otherpct2010, otherpct2017, otherMOEprop2017, otherSIG) %>% 
  rename(y2000 = "otherpct2000",
         y2010 = "otherpct2010",
         y2017 = "otherpct2017",
         MOE = "otherMOEprop2017",
         Signigicant = "otherSIG")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2017 = percent(y2017),
         MOE = percent(MOE))

twotablepct <- racetablepct %>% 
  mutate(Race = "Hispanic (any race)") %>% 
  select(name, Race, twopct2000, twopct2010, twopct2017, twoMOEprop2017, twoSIG)%>% 
  rename(y2000 = "twopct2000",
         y2010 = "twopct2010",
         y2017 = "twopct2017",
         MOE = "twoMOEprop2017",
         Signigicant = "twoSIG")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2017 = percent(y2017),
         MOE = percent(MOE))

hisptablepct <- racetablepct %>% 
  mutate(Race = "Other") %>% 
  select(name, Race, hisppct2000, hisppct2010, hisppct2017, hispMOEprop2017, hispSIG)%>% 
  rename(y2000 = "hisppct2000",
         y2010 = "hisppct2010",
         y2017 = "hisppct2017",
         MOE = "hispMOEprop2017",
         Signigicant = "hispSIG")%>% 
  mutate(y2000 = percent(y2000),
         y2010 = percent(y2010),
         y2017 = percent(y2017),
         MOE = percent(MOE))


poptable <- bind_rows(blacktablepct , whitetablepct , asiantablepct , nativetablepct , twotablepct , hisptablepct , othertablepct ) 














#### FOr count




blacktable <- race %>% 
  mutate(Race = "Black or African American") %>% 
  select(name, Race, black2000, black2010, black2017, blackMOE)%>% 
  rename(y2000 = "black2000",
         y2010 = "black2010",
         y2017 = "black2017",
         MOE = "blackMOE") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2017 = comma(y2017),
         MOE = comma(MOE))

whitetable <- race %>% 
  mutate(Race = "White") %>% 
  select(name, Race, white2000, white2010, white2017, whiteMOE) %>% 
  rename(y2000 = "white2000",
         y2010 = "white2010",
         y2017 = "white2017",
         MOE = "whiteMOE") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2017 = comma(y2017),
         MOE = comma(MOE))

asiantable <- race %>% 
  mutate(Race = "Asian") %>% 
  select(name, Race, asian2000, asian2010, asian2017, asianMOE)%>% 
  rename(y2000 = "asian2000",
         y2010 = "asian2010",
         y2017 = "asian2017",
         MOE = "asianMOE") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2017 = comma(y2017),
         MOE = comma(MOE))

nativetable <- race %>% 
  mutate(Race = "American Indian") %>% 
  select(name, Race, native2000, native2010, native2017, nativeMOE)%>% 
  rename(y2000 = "native2000",
         y2010 = "native2010",
         y2017 = "native2017",
         MOE = "nativeMOE") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2017 = comma(y2017),
         MOE = comma(MOE))

othertable <- race %>% 
  mutate(Race = "2 race categories") %>% 
  select(name, Race, other2000, other2010, other2017, otherMOE) %>% 
  rename(y2000 = "other2000",
         y2010 = "other2010",
         y2017 = "other2017",
         MOE = "otherMOE") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2017 = comma(y2017),
         MOE = comma(MOE))

twotable <- race %>% 
  mutate(Race = "Hispanic (any race)") %>% 
  select(name, Race, two2000, two2010, two2017, twoMOE)%>% 
  rename(y2000 = "two2000",
         y2010 = "two2010",
         y2017 = "two2017",
         MOE = "twoMOE") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2017 = comma(y2017),
         MOE = comma(MOE))

hisptable <- race %>% 
  mutate(Race = "Other") %>% 
  select(name, Race, hisp2000, hisp2010, hisp2017, hispMOE)%>% 
  rename(y2000 = "hisp2000",
         y2010 = "hisp2010",
         y2017 = "hisp2017",
         MOE = "hispMOE") %>% 
  mutate(y2000 = comma(y2000),
         y2010 = comma(y2010),
         y2017 = comma(y2017),
         MOE = comma(MOE))


poptableCount <- bind_rows(blacktable , whitetable , asiantable , nativetable , twotable , hisptable , othertable ) 
