library(censusapi)
library(tidyverse)



### Low Response Score available in Census Planning Database
### only years 15-16 available in API
### download tract-level spreadsheet in zip file below
### https://www2.census.gov/adrm/PDB/2019/pdb2019trv3_us.zip

#C:/Users/jenna/Documents/pdb2019censusLRS.csv

LRS <- read_csv('YOUR FILE PATH HERE')%>%
  select(GIDTR, State, State_name, County, County_name, LAND_AREA, Tot_Population_ACS_13_17,Tot_Population_ACSMOE_13_17, Low_Response_Score) %>%
  filter(State == "22")

### Count of children 4 and under
np.pull <- function(variables, names = variables, year=2017, survey = "acs/acs5"){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  tract <- getCensus(name = survey, 
                     vintage = year, 
                     key = censuskey, 
                     vars = variables, 
                     region = "tract:*", 
                     regionin = "state:22") %>% select(-state)
  colnames(tract) <- c("parish", "tract",names) 
  return(tract)
}

age.vars <-c("B01003_001E", "B01003_001M",
             "B01001_003E", 
             "B01001_027E", 
             "B01001_003M", 
             "B01001_027M")
age.names <-c("pop", "popMOE", "m_under5","f_under5","m_under5MOE","f_under5MOE")
ageRaw <- np.pull(variables=age.vars, names=age.names) 

ageRaw[ageRaw == -555555555] <- 0  
age <- ageRaw %>%
  mutate(t4less = (m_under5 + f_under5), 
         t4lessmoeagg = sqrt(m_under5MOE^2 + f_under5MOE^2),
         t4lessCV = (t4lessmoeagg/1.64)/t4less)


# The most straighforward measure of English ability is probably the English ability question from the ACS.
# 
# A paper published at the Census Bureau reports "that the English-ability question, despite being a self-assessment, does a good job of measuring English ability."
# https://www.census.gov/newsroom/blogs/research-matters/2015/10/how-well-do-you-speak-english-assessing-the-validity-of-the-american-community-survey-english-ability-question.html

# In neighborhood profiles, we group together people who speak english "well" or "very well" and people who speak English "not well" or "not at all." The Census Bureau/LEP.gov considers anyone who speaks English less than "very well" to have limited English proficiency. 
#
# https://www.lep.gov/faqs/faqs.html#OneQ1

# I recommend that becaue "Limited English Proficiency" is a known term, we use that indicator rather than a more stringent "Low English Proficiency." The data may tell us to go with the lower threshold, though!


lang.vars <- c("B16005_001E","B16005_003E","B16005_005E","B16005_006E","B16005_007E","B16005_008E","B16005_010E","B16005_011E","B16005_012E","B16005_013E","B16005_015E","B16005_016E","B16005_017E","B16005_018E","B16005_020E","B16005_021E","B16005_022E","B16005_023E","B16005_025E","B16005_027E",
               "B16005_028E","B16005_029E","B16005_030E","B16005_032E","B16005_033E","B16005_034E","B16005_035E","B16005_037E","B16005_038E","B16005_039E","B16005_040E","B16005_042E","B16005_043E","B16005_044E","B16005_045E","B16005_001M","B16005_003M","B16005_005M","B16005_006M","B16005_007M",
               "B16005_008M","B16005_010M","B16005_011M","B16005_012M","B16005_013M","B16005_015M","B16005_016M","B16005_017M","B16005_018M","B16005_020M","B16005_021M","B16005_022M","B16005_023M","B16005_025M","B16005_027M","B16005_028M","B16005_029M","B16005_030M","B16005_032M","B16005_033M",
               "B16005_034M","B16005_035M","B16005_037M","B16005_038M","B16005_039M","B16005_040M","B16005_042M","B16005_043M","B16005_044M","B16005_045M")
lang.names <- c("tot","nat.onlyeng","nat.span.vwell","nat.span.well","nat.span.notwell","nat.span.notatall","nat.euro.vwell","nat.euro.well","nat.euro.notwell","nat.euro.notatall","nat.asian.vwell","nat.asian.well","nat.asian.notwell","nat.asian.notatall","nat.other.vwell","nat.other.well",
                "nat.other.notwell","nat.other.notatall","forbor.onlyeng","forbor.span.vwell","forbor.span.well","forbor.span.notwell","forbor.span.notatall","forbor.euro.vwell","forbor.euro.well","forbor.euro.notwell","forbor.euro.notatall","forbor.asian.vwell","forbor.asian.well",
                "forbor.asian.notwell","forbor.asian.notatall","forbor.other.vwell","forbor.other.well","forbor.other.notwell","forbor.other.notatall","totMOE","nat.onlyengMOE","nat.span.vwellMOE","nat.span.wellMOE","nat.span.notwellMOE","nat.span.notatallMOE","nat.euro.vwellMOE",
                "nat.euro.wellMOE","nat.euro.notwellMOE","nat.euro.notatallMOE","nat.asian.vwellMOE","nat.asian.wellMOE","nat.asian.notwellMOE","nat.asian.notatallMOE","nat.other.vwellMOE","nat.other.wellMOE","nat.other.notwellMOE","nat.other.notatallMOE","forbor.onlyengMOE",
                "forbor.span.vwellMOE","forbor.span.wellMOE","forbor.span.notwellMOE","forbor.span.notatallMOE","forbor.euro.vwellMOE","forbor.euro.wellMOE","forbor.euro.notwellMOE","forbor.euro.notatallMOE","forbor.asian.vwellMOE","forbor.asian.wellMOE","forbor.asian.notwellMOE",
                "forbor.asian.notatallMOE","forbor.other.vwellMOE","forbor.other.wellMOE","forbor.other.notwellMOE","forbor.other.notatallMOE")
langRaw <- np.pull(variables = lang.vars, names = lang.names)

langRaw[langRaw == -555555555] <- 0 
langUSRaw[langUSRaw == -555555555] <- 0 

lang <- langRaw %>% 
  mutate(engvwellpct = (nat.onlyeng + 
                       forbor.onlyeng + 
                       nat.euro.vwell + 
                       nat.span.vwell + 
                       #nat.span.well + 
                       #forbor.span.well +   
                       forbor.span.vwell + 
                       #nat.euro.well + 
                       nat.asian.vwell + 
                       #nat.asian.well + 
                       nat.other.vwell + 
                       #nat.other.well + 
                       forbor.euro.vwell + 
                       #forbor.euro.well + 
                       forbor.asian.vwell + 
                       #forbor.asian.well + 
                       #forbor.other.well + 
                       forbor.other.vwell) / tot,
         engwellpct = (nat.span.well + 
                         forbor.span.well +                         
                         nat.euro.well + 
                         nat.asian.well + 
                         nat.other.well +
                         forbor.euro.well +
                         forbor.asian.well + 
                         forbor.other.well) / tot,)
         engnotwellpct = (nat.span.notwell + 
                          nat.span.notatall + 
                          forbor.span.notwell + 
                          forbor.span.notatall +
                          nat.euro.notwell + 
                          nat.euro.notatall + 
                          nat.asian.notwell + 
                          nat.asian.notatall + 
                          nat.other.notwell + 
                          nat.other.notatall + 
                          forbor.euro.notwell + 
                          forbor.euro.notatall + 
                          forbor.other.notwell + 
                          forbor.asian.notwell + 
                          forbor.asian.notatall) / tot,
         
         engwellMOE = moeagg(cbind(nat.onlyengMOE, forbor.onlyengMOE, nat.euro.vwellMOE, nat.span.vwellMOE, nat.span.wellMOE, forbor.span.vwellMOE, forbor.span.wellMOE, nat.euro.wellMOE, nat.asian.vwellMOE, nat.asian.wellMOE, nat.other.vwellMOE, nat.other.wellMOE, forbor.euro.vwellMOE, forbor.euro.wellMOE, forbor.asian.vwellMOE, forbor.asian.wellMOE, forbor.other.vwellMOE, forbor.other.wellMOE)),
         spanengnotwellMOE = moeagg(cbind(nat.span.notwellMOE, nat.span.notatallMOE, forbor.span.notwellMOE, forbor.span.notatallMOE)),
         engnotwellMOE = moeagg(cbind(nat.euro.notwellMOE, nat.euro.notatallMOE, nat.asian.notwellMOE, nat.asian.notatallMOE, nat.other.notwellMOE, nat.other.notatallMOE, forbor.euro.notwellMOE, forbor.euro.notatallMOE, forbor.other.notwellMOE, forbor.asian.notwellMOE, forbor.asian.notatallMOE)),
         
         engwellMOEprop = moeprop(y= tot, moex = engwellMOE, moey = totMOE, p = engwellpct),
         spanengnotwellMOEprop = moeprop(y= tot, moex = spanengnotwellMOE, moey = totMOE, p = spanengnotwellpct),
         engnotwellMOEprop = moeprop(y= tot, moex = engnotwellMOE, moey = totMOE, p = engnotwellpct)
  )

langEST2 <- langAnalysis %>% 
  select(geo,contains("pct")) %>% 
  mutate_at(vars(-1), funs(as.numeric)) %>% 
  mutate_at(vars(-1), funs(scales::percent(.,accuracy = .1))) %>% 
  gather(meas, val, -1)
langMOE2 <- langAnalysis %>% 
  select(geo, contains("MOEprop")) %>% 
  mutate_at(vars(-1),funs(as.numeric)) %>% 
  mutate_at(vars(-1), funs(scales::percent(.,accuracy = .1))) %>% 
  gather(measMOE, valMOE, -1)
llang <- bind_cols(langEST2, langMOE2) %>% select(-geo1)

