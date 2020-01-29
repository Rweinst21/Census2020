library(ggmap)
register_google(key = "AIzaSyBQq7E4_FdlnDX_YpjU9lws_vbNmVky1oI")
libraries <- read.csv("inputs/Public_Libraries_data_LA_updated.csv")

librariesGC2 <- libraries %>%
  mutate(fullAdd = paste(as.character(Address), as.character(City), "LA", sep = ", ")) %>%
  mutate_geocode(fullAdd)


daycares <- read.csv("inputs/day care centers LA.csv")   

daycaresGC <- daycares %>%
  mutate(fullAdd = as.character(Full.Address))%>%
  mutate_geocode(fullAdd)

write.csv(librariesGC2, file = "inputs/Public_Libraries_data_LA_updated_geocoded.csv")
write.csv(daycaresGC, file = "inputs/day care centers LA geocoded.csv" )             

tract_ZIP_hud <- read.csv("tract_ZIPcrosswalk.csv") %>%
  mutate(ZIP = as.character(ZIP))



# labelled_ZIP <- read.csv("zips_label_pobox.csv") %>%
#   filter(Zip.Code.Type != "PO Box") %>%
#   mutate(ZIP = ï..Zip.Code)


censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
 
zips <- getCensus(name = "acs/acs5/subject", 
                     vintage = 2017, 
                     key = censuskey, 
                     vars = c("S0101_C01_001E"), 
                     #region = , 
                     region = "zip code tabulation area") %>% 
  rename(pop = S0101_C01_001E) %>% 
  filter(pop >0)

tract_ZIP_nonpo <- tract_ZIP_hud %>%
  filter(ZIP %in% zips$zip_code_tabulation_area) %>%
  rename(GEOID =  "ï..TRACT") %>%
  filter(str_sub(GEOID,1,2)== "22")

write.csv(tract_ZIP_nonpo, file = "tract_ZIP_nonPOBox.csv")

