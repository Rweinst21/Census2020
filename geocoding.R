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
