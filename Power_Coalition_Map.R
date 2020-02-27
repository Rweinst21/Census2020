
library(sf)
library(tidycensus)

acs.parishes_sf <- get_acs(geography = "county",
                           key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                           variables = c(medincome = "B19013_001"), # This is median HH income
                           shift_geo = FALSE, # This downloads the geog in Albers
                           geometry = TRUE)%>%
  filter(str_sub(GEOID,1,2)=="22") 
         
parishes_map <- acs.parishes_sf %>% 
  mutate(County = as.numeric(str_sub(GEOID,3,5)))
  

acs.parishes_sf %>%
  left_join(countyLE.map, by=c("GEOID"="fips5"))  %>%
  ggplot() +
  geom_sf(aes(fill=LE.disc)) +
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  #scale_fill_manual(values = lacroix_palette("Berry", n = 11, type = "continuous"),  na.value = "gray70")+  #
  #scale_fill_paletteer_c("pals::ocean.curl", na.value = "gray70") + # "Redmonder::dPBlPuGn" "LaCroixColoR::Lemon" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  scale_fill_brewer(palette = "PRGn", direction = -1,  na.value = "gray70") +
  #scale_fill_viridis_d(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  #scale_fill_distiller(direction = 1, palette = 9, guide = "legend") +
  coord_sf(datum = NA) +
  themeDC_map() +
  theme(legend.position = "right") 