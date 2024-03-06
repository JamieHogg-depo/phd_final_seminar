## Create aus examples for final seminar

library(tidyverse)
library(sf)

#load data
sa2 <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  filter(STATE_NAME != "Other Territories")%>% 
  st_transform(crs = st_crs(3112))

# create state boundaries
state_border <- sa2 %>% 
  group_by(STATE_NAME) %>% 
  summarise(geometry = st_union(geometry))%>% 
  st_as_sf() %>% 
  st_transform(crs = st_crs(3112))

# One estimate
sa2 %>% 
  mutate(y = 0) %>% 
  ggplot() +
  geom_sf(aes(fill = y), col = NA) +
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1) + 
  theme_void()+
  theme(legend.position = "none")+
  scale_fill_viridis_c(begin = 0, end = 1, 
                     option = "F")
ggsave(file = "C:/python_proj/phd_final_seminar/imgs/allAus.png",
       width = 7.3, height = 6.76)

# unique estimate for SA2s
sa2 %>% 
  mutate(y = rnorm(nrow(.))) %>% 
  ggplot() +
  geom_sf(aes(fill = y), col = NA) +
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1) + 
  theme_void()+
  theme(legend.position = "none")+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       option = "F")
ggsave(file = "C:/python_proj/phd_final_seminar/imgs/allSA2s.png",
       width = 7.3, height = 6.76)