library(tidyverse)
library(data.table)
theme_set(theme_bw())

# Read raw data ----
# fs = list.files(recursive = T) %>% 
#   str_subset(pattern = "Population") 
# fs
# 
# plants = data.frame()
# for (f in fs){
#   s = strsplit(strsplit(f, "/")[[1]][1], "_")[[1]]
#   s
#   d = fread(f) %>% 
#     mutate(seed = s[5],
#            curved = s[3])
#   plants = bind_rows(plants, d)
# }
# 
# save(plants, file="plants.rda")
load(file="plants.rda")

# Visualization ----
plants %>% 
  filter(time == max(time)) %>% 
  gather(., key, value, r_crown, h_stem, r_stem, r_root) %>% 
  ggplot(., aes(x = plant, y = value, col = curved)) +
  geom_boxplot() +
  facet_wrap(~key, scales = "free") +
  labs(x = "Plant",
       col = "Curved crown")

ggsave("geomtries_100y.jpg", width = 8, height = 4)  


plants %>% 
  group_by(time, plant, curved) %>% 
  mutate(m = mean(ag_factor)) %>% 
  ggplot(., aes(x = time/3600/24/365.25, y = ag_factor, col = curved,
                group = interaction(curved, plant))) +
  geom_line(linewidth = 0.2, alpha = 0.1) +
  geom_line(aes(y = m), size = 0.8) +
  facet_wrap(~plant) +
  labs(x = "Time",
       y = "ag factor",
       col = "Curved crown")

ggsave("ag_over_time.jpg", width = 8, height = 4)  
