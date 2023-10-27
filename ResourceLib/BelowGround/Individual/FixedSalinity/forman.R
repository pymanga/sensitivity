library(tidyverse)

# a = self._salt_effect_d
# b = self._salt_effect_ui

get.bg_factor = function(a, b, salinity_plant){
  return((1 / (1 + exp(a *
      (b - (salinity_plant))))))
}


tt = data.frame()

for (a in seq(-1, 0, length.out = 50)){
  for (b in seq(0, 100, length.out = 50)){
    t = data.frame(salinity_plant = seq(0, 90, by = 10)) %>% 
      mutate(a = a,
             b = b,
             res_b = get.bg_factor(a, b, salinity_plant))
    tt = rbind(tt, t)
    
  }
}


tt %>% 
  rename("Salinity" = "salinity_plant") %>% 
  ggplot(., aes(x = a, fill = res_b, 
                y = b)) +
  geom_tile() +
  geom_point(aes(x = -0.18, y = 72, col = "Avicennia germinans"), 
             size = 4, shape = 8) +
  geom_point(aes(x = -0.25, y = 58, col = "Rhizophora mangle"), 
             size = 4, shape = 8) +
  scale_fill_viridis_c() +
  facet_wrap(~Salinity, ncol = 5, labeller = label_both) +
  labs(x = "Constant for salt effect on growth",
       y = "Salt effect on growth",
       fill = "Growth reduction factor",
       col = "Chen and Twilley (1998)") +
  theme_classic()

ggsave("forman.jpg")
