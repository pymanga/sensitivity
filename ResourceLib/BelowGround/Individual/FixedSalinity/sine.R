library(tidyverse)

get.salinity = function(amplitude, stretch_h, stretch_v, deviation){
  years = 1
  t_end = years * 3600 * 24 * 365.25
  delta_t = 1e6
  steps_per_year = 3600 * 24 * 365.25 / delta_t
  
  return(data.frame(i = years,
                    time = seq(0, t_end, by = delta_t)) %>% 
           mutate(sal = rnorm(1, amplitude, 2) * 
                    sin(time/3600/24 / rnorm(1, stretch_h, 2)) + 
                    rnorm(1, stretch_v, 2)) %>%
           rowwise() %>%
           mutate(salinity = rnorm(1, sal, deviation)) %>%
           mutate(salinity = ifelse(salinity < 0, 0, salinity)))
}



salinities = data.frame()

for (stretch_v in c(25, 50, 70)){
  for (stretch_h in c(29, 58)){
    for (amplitude in c(5, 15)){
      for (deviation in c(0, 2, 5)){
        for (seed in seq(1, 100, by = 20)){
          s = get.salinity(amplitude, stretch_h, stretch_v, deviation) %>% 
            mutate(amplitude = amplitude,
                   stretch_v = stretch_v,
                   stretch_h = stretch_h,
                   deviation = deviation,
                   seed = seed)
          salinities = rbind(salinities, s)
          
        }
      }
    }
  }
}


salinities %>% 
  group_by(time, amplitude, deviation, stretch_h, stretch_v) %>% 
  mutate(m = mean(salinity),
          sd = sd(salinity)) %>%  
  ggplot(., aes(x = time/3600/24/365.25, y = salinity,
                group = interaction(deviation, amplitude, seed),
                linetype = factor(deviation), 
                col = factor(amplitude))) +
  geom_line(alpha = 0.4) +
  geom_line(size = 1, aes(y = m)) +
  scale_color_viridis_d(end=0.8) +
  facet_grid(stretch_h~stretch_v,
             labeller = label_both) +
  labs(col = "Amplitude",
       linetype = "Deviation", 
       x = "Time (year)",
       y = "Salinity (ppt)")

ggsave("sine.jpg")
