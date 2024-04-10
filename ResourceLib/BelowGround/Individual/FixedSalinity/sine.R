library(tidyverse)
library(ggpubr)
theme_set(theme_bw())

# Function to calculate salinity at the border
# time_sec: time (seconds)
# s_x: salinity border (kg/kg)
# amplitude: amplitude of sine (kg/kg)
# stretch: horizontal stretch of sine 
# offset: offset of sine
# deviation: deviation in salinity (kg/kg)

sal.x.t = function(time_sec, s_x, amplitude, stretch, offset, deviation){
    xi <- amplitude * sin(time_sec /stretch + offset) + s_x
    xi = rnorm(n = length(xi), mean = xi, sd = deviation)
    return(xi)
}


# Example I ----

time_sec <- 3600*24*c(1:365*2)   # t2 years
salinity_left = 35 / 10^3        # salinity at the left border (x=0), 35ppt
amplitude <- 15 / 10^3
stretch <-  365*3600*24 / 2 / pi #0.5*365/2/pi * 3600 * 24 
offset <- 0
deviation = 0

sal_t_left = sal.x.t(time_sec, salinity_left, amplitude, stretch, offset, deviation)

data.frame(x = time_sec/3600/24,
           y = sal_t_left*10^3) %>% 
    mutate(year = ifelse(x < 365.25, "1", "2")) %>%
    ggplot(., aes(x = x, y = y, col = year)) +
    geom_point() +
    geom_hline(yintercept = salinity_left*10^3) +
    geom_vline(xintercept = seq(1, 365.25, by = 30.25))



# Example II ----

time_sec <- 3600*24*c(1:365)   # time in sec., e.g. 2 years
salinity_left = 35 / 10^3        # salinity at the left border (x=0)

amplitude <- 15 / 10^3
stretch <- 3600 * 24 * 58
offset <- 0
deviation = 0

# Reference
ref = sal.x.t(time_sec = time_sec, 
              s_x = salinity_left, 
              amplitude = amplitude, 
              stretch = stretch, 
              offset = offset,
              deviation = deviation)

# Amplitude
am_a = sal.x.t(time_sec = time_sec, 
                s_x = salinity_left, 
                amplitude = amplitude / 2, 
                stretch = stretch, 
                offset = offset,
                deviation = deviation)

am_b = sal.x.t(time_sec = time_sec, 
                s_x = salinity_left, 
                amplitude = amplitude * 2, 
                stretch = stretch, 
                offset = offset,
                deviation = deviation)
    
p1 = data.frame(x = time_sec / 3600 / 24,
                ref, am_a, am_b) %>% 
    gather(., key, value, ref, am_a, am_b) %>% 
    mutate(value = value * 10^3) %>% 
    mutate(key = factor(key,
                           levels = c("ref", "am_a", "am_b"),
                           labels = c("ref", "ref/2", "ref*2"))) %>% 
    ggplot(., aes(x = x, y = value, col = key, linetype = key)) +
    geom_point(size = 1) +
    geom_hline(yintercept = salinity_left*10^3) +
    scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
    labs(x = "Time (days)",
         y = "Salinity at x=0 (ppt)",
         col = "Amplitude",
         linetype = "Amplitude") +
    theme(legend.position = c(0.9, 0.8))

# Deviation

dev_a = sal.x.t(time_sec = time_sec, 
                s_x = salinity_left, 
                amplitude = amplitude, 
                stretch = stretch, 
                offset = offset,
                deviation = deviation + 1/10^3)

dev_b = sal.x.t(time_sec = time_sec, 
                s_x = salinity_left, 
                amplitude = amplitude, 
                stretch = stretch, 
                offset = offset,
                deviation = deviation + 3/10^3)



p2 = data.frame(x = time_sec / 3600 / 24,
                ref, dev_a, dev_b) %>% 
    gather(., key, value, ref, dev_a, dev_b) %>% 
    mutate(value = value * 10^3) %>% 
    mutate(key = factor(key,
                        levels = c("ref", "dev_a", "dev_b"),
                        labels = c("ref", "1", "3"))) %>% 
    ggplot(., aes(x = x, y = value, col = key, linetype = key)) +
    geom_point(size = 1) +
    geom_hline(yintercept = salinity_left*10^3) +
    scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
    labs(x = "Time (days)",
         y = "Salinity at x=0 (ppt)",
         col = "Deviation",
         linetype = "Deviation") +
    theme(legend.position = c(0.9, 0.8))

# Offset along time 
off_a = sal.x.t(time_sec = time_sec, 
                s_x = salinity_left, 
                amplitude = amplitude, 
                stretch = stretch, 
                offset = offset - pi/2,
                deviation = deviation)

off_b = sal.x.t(time_sec = time_sec, 
                s_x = salinity_left, 
                amplitude = amplitude, 
                stretch = stretch, 
                offset = offset + pi/2,
                deviation = deviation)


p3 = data.frame(x = time_sec / 3600 / 24,
                ref, off_a, off_b) %>% 
    gather(., key, value, ref, off_a, off_b) %>% 
    mutate(value = value * 10^3) %>% 
    mutate(key = factor(key,
                        levels = c("ref", "off_a", "off_b"),
                        labels = c("ref", "-pi/2", "+pi/2"))) %>% 
    ggplot(., aes(x = x, y = value, col = key, linetype = key)) +
    geom_point(size = 1) +
    geom_hline(yintercept = salinity_left*10^3) +
    scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
    labs(x = "Time (days)",
         y = "Salinity at x=0 (ppt)",
         col = "Offset",
         linetype = "Offset") +
    theme(legend.position = c(0.9, 0.8))

# Horizontal stretch, i.e., defining the number of cycles per time period
str_a = sal.x.t(time_sec = time_sec, 
              s_x = salinity_left, 
              amplitude = amplitude, 
              stretch = stretch * 2, 
              offset = offset,
              deviation = deviation)

str_b = sal.x.t(time_sec = time_sec, 
              s_x = salinity_left, 
              amplitude = amplitude, 
              stretch = stretch / 2, 
              offset = offset,
              deviation = deviation)



p4 = data.frame(x = time_sec / 3600 / 24,
                ref, str_a, str_b) %>% 
    gather(., key, value, ref, str_a, str_b) %>% 
    mutate(value = value * 10^3) %>% 
    mutate(key = factor(key,
                        levels = c("ref", "str_a", "str_b"),
                        labels = c("ref", "ref*2", "ref/2"))) %>% 
    ggplot(., aes(x = x, y = value, col = key, linetype = key)) +
    geom_point(size = 1) +
    geom_hline(yintercept = salinity_left*10^3) +
    scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
    labs(x = "Time (days)",
         y = "Salinity at x=0 (ppt)",
         col = "Stretch",
         linetype = "Stretch") +
    theme(legend.position = c(0.9, 0.8))




ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
          # legend = "bottom",
          labels = c("(a)", "(b)", "(c)", "(d)"))

ggsave("salinity_stretch_offset.jpg",
       width = 10, height = 7)
