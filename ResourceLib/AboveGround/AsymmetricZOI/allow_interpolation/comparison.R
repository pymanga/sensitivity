library(tidyverse)
library(data.table)
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette =
                                                                                       "Dark2")
theme_set(theme_bw())

fs = list.files(recursive = T) %>%
  str_subset(pattern = "Population.csv")
length(fs)
trees = data.frame()

for (f in fs) {
  s = strsplit(strsplit(f, "/")[[1]][1], "_")[[1]]
  print(s)

  t = fread(f) %>%
    mutate(mesh = as.numeric(gsub("-", ".", s[2])),
           module = s[3])
  trees = bind_rows(trees, t)
}

trees %>% 
  filter(time == max(time)) %>% 
  gather(., key, value, r_stem:r_root, ag_resources:bg_resources) %>% 
  group_by(plant, module, key) %>% 
  mutate(value = value/mean(value) - 1,
         module = factor(module,
                         levels = c("AZOI", "symZOI"),
                         labels = c("asymZOI", "symZOI")),
         key = factor(key,
                      levels = c("r_stem", "h_stem", "r_crown", "r_root",
                                 "ag_resources", "bg_resources"),
                      labels = c("r_stem", "h_stem", "r_crown", "r_root",
                                 "res_ag", "res_bg"))) %>% 
  ggplot(., aes(x = key, y = value,
                fill = factor(mesh),
                linetype = factor(mesh))) +
  geom_col(position = "dodge") +
  facet_grid(module~plant, scales = "free") +
  labs(x = "",
       y = "value:mean(value)-1",
       linetype = "Mesh size (m)",
       fill = "Mesh size (m)")

ggsave("geomtry_resources.jpg", width = 10, height = 5)
