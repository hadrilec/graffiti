
library("dlstats")
library(lubridate)
library(cranlogs)
library(tidyverse)
library(RColorBrewer)

df = cranlogs::cran_downloads(c("insee", "rwebstat", "eurostat", "rsdmx",
                                "ecb", "rdbnomics"),
                              from = "2020-01-01", to = lubridate::today()) %>%
  mutate(count = case_when(count == 0 ~ NA_real_, TRUE ~ as.numeric(count)))

ggplot(df, aes(x = date, y = count, colour = package)) +
  geom_line() + geom_point()

df2 = df %>% filter(date >= "2020-07-05")

mycolors = c(brewer.pal(8, "Set1")[-6], brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))

last_values = df2 %>%
  drop_na() %>%
  group_by(package) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(date = date %m+% days(2))

gg_pkg =
ggplot(df2, aes(x = date, y = count, colour = package)) +
  geom_line() +
  geom_point() +
  geom_text(data = last_values, aes(x = date, y = count, label = package)) +
  scale_colour_manual(values = mycolors)
gg_pkg
