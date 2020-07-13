library("idbr")
library("purrr")
library("dplyr")


yrs <-  seq(1980, 2030, by = 5)

df <- map_df(c("male", "female"), function(sex){
  mutate(idb1("US", yrs, sex = sex), sex_label = sex)
})

names(df) <- tolower(names(df))

df <- df %>%
  mutate(population = pop*ifelse(sex_label == "male", -1, 1))

series <- df %>% 
  group_by(sex_label, age) %>% 
  do(data = list(sequence = .$population)) %>% 
  ungroup() %>% 
  group_by(sex_label) %>% 
  do(data = .$data) %>%
  mutate(name = sex_label) %>% 
  list_parse()

maxpop <- max(abs(df$population))

xaxis <- list(categories = sort(unique(df$age)),
              reversed = FALSE, tickInterval = 5,
              labels = list(step = 5))

hc_us_demography =
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_motion(enabled = TRUE, labels = yrs, series = c(0,1), autoplay = TRUE, updateInterval = 1) %>% 
  hc_add_series_list(series) %>% 
  hc_plotOptions(
    series = list(stacking = "normal"),
    bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
  ) %>% 
  hc_tooltip(shared = TRUE) %>% 
  hc_yAxis(
    labels = list(
      formatter = JS("function(){ return Math.abs(this.value) / 1000000 + 'M'; }") 
    ),
    tickInterval = 0.5e6,
    min = -maxpop,
    max = maxpop) %>% 
  hc_xAxis(
    xaxis,
    rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))
  ) %>% 
  hc_tooltip(shared = FALSE,
             formatter = JS("function () { return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")
  ) %>% 
  hc_title(text = "Pyramides des âges aux Etats-Unis")


export_minio_graph(hc_us_demography, perim = "US", folder_name = "us_demo_pyramid")
