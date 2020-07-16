

library(lubridate)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rsdmx)

runtime_start = Sys.time()

# EONIA volume
EONIA_vol = as.data.frame(
  readSDMX(providerId = "ECB", resource = "data", flowRef = "EON",
           key = "D.EONIA_TO.VOLUME",verbose=F)
)

EONIA_vol[,"type"] = "Volume en milliards d'euros"
EONIA_vol[,"obsValue"] = EONIA_vol[,"obsValue"]/1000
EONIA_vol[,"TITLE"] = "EONIA"

# EONIA yield
EONIA_yield = as.data.frame(
  readSDMX(providerId = "ECB", resource = "data", flowRef = "EON",
           key = "D.EONIA_TO.RATE",verbose=F)
)
EONIA_yield[,"type"] = "Yield en %"
# EONIA_yield[,"TITLE"] = paste("EONIA : ", EONIA_yield[,"TITLE"])

# ESTER volume
ESTER_vol = as.data.frame(
  readSDMX(providerId = "ECB", resource = "data", flowRef = "EST",
           key = "B.EU000A2X2A25.TT",verbose=F)
)
ESTER_vol[,"type"] = "Volume en milliards d'euros"
ESTER_vol[,"obsValue"] = ESTER_vol[,"obsValue"]/1000
ESTER_vol[,"TITLE"] = "ESTER"

ESTER_yield_25th = as.data.frame(
  readSDMX(providerId = "ECB", resource = "data", flowRef = "EST",
           key = "B.EU000A2X2A25.R25",verbose=F)
)
ESTER_yield_25th[,"type"] = "Yield"

ESTER_yield_75th = as.data.frame(
  readSDMX(providerId = "ECB", resource = "data", flowRef = "EST",
           key = "B.EU000A2X2A25.R75",verbose=F)
)
ESTER_yield_75th[,"type"] = "Yield"

df = bind_rows(ESTER_vol, EONIA_vol, EONIA_yield) %>% 
  select(TITLE, type, obsTime, obsValue)



start_date = as.Date('2015-01-01')

df = df %>% 
  filter(obsTime >= start_date)

min_date = start_date
max_date = today() %m+% months(1)

xaxis_breaks = seq.Date(from = start_date, to = max_date, by = "3 months")

day_subtitle = day(max(df$obsTime))
month_subtitle = as.character(month(max(df$obsTime) , label = TRUE, abbr= FALSE))

last_value = df %>% 
  group_by(type) %>% 
  filter(obsTime == max(obsTime))

rows1 = which(last_value[,"TITLE"] == "ESTER")
rows2 = which(last_value[,"TITLE"] != "ESTER")

data_subtitle = sprintf("Volume : %s milliards d'euros , Yield : %s%%", round(last_value[rows1,"obsValue"]) , last_value[rows2,"obsValue"])

subtitle = sprintf(" BCE, Dernier point: %s %s \n %s",
                   day_subtitle,month_subtitle,data_subtitle)

gg_EONIA_volume = 
  ggplot(data = df,aes(x = as.Date(obsTime),y = obsValue, colour = TITLE)) + 
  facet_wrap(~type, scales= "free") +
  geom_line() +
  ggtitle("Transactions au jour-le-jour sur le marché monétaire européen") +
  labs(x = "", y = "Milliards", subtitle = subtitle) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y") + 
  theme_stata() + 
  theme(axis.text.y = element_text(angle = 0 ),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.subtitle=element_text(size = 12, hjust = 0.5, face = "italic", color = "black")) 

runtime_end = Sys.time()
run_time = as.numeric(difftime(runtime_end, runtime_start), units = "secs")

export_minio_graph(gg_EONIA_volume,
                   perim = "FI", folder_name = "EONIA_volume",
                   update = TRUE,
                   run_time = run_time)
