# Load libraries
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(rcompanion)
####################### Average DAILY Temperature ##############################
# Load data
air_temp <- read_csv("acre_avg_air_temp.csv") # base file reading function may also be used

# convert the char field to date field
air_temp[[1]] <- as.Date(air_temp[[1]],"%m/%d/%Y")

# %>% is the pipe function (similar to Unix) available through dplyr library
# Filter all observations belonging to December 
x <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(month(Date)==12) %>%
  mutate(doy = day(Date)) %>%
  group_by(doy) %>%
  summarize(median_avg_air_temp = median(Air_Temp))

# To compute confidence bands (w.r.t medians)
x0 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

conf_interval = 0.95

cdata_median_rcom = groupwiseMedian(Air_Temp ~ doy,
                data       = x0, 
                conf       = conf_interval, 
                R          = 5000,
                percentile = TRUE, 
                bca        = FALSE, 
                digits     = 3)
# cdata = ddply(x0,"doy",summarise,
#               N    = sum(!is.na(Air_Temp)),
#               median = mean(Air_Temp, na.rm=TRUE),
#               mean = median(Air_Temp, na.rm=TRUE),
#               sd   = sd(Air_Temp, na.rm=TRUE),
#               se   = sd / sqrt(N),
#               ci = se * qt(conf_interval/2 + .5, N-1),
#               upr = mean + ci,
#               lwr = mean - ci)
# cdata
# 
# cdata_median = ddply(x0,"doy",summarise,
#                      N    = sum(!is.na(Air_Temp)),
#                      median = median(Air_Temp, na.rm=TRUE),
#                      upr = quantile(Air_Temp,probs=0.975,na.rm=TRUE),
#                      lwr = quantile(Air_Temp,probs=0.025,na.rm=TRUE))
# cdata_median


# Filter all observations belonging to December 2017
x17 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(year(Date)==2017) %>%  
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

# Filter all observations belonging to December 2018
x18 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(year(Date)==2018) %>%  
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

# Plot time-series data (Median, 2017, and 2018) and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("daily_avg_temp_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=doy)) 
gg2 <- gg1 +  geom_line(aes(y=median_avg_air_temp, col="Median 2002-18")) + 
  geom_point(aes(y=median_avg_air_temp, col="Median 2002-18")) + 
  geom_point(data=x17,aes(y=Air_Temp, col="Daily 2017")) +
  geom_line(data=x17, aes(y=Air_Temp, col="Daily 2017")) +
  geom_point(data=x18,aes(y=Air_Temp, col="Daily 2018")) +
  geom_line(data=x18, aes(y=Air_Temp, col="Daily 2018")) +
  labs(title="December Avg. Air Temperature", 
       subtitle="Station: ACRE, West Lafayette, IN", 
       caption="Data Source: iclimate.org", y="Air temperature (C)", x="Days") +  # title and caption
  scale_x_continuous(breaks = seq(1,31,2)) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("Median 2002-18"="#00ba38", "Daily 2017"="#f8766d", "Daily 2018"="#0072B2")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()


# Plot time-series data (ALL OBSERVATIONS) and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("all_daily_avg_temp_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=doy)) 
gg2 <- gg1 +  geom_line(aes(y=median_avg_air_temp, col="Median 2002-18")) + 
  geom_point(aes(y=median_avg_air_temp, col="Median 2002-18")) + 
  geom_point(data=x0,aes(y=Air_Temp),alpha=0.05) +
  geom_ribbon(data=cdata_median_rcom,aes(ymin=Percentile.lower,ymax=Percentile.upper,fill="95% CI"),alpha=0.2)+
  geom_point(data=x17,aes(y=Air_Temp, col="Daily 2017")) +
  geom_line(data=x17, aes(y=Air_Temp, col="Daily 2017")) +
  geom_point(data=x18,aes(y=Air_Temp, col="Daily 2018")) +
  geom_line(data=x18, aes(y=Air_Temp, col="Daily 2018")) +
  labs(title="December Avg. Air Temperature", 
       subtitle="Station: ACRE, West Lafayette, IN", 
       caption="Data Source: iclimate.org", y="Air temperature (C)", x="Days") +  # title and caption
  scale_x_continuous(breaks = seq(1,31,2)) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("Median 2002-18"="#00ba38", "Daily 2017"="#f8766d", "Daily 2018"="#0072B2")) +  # line color
  scale_fill_manual("", 
                     values = c("95% CI"="#00ba38")) +  # confidence band color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()

####################### MAXIMUM DAILY Temperature ##############################
# Load data
air_temp <- read_csv("acre_max_air_temp.csv") # base file reading function may also be used

# convert the char field to date field
air_temp[[1]] <- as.Date(air_temp[[1]],"%m/%d/%Y")

# %>% is the pipe function (similar to Unix) available through dplyr library
# Filter all observations belonging to December 
x <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(month(Date)==12) %>%
  mutate(doy = day(Date)) %>%
  group_by(doy) %>%
  dplyr::summarize(median_max_air_temp = median(Air_Temp))

# To compute confidence bands (w.r.t medians)
x0 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

conf_interval = 0.95

cdata_median_rcom = groupwiseMedian(Air_Temp ~ doy,
                                    data       = x0, 
                                    conf       = conf_interval, 
                                    R          = 5000,
                                    percentile = TRUE, 
                                    bca        = FALSE, 
                                    digits     = 3)

# Filter all observations belonging to December 2017
x17 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(year(Date)==2017) %>%  
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

# Filter all observations belonging to December 2018
x18 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(year(Date)==2018) %>%  
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

# Plot time-series data (Median, 2017, and 2018) and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("daily_max_temp_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=doy)) 
gg2 <- gg1 +  geom_line(aes(y=median_max_air_temp, col="Median 2002-18")) + 
  geom_point(aes(y=median_max_air_temp, col="Median 2002-18")) + 
  geom_point(data=x17,aes(y=Air_Temp, col="Daily 2017")) +
  geom_line(data=x17, aes(y=Air_Temp, col="Daily 2017")) +
  geom_point(data=x18,aes(y=Air_Temp, col="Daily 2018")) +
  geom_line(data=x18, aes(y=Air_Temp, col="Daily 2018")) +
  labs(title="December Max. Air Temperature", 
       subtitle="Station: ACRE, West Lafayette, IN", 
       caption="Data Source: iclimate.org", y="Air temperature (C)", x="Days") +  # title and caption
  scale_x_continuous(breaks = seq(1,31,2)) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("Median 2002-18"="#00ba38", "Daily 2017"="#f8766d", "Daily 2018"="#0072B2")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()


# Plot time-series data (ALL OBSERVATIONS) and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("all_daily_max_temp_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=doy)) 
gg2 <- gg1 +  geom_line(aes(y=median_max_air_temp, col="Median 2002-18")) + 
  geom_point(aes(y=median_max_air_temp, col="Median 2002-18")) + 
  geom_point(data=x0,aes(y=Air_Temp),alpha=0.05) +
  geom_ribbon(data=cdata_median_rcom,aes(ymin=Percentile.lower,ymax=Percentile.upper,fill="95% CI"),alpha=0.2)+
  geom_point(data=x17,aes(y=Air_Temp, col="Daily 2017")) +
  geom_line(data=x17, aes(y=Air_Temp, col="Daily 2017")) +
  geom_point(data=x18,aes(y=Air_Temp, col="Daily 2018")) +
  geom_line(data=x18, aes(y=Air_Temp, col="Daily 2018")) +
  labs(title="December Max. Air Temperature", 
       subtitle="Station: ACRE, West Lafayette, IN", 
       caption="Data Source: iclimate.org", y="Air temperature (C)", x="Days") +  # title and caption
  scale_x_continuous(breaks = seq(1,31,2)) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("Median 2002-18"="#00ba38", "Daily 2017"="#f8766d", "Daily 2018"="#0072B2")) +  # line color
  scale_fill_manual("", 
                    values = c("95% CI"="#00ba38")) +  # confidence band color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()


####################### MINIMUM DAILY Temperature ##############################
# Load data
air_temp <- read_csv("acre_min_air_temp.csv") # base file reading function may also be used

# convert the char field to date field
air_temp[[1]] <- as.Date(air_temp[[1]],"%m/%d/%Y")

# %>% is the pipe function (similar to Unix) available through dplyr library
# Filter all observations belonging to December 
x <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(month(Date)==12) %>%
  mutate(doy = day(Date)) %>%
  group_by(doy) %>%
  dplyr::summarize(median_min_air_temp = median(Air_Temp))

# To compute confidence bands (w.r.t medians)
x0 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

conf_interval = 0.95

cdata_median_rcom = groupwiseMedian(Air_Temp ~ doy,
                                    data       = x0, 
                                    conf       = conf_interval, 
                                    R          = 5000,
                                    percentile = TRUE, 
                                    bca        = FALSE, 
                                    digits     = 3)

# Filter all observations belonging to December 2017
x17 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(year(Date)==2017) %>%  
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

# Filter all observations belonging to December 2018
x18 <- air_temp %>% 
  select(Date,Air_Temp) %>% 
  filter(year(Date)==2018) %>%  
  filter(month(Date)==12) %>%
  mutate(doy = day(Date))

# Plot time-series data (Median, 2017, and 2018) and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("daily_min_temp_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=doy)) 
gg2 <- gg1 +  geom_line(aes(y=median_min_air_temp, col="Median 2002-18")) + 
  geom_point(aes(y=median_min_air_temp, col="Median 2002-18")) + 
  geom_point(data=x17,aes(y=Air_Temp, col="Daily 2017")) +
  geom_line(data=x17, aes(y=Air_Temp, col="Daily 2017")) +
  geom_point(data=x18,aes(y=Air_Temp, col="Daily 2018")) +
  geom_line(data=x18, aes(y=Air_Temp, col="Daily 2018")) +
  labs(title="December Min. Air Temperature", 
       subtitle="Station: ACRE, West Lafayette, IN", 
       caption="Data Source: iclimate.org", y="Air temperature (C)", x="Days") +  # title and caption
  scale_x_continuous(breaks = seq(1,31,2)) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("Median 2002-18"="#00ba38", "Daily 2017"="#f8766d", "Daily 2018"="#0072B2")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()


# Plot time-series data (ALL OBSERVATIONS) and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("all_daily_min_temp_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=doy)) 
gg2 <- gg1 +  geom_line(aes(y=median_min_air_temp, col="Median 2002-18")) + 
  geom_point(aes(y=median_min_air_temp, col="Median 2002-18")) + 
  geom_point(data=x0,aes(y=Air_Temp),alpha=0.05) +
  geom_ribbon(data=cdata_median_rcom,aes(ymin=Percentile.lower,ymax=Percentile.upper,fill="95% CI"),alpha=0.2)+
  geom_point(data=x17,aes(y=Air_Temp, col="Daily 2017")) +
  geom_line(data=x17, aes(y=Air_Temp, col="Daily 2017")) +
  geom_point(data=x18,aes(y=Air_Temp, col="Daily 2018")) +
  geom_line(data=x18, aes(y=Air_Temp, col="Daily 2018")) +
  labs(title="December Min. Air Temperature", 
       subtitle="Station: ACRE, West Lafayette, IN", 
       caption="Data Source: iclimate.org", y="Air temperature (C)", x="Days") +  # title and caption
  scale_x_continuous(breaks = seq(1,31,2)) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("Median 2002-18"="#00ba38", "Daily 2017"="#f8766d", "Daily 2018"="#0072B2")) +  # line color
  scale_fill_manual("", 
                    values = c("95% CI"="#00ba38")) +  # confidence band color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()