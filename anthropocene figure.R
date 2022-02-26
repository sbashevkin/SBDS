library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

temp_data<-read_csv("Delta_Yolo_water_temp_mean.csv")%>%
  mutate(Date=mdy(Date),
    Year=year(Date))

temp_data_25A<-temp_data%>%
  group_by(`Station Name`, Year)%>%
  summarise(modeled_days_over_25=length(which(GFDL.CM3_rcp45>25)),
            measured_days_over_25=length(which(livneh_historical>25)), .groups="drop")%>%
  group_by(Year)%>%
  summarise(across(c(modeled_days_over_25, measured_days_over_25), mean))%>%
  mutate(Calculation="Calculate days over 25, then average across stations",
         measured_days_over_25=if_else(Year>=2014, NA_real_, measured_days_over_25))

temp_data_25B<-temp_data%>%
  group_by(Date, Year)%>%
  summarise(across(c(GFDL.CM3_rcp45, livneh_historical), mean), .groups="drop")%>%
  group_by(Year)%>%
  summarise(modeled_days_over_25=length(which(GFDL.CM3_rcp45>25)),
            measured_days_over_25=length(which(livneh_historical>25)), .groups="drop")%>%
  mutate(Calculation="Average across stations, then calculate days over 25",
         measured_days_over_25=if_else(Year>=2014, NA_integer_, measured_days_over_25))

combined<-temp_data_25A%>%
  bind_rows(temp_data_25B)

ggplot(combined, aes(x=Year, y=modeled_days_over_25, color=Calculation))+
  geom_line()+
  theme_bw()+
  theme(legend.position="bottom")

ggplot(combined, aes(x=Year, y=measured_days_over_25, color=Calculation))+
  geom_line()+
  theme_bw()+
  theme(legend.position="bottom")

#A seems to be better since it shows there are days over 25 in all years

write_csv(temp_data_25A, "Days_over_25.csv")
