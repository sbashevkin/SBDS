library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)

temp_data<-read_csv("Delta_Yolo_water_temp_mean.csv")%>%
  mutate(Date=mdy(Date),
    Year=year(Date))

temp_data_25A<-temp_data%>%
  group_by(`Station Name`, Year)%>%
  summarise(Days_over_25=length(which(GFDL.CM3_rcp45>25)), .groups="drop")%>%
  group_by(Year)%>%
  summarise(Days_over_25=mean(Days_over_25))%>%
  mutate(Calculation="Calculate days over 25, then average across stations")

temp_data_25B<-temp_data%>%
  group_by(Date, Year)%>%
  summarise(GFDL.CM3_rcp45=mean(GFDL.CM3_rcp45), .groups="drop")%>%
  group_by(Year)%>%
  summarise(Days_over_25=length(which(GFDL.CM3_rcp45>25)), .groups="drop")%>%
  mutate(Calculation="Average across stations, then calculate days over 25")

combined<-temp_data_25A%>%
  bind_rows(temp_data_25B)

ggplot(combined, aes(x=Year, y=Days_over_25, color=Calculation))+
  geom_line()+
  theme_bw()+
  theme(legend.position="bottom")

#A seems to be better since it shows there are days over 25 in all years

write_csv(temp_data_25A, "Days_over_25.csv")
