require(tidyverse)
require(ggspatial)
require(sf)
require(maps)

bbox<-tibble(x=c(-122.4339, -121.25),
             y=c(37.79301, 38.53464))%>%
  st_as_sf(coords=c("x", "y"), crs=4326)%>%
  st_transform(crs=26910)

base<-deltamapr::WW_Watershed%>%
  st_transform(crs=26910)

labels<-tibble(label=c("San Francisco Bay", "San Pablo Bay", "Suisun Bay", "Suisun Marsh",
                       "Confluence", "Cache Slough", "Sacramento River", "San Joaquin River", "Napa River",
                       "Sacramento Ship Channel", "Cosumnes\nRiver", "Mokelumne River",
                       "Frank's Tract", "Twitchell Island", "WWTP", "Discovery Bay", "Stockton", "SWP intake", "CVP intake"),
               Latitude=c(37.9, 38.07, 38.08, 38.2, 38.046, 38.24, 38.50000, 37.9, 38.23, 38.51892, 38.35944, 38.2,
                          38.044517, 38.108851, 38.442691, 37.906527, 37.956259, 37.801337, 37.796578),
               Longitude=c(-122.4, -122.4, -122.05, -122.05, -121.9, -121.69, -121.5600, -121.325, -122.3, -121.588, -121.3404, -121.335,
                           -121.601559, -121.654237, -121.474878, -121.598779, -121.292544, -121.620318, -121.585486),
               label_lat=c(37.9, 38.11, 38.15, 38.25, 38, 38.2, 38.49785, 37.88, 38.25, 38.54994, 38.3, 38.11588,
                           37.963091, 38.138315, 38.4, 37.92, 38.05, 37.84, 37.78),
               label_lon=c(-122.25, -122.38, -122.18, -122.18, -122, -121.8, -121.4, -121.773246, -122.37, -121.8, -121.28, -121.35,
                           -121.773246, -121.8, -121.42, -121.773246, -121.3, -121.773246, -121.773246))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=26910)%>%
  mutate(X=st_coordinates(.)[,"X"], Y=st_coordinates(.)[,"Y"])%>%
  st_drop_geometry()%>%
  st_as_sf(coords=c("label_lon", "label_lat"), crs=4326)%>%
  st_transform(crs=26910)%>%
  mutate(label_X=st_coordinates(.)[,"X"], label_Y=st_coordinates(.)[,"Y"])%>%
  st_drop_geometry()

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))%>%
  st_transform(crs=st_crs(base))
california<-filter(states, ID=="california")

base2<-base%>%
  st_crop(st_bbox(bbox))

lims<-st_bbox(bbox)

pout<-ggplot(states)+
  geom_sf(color="dodgerblue3")+
  geom_sf(data=base2, color="dodgerblue3", fill="dodgerblue3")+
  geom_rect(xmin = lims["xmin"]-22000, xmax = lims["xmax"]+22000, ymin = lims["ymin"]-22000, ymax = lims["ymax"]+22000,
            fill = NA, colour = "black", size = 0.7)+
  coord_sf(xlim=c(st_bbox(california)["xmin"], st_bbox(california)["xmax"]), ylim=c(st_bbox(california)["ymin"], st_bbox(california)["ymax"]))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "dodgerblue3"), axis.text.x=element_text(angle=45, hjust=1))
pout

p<-ggplot() +
  geom_sf(data=base, fill="slategray3", color="slategray4")+
  geom_segment(data=labels, aes(x=label_X, y=label_Y, xend=X, yend=Y), arrow=arrow(type="closed", length=unit(0.1, "inches")), size=1)+
  geom_label(data=labels, aes(label=label, x=label_X, y=label_Y))+
  coord_sf(xlim=c(lims["xmin"], lims["xmax"]), ylim=c(lims["ymin"], lims["ymax"]))+
  scale_fill_brewer(type="qual", palette="Set1", name="Survey")+
  scale_shape_manual(values=21:25, name="Survey")+
  ylab("")+
  xlab("")+
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", pad_y=unit(0.05, "npc"), which_north = "true")+
  theme_bw()+
  theme(legend.background = element_rect(color="black"), legend.position=c(0.925,0.85))+
  annotation_custom(
    grob = ggplotGrob(pout),
    xmin = -Inf,
    xmax = 570000,
    ymin = 4240000,
    ymax = Inf
  )


ggsave("SBDS_2022_map.png", plot=p, device="png", width=8, height=8, units = "in")
