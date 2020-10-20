if (!require("pacman")) install.packages("pacman")
pacman::p_load(hrbrthemes,viridis, scales,lubridate,RCurl, sf, hrbrthemes, lwgeom, rgdal, broom, wesanderson, rnaturalearth, maps, mapdata, spData, tigris, tidycensus, leaflet, tmap, tmaptools, tidyverse, rvest, polite, readr, knitr, readxl)

# First, I want to obtain the data about covid and provinces in Spain
x <- getURL("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-provincias-spain_consolidated.csv")
data_province <- read.csv(text = x)

data_province<-data_province%>%rename(region=ine_code)

data_province$date<-as.POSIXct(data_province$date)
data_province$date<-as.Date(data_province$date)
sapply(data_province, class)

#Now I want to generate a dataframe of only one day in order to generate the database for the map


single_day <- subset(data_province, date=="2020-09-17" )

#now I want to get the spain map
#To complete this task, I am going to rely on https://github.com/aaumaitre/maps_Spain

# read the province shapefile:

#https://rstudio-pubs-static.s3.amazonaws.com/571210_03ab303e7b934a8aa9c6e9d676edf16f.html

sf_regional <- readOGR("C:\\Users\\Usuario\\Desktop\\covid_momo\\Provincias_ETRS89_30N\\Provincias_ETRS89_30N.shp")


#Convert it to a dataframe by using the tidy function of the broom package:

provinces <- tidy(sf_regional)

# recover row names

nombres_provincias <- data.frame(sf_regional$Texto)
head(nombres_provincias)
# Create and append id
nombres_provincias$id <- as.character(seq(0, nrow(nombres_provincias)-1))
#Joining
regional_df2 <- left_join(provinces, nombres_provincias, by="id")

regional_df2 <-regional_df2 %>% rename(region=id)
sapply(regional_df2, class)

#now, since the region is not of integer type, we have to write it as numeric if we want to sumar 1

regional_df2$region = as.numeric(as.character(regional_df2$region))

sapply(regional_df2, class)

single_day<-single_day %>% mutate(region=region)
regional_df2<-regional_df2 %>% mutate(region=region+1)

#joining with the regional df
regional_plot <- regional_df2%>%
  left_join(single_day, by = "region")

regional_plot%>%ggplot(aes(x=long, y = lat, group = group))+
  #ri_disp is the income variable
  geom_polygon(aes(fill = PCR), color = "white")+
  theme_minimal()





quantile(regional_plot$PCR, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)

corte <- c(96 , 120, 200, 266)

val_min <- min(regional_plot$PCR)
val_max <- max(regional_plot$PCR)
breaks <- c(val_min, corte, val_max)
regional_plot$breaks <- cut(regional_plot$PCR,
                                      breaks = breaks,
                                      include.lowest = T)

breaks_scale <- levels(regional_plot$breaks)
labels_scale <- rev(breaks_scale)



colores <- wes_palette("Moonrise3", 5, type = "discrete")

segovia <- subset(regional_plot, region==39)


segovia %>%
  ggplot(aes(x=long, y= lat, group = group)) +
  geom_polygon(aes(fill=breaks), color= "white", size = 0.2) +
  labs( title = "Tasa Bruta de Mortalidad por Provincia",
        subtitle = "Defunciones por mil habitantes",
        caption = "Fuente: INE",
        fill = "") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "snow", color = NA),
    panel.background = element_rect(fill= "snow", color = NA),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(color = "grey40", size = 8),
    legend.text = element_text(color = "grey40", size = 7, hjust = 0),
    legend.position = c(0.93, 0.3),
    plot.margin = unit(c(0.5,2,0.5,1), "cm")) +
  scale_fill_manual(
    values = rev(colores),
    breaks = rev(breaks_scale))











