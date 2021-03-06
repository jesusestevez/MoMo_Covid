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
  geom_polygon(aes(fill = daily_cases_PCR_avg7), color = "white")+
  theme_minimal()
#
#
# The Theme:

theme_ari_maps <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "ivory1", color = NA),
      panel.background = element_rect(fill = "ivory1", color = NA),
      legend.background = element_rect(fill = "ivory1", color = NA),
      panel.border = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
      plot.caption = element_text(size = 7.5, color = "grey40"),
      legend.title = element_text(color = "grey40", size = 8),
      legend.text = element_text(color = "grey40", size = 7, hjust = 0),
      legend.position = c(0.7, 0.07),
      legend.text.align = 0,
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(2,0.2,.2,0.2), "cm"))
}

# The Colors:

pal <- wes_palette("Zissou1", 5, type = "discrete")

#Getting the quantiles:
quantile(regional_plot$daily_cases_PCR_avg7, probs = c(.2,.4,.6,.8), na.rm = TRUE)
#This returns  57.7  97.0 130.7 203.4 

#I'm going to slightly change the breaks to make them prettier
#again, this fully depends on your preferences
pretty_breaks <- c( 60 , 100, 150, 200 )

# Getting the minimum and maximum value to surround the breaks
minVal <- min(regional_plot$daily_cases_PCR_avg7, na.rm = T)
maxVal <- max(regional_plot$daily_cases_PCR_avg7, na.rm = T)

#Putting them together:
brks <- c(minVal, pretty_breaks, maxVal)

# Creating labels
labels <- c()
# round the extremes
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

regional_plot$brks <- cut(regional_plot$daily_cases_PCR_avg7, 
                           breaks = brks, 
                           include.lowest = TRUE, 
                           labels = labels)

brks_scale <- levels(regional_plot$brks)
labels_scale <- rev(brks_scale)



regional_plot%>%
  #you should be using the following aesthetics for any plot you make:
  ggplot(aes(x=long, y = lat, group = group))+
  #we use brks for the fill and resuce the size of the borders
  geom_polygon(aes(fill=brks), color = "white", size = 0.3)+
  #Adding the color palette 
  #AND setting how I want the scale to look like
  scale_fill_manual(
    values = rev(pal), #I use rev so that red is for lowest values 
    breaks = rev(brks_scale),
    name = "Daily Cases",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(direction = "horizontal",
                         keyheight = unit(2, units = "mm"),
                         keywidth = unit(50 / length(labels), units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 1,
                         nrow = 1,
                         byrow = T,
                         reverse = T,
                         label.position = "bottom"))+
  labs(title="Covid in Spain: Daily cases PCR 7 DAYS AVG",
    
       caption = "Jesus Estevez-Sanchez - Data: escovid19data")+
  theme_ari_maps()