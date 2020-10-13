# https://www.njtierney.com/post/2020/10/11/times-scales-covid/

if (!require("pacman")) install.packages("pacman")
pacman::p_load(scales,lubridate,RCurl, sf, hrbrthemes, lwgeom, rgdal, broom, wesanderson, rnaturalearth, maps, mapdata, spData, tigris, tidycensus, leaflet, tmap, tmaptools, tidyverse, rvest, polite, readr, knitr, readxl)

## My preferred ggplot2 plotting theme (optional)
theme_set(hrbrthemes::theme_ipsum())

#First, I am going to read the isc3 excess deaths data for regions from the web
data_excess_deaths <- read.csv("https://momo.isciii.es/public/momo/data")

#Second, I am going to read the data provided by montera34

x <- getURL("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-ccaa-spain_consolidated.csv")
data_ccaa <- read.csv(text = x)

#now I want to get the spain map
#To complete this task, I am going to rely on https://github.com/aaumaitre/maps_Spain

# read the regional shapefile:
sf_regional <- readOGR("C:\\Users\\Usuario\\Desktop\\covid_momo\\ComunidadesAutonomas_ETRS89_30N\\Comunidades_Autonomas_ETRS89_30N.shp")

#Convert it to a dataframe by using the tidy function of the broom package:

regional_df <- tidy(sf_regional)

# There's a slight problem here, which is that by doing tidy(), we're losing all information identifying our regional units 
# (the "id" column doesn't really tell us which region corresponds to each id). This can be easily fixed by creating a 
# temporary data frame with the name/code of the regions (which is in the original sahapefile) and then joining it to our
# regional_df df.

# Recover row name
temp_df <- data.frame(sf_regional$Texto)

temp_df$id <- as.character(seq(0,nrow(temp_df)-1))
#in this case, we make it to start with a 0 to be consistent with the shapefile informaiton in the merging process.

#now, we make a left join of the data

regional_df2 <- left_join(regional_df, temp_df, by = 'id')

#now, in order to be able to join the data, I have to rename the ccaa column id as region

regional_df2 <-regional_df2 %>% rename(region=id)

#now, since the region is not of integer type, we have to write it as numeric if we want to sumar 1

regional_df2$region = as.numeric(as.character(regional_df2$region))

sapply(regional_df2, class)

regional_df2<-regional_df2 %>% mutate(region=region+1)

#now, I want to see whether the fecha de defuncion en excess esta en formato fecha

sapply(data_excess_deaths, class)

#Ahora tengo que cambiar el codigo ine ambito de excess deaths por region

data_excess_deaths<-data_excess_deaths%>%rename(region=cod_ine_ambito)

#Y ahora cambio el NA por un 0, que representa el total
#pero primero lo tengo que cambiar todo a numerico

data_excess_deaths$region = as.numeric(as.character(data_excess_deaths$region))

data_excess_deaths<-data_excess_deaths%>%mutate(region=replace_na(region,0))

#ahora renombro la fecha como date
data_excess_deaths<-data_excess_deaths%>%rename(date=fecha_defuncion)

#vale, una vez que tenemos esto, lo que quiero hacer es simplemente un join de regionaldf2 con el excess, para verlo mejor
#regional_plot<-left_join(data_excess_deaths_nospain, regional_df2, by ='region')
#esto me da error porque seria un many to many  y eso imposible es ahora, tendria que ser con un shiny o algo asi


#ahora bien, lo que me pasa ahora es que quiero comprobar los datos por ccaa
data_ccaa<-data_ccaa%>%rename(region=ine_code)
sapply(data_excess_deaths, class)
sapply(data_ccaa, class)
#como no coinciden las clases de las dates, tengo que cambiarlas en data_ccaa usando lubridate
data_ccaa$date<-as.POSIXct(data_ccaa$date)
data_ccaa$date<-as.Date(data_ccaa$date)
data_excess_deaths$date<-as.Date(data_excess_deaths$date)

sapply(data_excess_deaths, class)
sapply(data_ccaa, class)

#y ahora puedo proceder a join ambos, eliminando primero ceuta y melilla
#recuerda que aqui dejamos todo lo distinto a lo que eliminamos
data_excess_deaths<-data_excess_deaths[data_excess_deaths$region != 18 & data_excess_deaths$region != 19,]
data_excess_deaths_nospain<-data_excess_deaths[data_excess_deaths$region != 0,]

regional_plot<-left_join(data_excess_deaths, data_ccaa, by = c('region','date'))

#tenemos muchos not a number debido a la falta de informacion, pero algo podremos hacer


data_excess_deaths_nospain %>%
  ggplot(aes(x=date, y=defunciones_observadas, col=fct_reorder2(nombre_ambito,date,defunciones_observadas))) + 
  geom_point(alpha = 0.7) +
  labs(
    title = "Defunciones observadas en España",
    x = "Date", y = "Time",
    caption = "Source: Myself"
  ) +
  theme(legend.title = element_blank()) ## Switch off legend title

#now, to plot the line instead of points:
regional_plot_nospain<-regional_plot[regional_plot$region != 18 & regional_plot$region != 19& regional_plot$region != 0,]


#vamos a guardar las nuevas bases de datos.

#save the dataframe that includes the map of spain:
save(regional_df2, file='regional_df2.RData')

#I do not want to save the data about the excess deaths nor the ccaa, since they have to be updated.


regional_plot_nospain %>%
  ggplot(aes(x=date, y=defunciones_observadas, col=fct_reorder2(ccaa,date,defunciones_observadas))) + 
  geom_line() +
  labs(
    title = "Defunciones observadas en España",
    x = "Date", y = "Time",
    caption = "Source: Myself"
  ) +
  theme(legend.title = element_blank()) ## Switch off legend title

#now, to make it better, I will erase from january backwards in regional_plot_nospain

#intensive_care_per_1000000 en España
bp <- 
  ggplot(regional_plot_nospain,aes(x=date, y=intensive_care_per_1000000, col=fct_reorder2(ccaa,date,PCR))) + 
  geom_line(size=1) + scale_x_date(limits = as.Date(c("2020-01-01","2020-10-13"))) + 
  labs(
    title = "intensive_care_per_1000000",
    x = "Date", y = "intensive_care_per_1000000",
    caption = "Source: Myself"
  ) +
  theme(legend.title = element_blank()) ## Switch off legend title

bp

#split it by region

bp + facet_wrap(vars(ccaa))

#defunciones_observadas en España
#voy a modificarlo para ver solo España
regional_plot_spain<-regional_plot[regional_plot$region < 1,]
#y ahora quiero solo cada 7 dias los datos, no solo limitar el eje x, por lo que
#•genero un vector y luego lo meto

vector <- seq(1,8988,6)

regional_plot_spain <- regional_plot_spain[-c(vector),]

#Y ahora solo para males
regional_plot_spain_males<-regional_plot_spain[regional_plot_spain$cod_sexo != 6 & regional_plot_spain$cod_sexo != 'all',]

bp <- 
  ggplot(regional_plot_spain_males,aes(x=date, y=defunciones_observadas, col=fct_reorder2(cod_gedad,date,PCR))) + 
  geom_line(size=1) + scale_x_date(limits = as.Date(c("2020-01-01","2020-10-13")), breaks = "1 week") + 
  labs(
    title = "defunciones_observadas",
    x = "Date", y = "defunciones_observadas",
    caption = "Source: Myself"
  ) +
  theme(legend.title = element_blank(),axis.text.x=element_text(angle=90, hjust=1)) ## Switch off legend title

bp

#split it by region

bp + facet_wrap(vars(ccaa))





