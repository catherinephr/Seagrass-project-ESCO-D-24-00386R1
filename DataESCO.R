##ESCO-D-24-00386R1##
##Catherine Hernandez##


#####Load Libraries####
#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl")
library(readxl) 
#install.packages("ggstatsplot")
library(ggstatsplot)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("gridExtra")
library(gridExtra)
library(scales)
#install.packages("ggthemes")
library(ggthemes)
library(dplyr)
#install.packages("lmodel2")  
library(lmodel2)
#install.packages("ggmap")
library(ggmap)
library(broom)
library(interp)



#Data

CTD_Laurel <- read_excel("CTD_Laurel.xlsx")
CTD_Magueyes <- read_excel("CTD_Magueyes.xlsx")
Laurel_MiniDOT <- read_excel("Laurel_MiniDOT.xlsx")
Magueyes_MiniDOT <- read_excel("Magueyes_MiniDOT.xlsx")
CurrentMagueyes <- read_excel("CurrentMagueyes.xlsx")
CurrentLaurel <- read_excel("CurrentLaurel.xlsx")
CatPARLaurel <- read_excel("Seagrass_Sensors/Laurel/MiniPAR/CatPARLaurel.xlsx")
CatPARMagueyes <- read_excel("Seagrass_Sensors/Magueyes/MiniPAR/CatPARMagueyes.xlsx")



# Same Time format
CTD_Laurel$TimeStamp <- as.POSIXct(CTD_Laurel$TimeStamp, tz = "UTC")
CTD_Magueyes$TimeStamp <- as.POSIXct(CTD_Magueyes$TimeStamp, tz = "UTC")
Laurel_MiniDOT$`Bolivia Time` <- as.POSIXct(Laurel_MiniDOT$`Bolivia Time`, tz = "UTC")
Magueyes_MiniDOT$`Bolivia Time` <- as.POSIXct(Magueyes_MiniDOT$`Bolivia Time`, tz = "UTC")
CatPARLaurel$`Bolivia Time` <- as.POSIXct(CatPARLaurel$`Bolivia Time`, tz = "UTC")
CatPARMagueyes$`Bolivia Time` <- as.POSIXct(CatPARMagueyes$`Bolivia Time`, tz = "UTC")
CurrentLaurel$`Time` <- as.POSIXct(CurrentLaurel$`Time`, tz = "UTC")
CurrentMagueyes$`Time` <- as.POSIXct(CurrentMagueyes$`Time`, tz = "UTC")

# start and end of the sampling period
fecha_inicio <- as.POSIXct("2023-07-14 10:00:02", tz = "UTC")
Surveys_fin <- as.POSIXct("2023-07-24 15:00:00", tz = "UTC")
fecha_fin_30min <- Surveys_fin + minutes(30)  


# Surveys
Prod_inicioL <- as.POSIXct("2023-07-14 10:45:47", tz = "UTC")
Prod_finL <- as.POSIXct("2023-07-21 10:35:47", tz = "UTC")

Prod_inicioM <- as.POSIXct("2023-07-17 12:10:02", tz = "UTC")
Prod_finM <- as.POSIXct("2023-07-24 12:30:02", tz = "UTC")

Surveys_inicio <- as.POSIXct("2023-07-24 05:00:00", tz = "UTC")
Surveys_fin <- as.POSIXct("2023-07-24 15:00:00", tz = "UTC")

# Filtered data
data_recortadaLaurel1 <- CTD_Laurel %>%
  filter(TimeStamp >= fecha_inicio & TimeStamp <= fecha_fin_30min)

data_recortadaMagueyes1 <- CTD_Magueyes %>%
  filter(TimeStamp >= fecha_inicio & TimeStamp <= fecha_fin_30min)

data_recortadaLaurel_DO <- Laurel_MiniDOT %>%
  filter(`Bolivia Time` >= fecha_inicio& `Bolivia Time` <= fecha_fin_30min)

data_recortadaMagueyes_DO <- Magueyes_MiniDOT %>%
  filter(`Bolivia Time` >= fecha_inicio & `Bolivia Time` <= fecha_fin_30min)

data_recortadaLaurel_PAR <- CatPARLaurel %>%
  filter(`Bolivia Time` >= fecha_inicio & `Bolivia Time` <= fecha_fin_30min)

data_recortadaMagueyes_PAR <- CatPARMagueyes %>%
  filter(`Bolivia Time` >= fecha_inicio & `Bolivia Time` <= fecha_fin_30min)

data_recortadaLaurel_current <- CurrentLaurel %>%
  filter(Time >= fecha_inicio & Time <= fecha_fin_30min)

data_recortadaMagueyes_current <- CurrentMagueyes %>%
  filter(Time >= fecha_inicio & Time <= fecha_fin_30min)

# TEMPERATURE PLOT

Temperature_plot <- ggplot() +
  geom_line(data = data_recortadaLaurel1, aes(x = `TimeStamp`, y = `Temperature`), linewidth = 0.6, color = "#A2CD5A") +
  geom_line(data = data_recortadaMagueyes1, aes(x = `TimeStamp`, y = `Temperature`), linewidth = 0.6, color = "#009EA6") +
  scale_x_datetime(limits = c(fecha_inicio, fecha_fin_30min), labels = scales::date_format("%d"), date_breaks = "1 day", timezone = "UTC") +
  scale_y_continuous(limits = c(29, 31.5)) +
  ylab(bquote(bold("Temperature (°C)"))) +
  xlab(bquote(bold("Days in July"))) +
  theme_few() +
  geom_vline(xintercept = c(Prod_inicioL, Prod_finL), linetype = "dotdash", color = "red", size = 0.6) +
  geom_vline(xintercept = c(Prod_inicioM, Prod_finM), linetype = "solid", color = "blue", size = 0.6) +
  geom_vline(xintercept = c(Surveys_inicio, Surveys_fin), linetype = "longdash", color = "gray31", size = 0.6)

# DEPTH PLOT

Depth_plot <- ggplot() +
  geom_line(data = data_recortadaLaurel1, aes(x = `TimeStamp`, y = `Depth(m)`), linewidth = 0.6, color = "#A2CD5A") +
  geom_line(data = data_recortadaMagueyes1, aes(x = `TimeStamp`, y = `Depth(m)`), linewidth = 0.6, color = "#009EA6") +
  scale_x_datetime(limits = c(fecha_inicio, fecha_fin_30min), labels = scales::date_format("%d"), date_breaks = "1 day", timezone = "UTC") +
  scale_y_continuous(limits = c(1.5, 2.5)) +
  ylab(bquote(bold("Depth (m)"))) +
  xlab(bquote(bold("Days in July"))) +
  theme_few() +
  geom_vline(xintercept = c(Prod_inicioL, Prod_finL), linetype = "dotdash", color = "red", size = 0.6) +
  geom_vline(xintercept = c(Prod_inicioM, Prod_finM), linetype = "solid", color = "blue", size = 0.6) +
  geom_vline(xintercept = c(Surveys_inicio, Surveys_fin), linetype = "longdash", color = "gray31", size = 0.6)

# DO PLOT

DO_plot <- ggplot() +
  geom_line(data = data_recortadaLaurel_DO, aes(x = `Bolivia Time`, y = `Dissolved Oxygen`), linewidth = 0.6, color = "#A2CD5A") +
  geom_line(data = data_recortadaMagueyes_DO, aes(x = `Bolivia Time`, y = `Dissolved Oxygen`), linewidth = 0.6, color = "#009EA6") +
  scale_x_datetime(limits = c(fecha_inicio, fecha_fin_30min), labels = scales::date_format("%d"), date_breaks = "1 day", timezone = "UTC") +
  ylab(bquote(bold("DO (mg" ~ L^-1 ~ ")"))) +
  xlab(bquote(bold("Days in July"))) +
  theme_few() +
  geom_vline(xintercept = c(Prod_inicioL, Prod_finL), linetype = "dotdash", color = "red", size = 0.6) +
  geom_vline(xintercept = c(Prod_inicioM, Prod_finM), linetype = "solid", color = "blue", size = 0.6) +
  geom_vline(xintercept = c(Surveys_inicio, Surveys_fin), linetype = "longdash", color = "gray31", size = 0.6)


# PAR PLOT

PAR_plot <- ggplot() +
  geom_line(data = data_recortadaLaurel_PAR, aes(x = `Bolivia Time`, y = `PAR`), linewidth = 0.6, color = "#A2CD5A") +
  geom_line(data = data_recortadaMagueyes_PAR, aes(x = `Bolivia Time`, y = `PAR`), linewidth = 0.6, color = "#009EA6") +
  scale_x_datetime(limits = c(fecha_inicio, fecha_fin_30min), labels = scales::date_format("%d"), date_breaks = "1 day", timezone = "UTC") +
  ylab(bquote(bold("PAR ("~mu~"mol m"^-2 ~ "s"^-1 ~")"))) +
  xlab(bquote(bold("Days in July"))) +
  theme_few() +
  geom_vline(xintercept = c(Prod_inicioL, Prod_finL), linetype = "dotdash", color = "red", size = 0.6) +
  geom_vline(xintercept = c(Prod_inicioM, Prod_finM), linetype = "solid", color = "blue", size = 0.6) +
  geom_vline(xintercept = c(Surveys_inicio, Surveys_fin), linetype = "longdash", color = "gray31", size = 0.6)


# CURRENT SPEED PLOT  

Current_plot <- ggplot() +
  geom_line(data = data_recortadaLaurel_current, aes(x = `Time`, y = `Speed (cm/s)`), linewidth = 0.6, color = "#A2CD5A") +
  geom_line(data = data_recortadaMagueyes_current, aes(x = `Time`, y = `Speed (cm/s)`), linewidth = 0.6, color = "#009EA6") +
  scale_x_datetime(limits = c(fecha_inicio, fecha_fin_30min), labels = scales::date_format("%d"), date_breaks = "1 day", timezone = "UTC") +
  ylab(bquote(bold("Speed (cm s"^-1 ~ ")"))) +
  xlab(bquote(bold("Days in July"))) +
  theme_few() +
  geom_vline(xintercept = c(Prod_inicioL, Prod_finL), linetype = "dotdash", color = "red", size = 0.6) +
  geom_vline(xintercept = c(Prod_inicioM, Prod_finM), linetype = "solid", color = "blue", size = 0.6) +
  geom_vline(xintercept = c(Surveys_inicio, Surveys_fin), linetype = "longdash", color = "gray31", size = 0.6)




## VERTICAL PLOTS

# Temperature
Temperature_plot <- Temperature_plot + labs(title = "a.") + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_text(size = 10))

# Depth
Depth_plot <- Depth_plot + labs(title = "b.") + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_text(size = 10))

# PAR
PAR_plot <- PAR_plot + labs(title = "c.") + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_text(size = 10))

# DO
DO_plot <- DO_plot + labs(title = "d.") + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_text(size = 10))

# Current
Current_plot <- Current_plot + labs(title = "e.") + 
  theme(axis.text = element_text(size = 8),   
        axis.title = element_text(size = 10))


combined_plot1 <- Temperature_plot / Depth_plot / PAR_plot / DO_plot / Current_plot

#Save
ggsave("combined_plot_uniform_text.png", plot = combined_plot1, dpi = 600, width = 10, height = 8)

# Different resolutions
ggsave("Seriesfinal2_high_res.png", plot = combined_plot1, 
       width = 10, height = 8, dpi = 900)  

ggsave("Seriesfinal2_high_res2.png", plot = combined_plot1, 
       width = 5.7, height = 8, dpi = 600)

ggsave("Seriesfinal2_high_res2.png", plot = combined_plot1, 
       width = 6, height = 8.6, dpi = 1000)  




# CONTOUR PLOTS 


# Data
Spatial_Surveys<- read_excel("Spatial Surveys (2).xlsx")
View(Spatial_Surveys)
Spatial_Surveys_GPS=merge(x=Spatial_Surveys,y=BIP_2023_GPS,by.x="GPS",by.y="Name")

# Interpolate data for contour plots #
register_google(key = "replace_with_your_own_google_API_key")

# Laurel

Laurel_Morning=Spatial_Surveys_GPS %>% 
  filter(Site=="Laurel") %>% 
  filter(Survey=="Morning")

Laurel_Map=
  get_map(location = c(Lon = mean(Laurel_Morning$Longitude), 
                       Lat = mean(Laurel_Morning$Latitude)),
          zoom=18,maptype="satellite")


Laurel_Afternoon=Spatial_Surveys_GPS %>% 
  filter(Site=="Laurel") %>% 
  filter(Survey=="Afternoon")

Laurel_Map=
  get_map(location = c(Lon = mean(Laurel_Afternoon$Longitude), 
                       Lat = mean(Laurel_Afternoon$Latitude)),
          zoom=18,maptype="satellite")

#Magueyes

Magueyes_Morning=Spatial_Surveys_GPS %>% 
  filter(Site=="Magueyes") %>% 
  filter(Survey=="Morning")

Magueyes_Map=
  get_map(location = c(Lon = mean(Magueyes_Morning$Longitude), 
                       Lat = mean(Magueyes_Morning$Latitude)),
          zoom=18,maptype="satellite")


Magueyes_Afternoon=Spatial_Surveys_GPS %>% 
  filter(Site=="Magueyes") %>% 
  filter(Survey=="Afternoon")

Magueyes_Map=
  get_map(location = c(Lon = mean(Magueyes_Afternoon$Longitude), 
                       Lat = mean(Magueyes_Afternoon$Latitude)),
          zoom=18,maptype="satellite")




### CONTOUR PLOTS  DIC

# Magueyes morning 


Interp_Data1 <- with(Magueyes_Morning, interp(x = Longitude, y = Latitude, z =`nDIC`,
                                              xo = seq(min(Longitude), max(Longitude), length = 500), 
                                              yo = seq(min(Latitude), max(Latitude), length = 500),     method="linear"))

ITP_Data1 <- tidy(Interp_Data1)

datosMM <- data.frame(
  Time = "MMorning",
  Speed = 0.57* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 315.15*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)



Magueyes_Morning_Plota=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data1, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 20)+
  geom_point(data = Magueyes_Morning, aes(x = Longitude, Latitude, color = `nDIC`), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosMM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), 
               color = "green", 
               size = 1)+
  scale_color_gradient(low = "#fed976", high = "#b10026", name = "DIC [µmol kg−1]",limits=c(1923.96,2088.02))+
  scale_fill_manual(values = c( 
    "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
    "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c",
    "#b10026","#b10026","#b10026","#b10026","#b10026"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none",
        plot.title = element_text(size = 18)) +
  labs(title = "(a) Magueyes morning")



# Magueyes afternoon

Interp_Data2 <- with(Magueyes_Afternoon, interp(x = Longitude, y = Latitude, z = `nDIC`,
                                                xo = seq(min(Longitude), max(Longitude), length = 500), 
                                                yo = seq(min(Latitude), max(Latitude), length = 500),
                                                linear = TRUE))

ITP_Data2 <- tidy(Interp_Data2)


datosMA <- data.frame(
  Time = "MAfternoon",
  Speed = 0.41* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 159.69*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)

Magueyes_Afternoon_Plotb=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data2, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 18.5)+
  geom_point(data = Magueyes_Afternoon, aes(x = Longitude, Latitude, color = `nDIC`), size = 3.2, alpha = 0.7)+geom_segment(data = datosMA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green",size = 1)+
  scale_color_gradient(low = "#fed976", high = "#b10026", name = "DIC [µmol kg−1]",limits=c(1923.96,2088.02))+
  scale_fill_manual(values = c( "#fed976","#fed976","#fed976","#fed976","#fed976",
                                "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
                                "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
                                "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none",
        plot.title = element_text(size = 18)) +
  labs(title = "(b) Magueyes afternoon")



# Laurel morning

Interp_Data3 <- with(Laurel_Morning, interp(x = Longitude, y = Latitude, z = `nDIC`,
                                            xo=seq(min(Longitude), max(Longitude), length = 500), 
                                            yo=seq(min(Latitude), max(Latitude), length = 500),
                                            method="linear"))

ITP_Data3 =tidy(Interp_Data3)

table(Laurel_Morning$`DIC`)

datosLM <- data.frame(
  Time = "LMorning",
  Speed = 5.54* 60 * 45 * 1 /11067972, 
  Heading = 255.70*(pi/180), 
  Longitude = -67.06192773
  , 
  Latitude = 17.94231683
  
)



Laurel_Morning_Plotac=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data3, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 9.5)+
  geom_point(data = Laurel_Morning, aes(x = Longitude, Latitude, color = `nDIC`), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosLM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), 
               color = "green", 
               size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "DIC [µmol kg−1]",limits=c(1923.96,2088.02))+
  scale_fill_manual(values = c( 
    
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
    "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none",
        plot.title = element_text(size = 18)) +
  labs(title = "(c) Laurel morning")


# Laurel afternoon

Interp_Data4 <- with(Laurel_Afternoon, interp(x = Longitude, y = Latitude, z = `nDIC`,
                                              xo=seq(min(Longitude), max(Longitude), length = 500), 
                                              yo=seq(min(Latitude), max(Latitude), length = 500),
                                              method="linear"))

ITP_Data4 =tidy(Interp_Data4)

table(Laurel_Afternoon$`DIC`)

datosLA <- data.frame(
  Time = "LAfternoon",
  Speed = 6.09* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 275.40*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.06192773
  , #check this is where the tilt current meter was
  Latitude = 17.94231683
  #check this is where the tilt current meter was
)



Laurel_Afternoon_Plotd=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data4, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 10)+
  geom_point(data = Laurel_Afternoon, aes(x = Longitude, Latitude, color = `nDIC`), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosLA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), 
               color = "green", 
               size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = expression(nDIC~(µmol~kg^-1)),limits=c(1922.96,2089))+
  scale_fill_manual(values = c( "#fed976","#fed976","#fed976","#fed976","#fed976",
                                "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
                                "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "right",
        plot.title = element_text(size = 18)) +
  labs(title = "(d) Laurel afternoon")






#### CONTOUR PLOTSDO

# Magueyes morning



Interp_Data_a <- with(Magueyes_Morning, interp(x = Longitude, y = Latitude, z = `DO (mg/L)`,
                                               xo=seq(min(Longitude), max(Longitude), length = 500), 
                                               yo=seq(min(Latitude), max(Latitude), length = 500),
                                               method="linear"))

ITP_Data_a =tidy(Interp_Data_a)

datosMM <- data.frame(
  Time = "MMorning",
  Speed = 0.57* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 315.15*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)




Magueyes_Morning_PlotDO=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data_a, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 10)+
  geom_point(data = Magueyes_Morning, aes(x = Longitude, Latitude, color = `DO (mg/L)`,), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosMM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), 
               color = "green", 
               size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "DO (mg L-1)",limits=c(5.1,8.25))+
  scale_fill_manual(values = c( "#fed976","#fed976","#fed976","#fed976","#fed976",
                                "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
                                "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
                                "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
                                "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c",
                                "#b10026","#b10026","#b10026","#b10026","#b10026"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")


#Magueyes afternoon

Interp_Data_b <- with(Magueyes_Afternoon, interp(x = Longitude, y = Latitude, z = `DO (mg/L)`,
                                                 xo=seq(min(Longitude), max(Longitude), length = 500), 
                                                 yo=seq(min(Latitude), max(Latitude), length = 500),
                                                 method="linear"))

ITP_Data_b =tidy(Interp_Data_b)

table(Magueyes_Afternoon$`DO (mg/L)`)

datosMA <- data.frame(
  Time = "MAfternoon",
  Speed = 0.41* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 159.69*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)



Magueyes_Afternoon_PlotDO=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data_b, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 20.5)+
  geom_point(data = Magueyes_Afternoon, aes(x = Longitude, Latitude, color = `DO (mg/L)`,), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosMA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), 
               color = "green", 
               size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "DO (mg L-1)",limits=c(5.1,8.25))+
  scale_fill_manual(values = c( 
    "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
    "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c",
    "#b10026","#b10026","#b10026","#b10026","#b10026"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")



# Laurel morning

Interp_Data_c <- with(Laurel_Morning, interp(x = Longitude, y = Latitude, z = `DO (mg/L)`,
                                             xo=seq(min(Longitude), max(Longitude), length = 500), 
                                             yo=seq(min(Latitude), max(Latitude), length = 500),
                                             method="linear"))

ITP_Data_c =tidy(Interp_Data_c)
table(Laurel_Morning$`DO (mg/L)`)

datosLM <- data.frame(
  Time = "LMorning",
  Speed = 5.54* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 255.70*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.06192773
  , #check this is where the tilt current meter was
  Latitude = 17.94231683
  #check this is where the tilt current meter was
)


Laurel_Morning_PlotDO=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data_c, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 14)+
  geom_point(data = Laurel_Morning, aes(x = Longitude, Latitude, color = `DO (mg/L)`), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosLM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), 
               color = "green", 
               size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "DO (mg L-1)",limits=c(5.1,8.25))+
  scale_fill_manual(values = c( "#fed976","#fed976","#fed976","#fed976","#fed976",
                                "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
                                "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")



# Laurel afternoon

Interp_Data_d <- with(Laurel_Afternoon, interp(x = Longitude, y = Latitude, z = `DO (mg/L)`,
                                               xo=seq(min(Longitude), max(Longitude), length = 500), 
                                               yo=seq(min(Latitude), max(Latitude), length = 500),
                                               method="linear"))

datosLA <- data.frame(
  Time = "LAfternoon",
  Speed = 6.09* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 275.40*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.06192773
  , #check this is where the tilt current meter was
  Latitude = 17.94231683
  #check this is where the tilt current meter was
)


ITP_Data_d =tidy(Interp_Data_d)

table(Laurel_Afternoon$`DO (mg/L)`)


Laurel_Afternoon_PlotDO=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data_d, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 10)+
  geom_point(data = Laurel_Afternoon, aes(x = Longitude, Latitude, color = `DO (mg/L)`), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosLA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), 
               color = "green", 
               size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = expression(DO~(mg~L^-1)),limits=c(5.1,8.25))+
  scale_fill_manual(values = c( 
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
    "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c",
    "#b10026","#b10026","#b10026","#b10026","#b10026"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "right")




####### CONTOUR PLOTS TA

# Magueyes morning


Interp_Data_f <- with(Magueyes_Morning, interp(x = Longitude, y = Latitude, z = `nTA`,
                                               xo=seq(min(Longitude), max(Longitude), length = 500), 
                                               yo=seq(min(Latitude), max(Latitude), length = 500),
                                               method="linear"))

ITP_Data_f =tidy(Interp_Data_f)

table(Magueyes_Morning$`TA`)

Magueyes_AfternoonTA_Plot=
  ggmap(Magueyes_Map)+
  geom_point(data=Magueyes_Afternoon,
             aes(x=Longitude,y=Latitude,color=`TA`,),size=4)+
  scale_color_viridis(option="plasma")+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("(d) Magueyes Afternoon Survey")

datosMM <- data.frame(
  Time = "MMorning",
  Speed = 0.57* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 315.15*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)




Magueyes_Morning_PlotTA=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data_f, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 15)+
  geom_point(data = Magueyes_Morning, aes(x = Longitude, Latitude, color = `nTA`,), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosMM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "TA (uM) ",limits=c(2277,2345))+
  scale_fill_manual(values = c( "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
                                "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
                                "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")




# Magueyes afternoon


Interp_Data_g <- with(Magueyes_Afternoon, interp(x = Longitude, y = Latitude, z = `nTA`,
                                                 xo=seq(min(Longitude), max(Longitude), length = 500), 
                                                 yo=seq(min(Latitude), max(Latitude), length = 500),
                                                 method="linear"))

ITP_Data_g =tidy(Interp_Data_g)

table(Magueyes_Afternoon$`TA`)

##PARA LA FLECHA

datosMA <- data.frame(
  Time = "MAfternoon",
  Speed = 0.41* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 159.69*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)



Magueyes_Afternoon_PlotTA=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data_g, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 15)+
  geom_point(data = Magueyes_Afternoon, aes(x = Longitude, Latitude, color = `nTA`,), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosMA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "TA (uM) ",limits=c(2277,2348))+
  scale_fill_manual(values = c( 
    "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
    "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")



# Laurel morning


Interp_Data_h <- with(Laurel_Morning, interp(x = Longitude, y = Latitude, z = `nTA`,
                                             xo=seq(min(Longitude), max(Longitude), length = 500), 
                                             yo=seq(min(Latitude), max(Latitude), length = 500),
                                             method="linear"))

ITP_Data_h =tidy(Interp_Data_h)


####PARA FLECHA
datosLM <- data.frame(
  Time = "LMorning",
  Speed = 5.54* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 255.70*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.06192773
  , #check this is where the tilt current meter was
  Latitude = 17.94231683
  #check this is where the tilt current meter was
)



Laurel_Morning_PlotTA=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data_h, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 15)+
  geom_point(data = Laurel_Morning, aes(x = Longitude, Latitude, color = `nTA`), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosLM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "TA (uM) ",limits=c(2277,2345))+
  scale_fill_manual(values = c( "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
                                
                                "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
                                "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a"
  ))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")

# Laurel afternoon

Interp_Data_i <- with(Laurel_Afternoon, interp(x = Longitude, y = Latitude, z = `nTA`,
                                               xo=seq(min(Longitude), max(Longitude), length = 500), 
                                               yo=seq(min(Latitude), max(Latitude), length = 500),
                                               method="linear"))

ITP_Data_i =tidy(Interp_Data_i)


datosLA <- data.frame(
  Time = "LAfternoon",
  Speed = 6.09* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 275.40*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.06192773
  , #check this is where the tilt current meter was
  Latitude = 17.94231683
  #check this is where the tilt current meter was
)



Laurel_Afternoon_PlotTA=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data_i, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 15)+
  geom_point(data = Laurel_Afternoon, aes(x = Longitude, Latitude, color = `nTA`), size = 3.2, alpha = 0.7)+
  geom_segment(data = datosLA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1) +
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = expression(nTA~(µmol~kg^-1)),limits=c(2277,2345))+
  scale_fill_manual(values = c(
    "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
    "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a"
  ))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "right")


#### CONTOUR PLOTS  pH
# Magueyes morning 


Interp_Data_j <- with(Magueyes_Morning, interp(x = Longitude, y = Latitude, z = `pH_T`,
                                               xo=seq(min(Longitude), max(Longitude), length = 500), 
                                               yo=seq(min(Latitude), max(Latitude), length = 500),
                                               method="linear"))

ITP_Data_j =tidy(Interp_Data_j)

datosMM <- data.frame(
  Time = "MMorning",
  Speed = 0.57* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 315.15*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)


Magueyes_Morning_Plotph=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data_j, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 12)+
  geom_point(data = Magueyes_Morning, aes(x = Longitude, Latitude, color = `pH_T`,), size = 3.8, alpha = 0.7)+
  geom_segment(data = datosMM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "TA (uM) ",limits=c(8,8.27))+
  scale_fill_manual(values = c( 
    "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
    "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a"
  ))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")





# Magueyes afternoon


Interp_Data_k <- with(Magueyes_Afternoon, interp(x = Longitude, y = Latitude, z = `pH_T`,
                                                 xo=seq(min(Longitude), max(Longitude), length = 500), 
                                                 yo=seq(min(Latitude), max(Latitude), length = 500),
                                                 method="linear"))

ITP_Data_k =tidy(Interp_Data_k)

table(Magueyes_Afternoon$`TA`)

##PARA LA FLECHA

datosMA <- data.frame(
  Time = "MAfternoon",
  Speed = 0.41* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 159.69*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.04621216
  , #check this is where the tilt current meter was
  Latitude = 17.96960326
  #check this is where the tilt current meter was
)



Magueyes_Afternoon_Plotph=
  ggmap(Magueyes_Map)+
  geom_contour_filled(data = ITP_Data_k, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 14)+
  geom_point(data = Magueyes_Afternoon, aes(x = Longitude, Latitude, color = `pH_T`,), size = 3.8, alpha = 0.7)+
  geom_segment(data = datosMA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "TA (uM) ",limits=c(8,8.27))+
  scale_fill_manual(values = c( 
    "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
    "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")



# Laurel morning


Interp_Data_l <- with(Laurel_Morning, interp(x = Longitude, y = Latitude, z = `pH_T`,
                                             xo=seq(min(Longitude), max(Longitude), length = 500), 
                                             yo=seq(min(Latitude), max(Latitude), length = 500),
                                             method="linear"))

ITP_Data_l =tidy(Interp_Data_l)
table(Laurel_Morning$`TA`)

####PARA FLECHA
datosLM <- data.frame(
  Time = "LMorning",
  Speed = 5.54* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 255.70*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.06192773
  , #check this is where the tilt current meter was
  Latitude = 17.94231683
  #check this is where the tilt current meter was
)



Laurel_Morning_Plotph=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data_l, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 12)+
  geom_point(data = Laurel_Morning, aes(x = Longitude, Latitude, color = `pH_T`), size = 3.8, alpha = 0.7)+
  geom_segment(data = datosLM, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1)+
  scale_color_gradient(low = "#ffeda0", high = "#b10026", name = "TA (uM) ",limits=c(8,8.27))+
  scale_fill_manual(values = c( 
    "#feb24c","#feb24c","#feb24c","#feb24c","#feb24c",
    "#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c","#fd8d3c",
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a"
  ))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")

# Laurel afternoon

Interp_Data_m <- with(Laurel_Afternoon, interp(x = Longitude, y = Latitude, z = `pH_T`,
                                               xo=seq(min(Longitude), max(Longitude), length = 500), 
                                               yo=seq(min(Latitude), max(Latitude), length = 500),
                                               method="linear"))

ITP_Data_m =tidy(Interp_Data_m)



datosLA <- data.frame(
  Time = "LAfternoon",
  Speed = 6.09* 60 * 45 * 1 /11067972, #added conversion factor for speed in cm/s to degrees/hour
  Heading = 275.40*(pi/180), #added conversion factor from degrees to radians to use witht he sin() and cos() functions in R
  Longitude = -67.06192773
  , #check this is where the tilt current meter was
  Latitude = 17.94231683
  #check this is where the tilt current meter was
)



Laurel_Afternoon_Plotph=
  ggmap(Laurel_Map)+
  geom_contour_filled(data = ITP_Data_m, aes(x = x, y = y, z = z), na.rm = F, alpha = 0.6, show.legend = F, bins = 10)+
  geom_point(data = Laurel_Afternoon, aes(x = Longitude, Latitude, color = `pH_T`), size = 3.5, alpha = 0.7)+
  geom_segment(data = datosLA, mapping = aes(x = Longitude, y = Latitude, xend = Longitude + Speed * sin(Heading), yend = Latitude + Speed * cos(Heading)), arrow = arrow(length = unit(0.09, "inches")), color = "green", size = 1) +
  scale_color_gradient(low = "#ffeda0", high = "#b10026",  name = expression(pH[T]),limits=c(8,8.27))+
  scale_fill_manual(values = c( 
    "#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a","#fc4e2a",
    "#e31a1c","#e31a1c","#e31a1c","#e31a1c","#e31a1c"
  ))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "right")




###RESOLUCION 


ggsave("Magueyes_Morning_Plota.png", plot = Magueyes_Morning_Plota, width = 10, height = 8, units = "in", dpi = 300)


###1

Magueyes_plots1<- Magueyes_Morning_Plota+Magueyes_Afternoon_Plotb+
  Magueyes_Morning_PlotDO+Magueyes_Afternoon_PlotDO


dpi <- 300

# Guarda el gráfico en formato PNG con la resolución especificada
ggsave("Magueyes_plots1.png", width = 12, height = 8, dpi = dpi)


###2
Laurel_plots1<-Laurel_Morning_Plotac+Laurel_Afternoon_Plotd+
  Laurel_Morning_PlotDO+Laurel_Afternoon_PlotDO

ggsave("Laurel_plots1.png", width = 12, height = 8, dpi = dpi)

###3

Magueyes_plots2<-Magueyes_Morning_PlotTA+Magueyes_Afternoon_PlotTA+ 
  Magueyes_Morning_Plotph+Magueyes_Afternoon_Plotph


ggsave("Magueyes_plots2a.png", width = 12, height = 8, dpi = dpi)


Laurel_plots2<-Laurel_Morning_PlotTA+Laurel_Afternoon_PlotTA+
  Laurel_Morning_Plotph+Laurel_Afternoon_Plotph

ggsave("Laurel_plots2.png", width = 12, height = 8, dpi = dpi)



#########TA vs DIC PLOT


#LINEAR REGRESION MODEL II

Spatial_Surveys<- read_excel("Spatial Surveys (2).xlsx", 
                             n_max = 34)

# Data filtered
laurel_data <- Spatial_Surveys[Spatial_Surveys$Site == "Laurel", ]
magueyes_data <- Spatial_Surveys[Spatial_Surveys$Site == "Magueyes", ]

# Lineal regression II
model_laurel <- lmodel2(nTA ~ nDIC, data = laurel_data)
model_magueyes <- lmodel2(nTA ~ nDIC, data = magueyes_data)

# Slope/intercept/slope.std.error
slope_laurel <- as.numeric(model_laurel$regression.results[2, "Slope"])
intercept_laurel <- as.numeric(model_laurel$regression.results[2, "Intercept"])
se_slope_laurel <- as.numeric(model_laurel$regression.results[2, "Slope.std.error"])
se_intercept_laurel <- as.numeric(model_laurel$regression.results[2, "Intercept.std.error"])

slope_magueyes <- as.numeric(model_magueyes$regression.results[2, "Slope"])
intercept_magueyes <- as.numeric(model_magueyes$regression.results[2, "Intercept"])
se_slope_magueyes <- as.numeric(model_magueyes$regression.results[2, "Slope.std.error"])
se_intercept_magueyes <- as.numeric(model_magueyes$regression.results[2, "Intercept.std.error"])


# PLOT DIC vs TA
slope_plot <- ggplot(Spatial_Surveys, aes(x = nDIC, y = nTA)) +
  geom_point(aes(shape = factor(Survey), color = factor(Site)), size = 4) + 
  scale_shape_manual(values = c(16, 17)) + 
  scale_color_manual(values = c("Laurel" = "#A2CD5A", "Magueyes" = "#00AFBB")) + 
  geom_abline(intercept = intercept_laurel, slope = slope_laurel, color = "#A2CD5A") + 
  geom_abline(intercept = intercept_magueyes, slope = slope_magueyes, color = "#00AFBB") + 
  labs(x = "nDIC (µmol kg⁻¹)", y = "nTA (µmol kg⁻¹)", shape = "Survey", color = "Site") +
  theme_test(base_size = 20) +  # Incrementa el tamaño base del texto
  scale_x_continuous(breaks = seq(1900, 2100, by = 50), limits = c(1900, 2100)) + 
  scale_y_continuous(breaks = seq(2200, 2400, by = 50), limits = c(2200, 2400)) + 
  coord_fixed(ratio = (2100-1900)/(2400-2200)) +  # Asegura la proporción 1:1 basada en los rangos
  theme(
    aspect.ratio = 1,  # Mantiene la relación de aspecto cuadrada
    text = element_text(size = 20),  # Aumenta el tamaño del texto en general
    axis.title = element_text(size = 20),  # Aumenta el tamaño de los títulos de los ejes
    axis.text = element_text(size = 20),  # Aumenta el tamaño de los textos de los ejes
    legend.title = element_text(size = 14),  # Aumenta el tamaño del título de la leyenda
    legend.text = element_text(size = 14)  # Aumenta el tamaño del texto de la leyenda
  ) +
  annotate("text", x = 1925 + 20, y = 2263, 
           label = paste("y = ", round(slope_laurel, 2), "x + ", 
                         round(intercept_laurel, 2)), 
           color = "#A2CD5A", hjust = 0, size = 7) +  # Aumenta el tamaño del texto anotado
  annotate("text", x = 1925 + 20, y = 2365, 
           label = paste("y = ", round(slope_magueyes, 2), "x + ", 
                         round(intercept_magueyes, 2)), 
           color = "#00AFBB", hjust = 0, size = 7)  # Aumenta el tamaño del texto anotado

# Save
ggsave("slope_plot.png", plot = slope_plot, width = 10, height = 10, dpi = 300)




