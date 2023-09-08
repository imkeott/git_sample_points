## load packages
library(readr)
library(here)
library(raster)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(tidyr)

## set wd
i_am("Desktop/Uni/praktikum/data/sample_points.R") # your path to script
wd <- here("Desktop","Uni","praktikum","data") # your wd
setwd(wd)
################################################################################
## preprocess data
################################################################################
# load data
# list of files 
csv_files <- list.files(path = "gee_points", pattern = "*.csv", full.names = TRUE)

# combine csv files from the folder into one dataframe
data <- data.frame()
for (csv_datei in csv_files) {
  daten <- read.csv(csv_datei)
  data <- rbind(data, daten)
}

# create new column point id 
data$pointID <- seq_along(data$system.index)

# seperate data from array format to dataframes. result: list of dataframes per Point
new_rows <- apply(data, MARGIN = 1, FUN = function(x){
  ## Margin = 1 ; applies over rows
  col_array <- x[2] ## array field
  col_geo   <- x[3] ## lonlat
  col_id    <- x[4] ## pointId
  # split the array into the years
  split_array <- strsplit(col_array, "],") ## split to list
  
  ## combine lists, add the corresponding pointID and lonlat
  new_rows <- lapply(split_array, function(y) {
    data.frame(array = y, pointID = rep(col_id, length(y)), LonLat = rep(col_geo, length(y)), stringsAsFactors = FALSE)
  })
})

# rbind two times to get one dataframe with all points
df_points <- do.call(rbind, new_rows)
df_points <- do.call(rbind, df_points)

# remove the brackets from the array column and the text from lonlat cloumn
df_points$array <- gsub("\\[|\\]", "", df_points$array)
df_points$LonLat <- gsub('\\{"type":"Point","coordinates":\\[|\\]\\}', '', df_points$LonLat)

# seperate array to its values 
df_points <- separate(data= df_points, col = array, into = c("TCI_detr", "TCI", "T2M", "PPT", "count", "change", "year"),sep = ",")
df_points <- separate(data= df_points, col = LonLat, into = c("Lon", "Lat"),sep = ",")

# change datatype of columns to numeric
df_points <- as.data.frame(sapply(df_points, function(x) as.numeric(as.character(x))))

# point id to factor
df_points$pointID <- as.factor(df_points$pointID)
################################################################################
## humidity according to Lang
################################################################################
## Regenfaktor = mean annual precipitation in mm / mean annual temperature in °C
df_points$regenfaktor <- df_points$PPT/df_points$T2M
df_points$humid <- ifelse(df_points$regenfaktor < 40, "arid",
                          ifelse(df_points$regenfaktor <60, "semiarid",
                                 ifelse(df_points$regenfaktor < 100, "semihumid",
                                        ifelse(df_points$regenfaktor < 160, "humid", "perhumid"))))
################################################################################
## warm and cold 
################################################################################
# everything above mean warm, everything below cold
df_points$temp_class <- ifelse(df_points$T2M < mean(df_points$T2M), "cold","warm")

################################################################################
## wet and dry 
################################################################################
# everything above mean warm, everything below cold
df_points$ppt_class <- ifelse(df_points$PPT < mean(df_points$PPT), "dry","wet")

################################################################################
## wet, dry, warm and cold
################################################################################
# everything above mean warm, everything below cold
df_points$tp_class <- ifelse(df_points$PPT < mean(df_points$PPT) & df_points$T2M < mean(df_points$T2M), "cold_dry",
                             ifelse(df_points$PPT > mean(df_points$PPT) & df_points$T2M < mean(df_points$T2M), "cold_wet",
                                    ifelse(df_points$PPT > mean(df_points$PPT) & df_points$T2M > mean(df_points$T2M), "warm_wet","warm_dry")))


################################################################################
## plots
################################################################################
df_filter_semihumid <- df_points %>% filter(humid == "semihumid")
custom_colors <- rainbow(100) 
ggplot(df_filter_semihumid, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  xlab("N Pixel")+
  ylab("TCI detrended")+
  ggtitle("semihumid")#+
#scale_color_manual(values = custom_colors) 

df_filter_humid <- df_points %>% filter(humid == "humid")
ggplot(df_filter_humid, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  xlab("N Pixel")+
  ylab("TCI detrended")+
  ggtitle("humid")

df_filter_perhumid <- df_points %>% filter(humid == "perhumid")
ggplot(df_filter_perhumid, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  xlab("N Pixel")+
  ylab("TCI detrended")+
  ggtitle("perhumid")

df_filter_arid <- df_points %>% filter(humid == "arid")
ggplot(df_filter_arid, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  xlab("N Pixel")+
  ylab("TCI detrended")+
  ggtitle("arid")

df_filter_semiarid <- df_points %>% filter(humid == "semiarid")
ggplot(df_filter_semiarid, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  xlab("N Pixel")+
  ylab("TCI detrended")+
  ggtitle("semiarid")


## plot all points
ggplot(df_points, aes(x=count, y=TCI_detr, color=tp_class)) +
  geom_point(size=0.5) +
  xlab("N Pixel")+
  ylab("TCI detrended")+
  ggtitle("all")
################################################################################
## facet wrap

## humidity
ggplot(df_points, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  facet_wrap(~humid)+
  xlab("N Pixel")+
  ylab("TCI detrended")

## temp
ggplot(df_points, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  facet_wrap(~temp_class)+
  xlab("N Pixel")+
  ylab("TCI detrended")


## wet dry 
ggplot(df_points, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  facet_wrap(~ppt_class)+
  xlab("N Pixel")+
  ylab("TCI detrended")

## warm,cold,wet,dry
ggplot(df_points, aes(x=count, y=TCI_detr, color=pointID)) +
  geom_point(size=0.5, show.legend = F) +
  facet_wrap(~tp_class)+
  xlab("N Pixel")+
  ylab("TCI detrended")
