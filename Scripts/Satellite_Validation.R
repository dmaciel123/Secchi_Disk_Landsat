## Validation of esitmated Secchi Disk Depth

require(dplyr)
require(data.table)
require(ggplot2)
require(randomForest)
require(e1071)
require(scales)
require(ggpointdensity)
library(viridis)
require(lubridate)
require(ggpubr)
require(sp)
require(rgdal)
require(mapview)
require(tidyverse)
require(Metrics)
require(sf)

#My functions 

source('Scripts/Functions.R')
source('Scripts/plot_scripts.R')
source('Scripts/iops_function.R')
source('Scripts/Functions_filter.R')
source("Scripts/SateliteValidation.R")


# Load data 

tm.sat =  fread('Data/tm_acolite_maciel_v3.csv') 
etm.sat = fread('Data/etm_acolite_maciel_v3.csv')
oli.sat = fread('Data/oli_acolite_Maciel_v3.csv')


# Model validation

tm = satelite_validation_new(secchi_real = tm.sat$secchi, 
                             secchi_estimated = tm.sat$predicted, NAME = '', 
                             Provider = tm.sat$Provider,filter_days = T, 
                             DATE = tm.sat$date,site_id = tm.sat$uid,
                             blue_sat = tm.sat$blue, green_sat = tm.sat$green, red_sat = tm.sat$red, nir_sat = tm.sat$nir,
                             filter_hour = T,
                             image_id = tm.sat$Image_Name,TIME = tm.sat$time,
                             Model = 'MDN', MAX_ZSD = 20,index = tm.sat$index,
                             lat = tm.sat$lat, separador = 'TM',cloud_QA = tm.sat$V1,
                             lon = tm.sat$lon,
                             save = FALSE, sensor = 'tm', DAYS = 2, method = 'ACOLITE')


etm = satelite_validation_new(secchi_real = etm.sat$secchi, 
                              secchi_estimated = etm.sat$predicted, NAME = '', 
                              Provider = etm.sat$Provider,filter_days = T, filter_hour = T,
                              blue_sat = etm.sat$blue, 
                              green_sat = etm.sat$green, red_sat = etm.sat$red, nir_sat = etm.sat$nir,
                              site_id = etm.sat$uid,
                              DATE = etm.sat$date,cloud_QA = etm.sat$V1,
                              image_id = etm.sat$Image_name,TIME = etm.sat$time,
                              Model = 'MDN', MAX_ZSD = 20,index = etm.sat$index,
                              lat = etm.sat$lat, separador = 'ETM+',
                              lon = etm.sat$lon,
                              save = FALSE, sensor = 'etm', DAYS = 3, method = 'ACOLITE')


oli = satelite_validation_new(secchi_real = oli.sat$secchi, 
                              secchi_estimated = oli.sat$predicted,
                              NAME = '', 
                              Provider = oli.sat$Provider,
                              filter_days = T, 
                              DATE = oli.sat$date,
                              cloud_QA = oli.sat$V1,
                              blue_sat = oli.sat$blue, green_sat = oli.sat$green, red_sat = oli.sat$red, nir_sat = oli.sat$nir,
                              site_id = oli.sat$uid,
                              image_id = oli.sat$meta.3,
                              TIME = oli.sat$time,
                              Model = 'MDN',
                              MAX_ZSD = 20,
                              index = oli.sat$index,
                              lat = oli.sat$lat,
                              separador = 'OLI',
                              filter_hour = T,
                              lon = oli.sat$lon,
                              save = FALSE, 
                              sensor = 'oli', 
                              DAYS = 3, 
                              method = 'ACOLITE')

grafico = ggarrange(tm$graph, etm$graph, oli$graph, ncol = 2,nrow = 2, align = 'h')
grafico_log = ggarrange(tm$graph.log, etm$graph.log, oli$graph.log, ncol = 2)
ggsave(grafico, filename = 'Outputs/ModelApplication/ACOLITE/acolite_v3_basico.jpeg',  width = 17, height = 17, units = 'in', dpi =300)

top_row = ggarrange(tm$graph.log, etm$graph.log, ncol = 2, labels = c("", ""))

bottom_row = ggarrange(NULL, oli$graph.log, NULL, ncol = 3, labels = c("", "", ""), widths = c(1,2,1))
final_plot = ggarrange(top_row, bottom_row, ncol = 1) + bgcolor("white") 

final_plot

ggsave(final_plot, filename = 'Outputs/ModelApplication/ACOLITE/acolite_v3.jpeg',  width = 17, height = 17, units = 'in', dpi =300)

