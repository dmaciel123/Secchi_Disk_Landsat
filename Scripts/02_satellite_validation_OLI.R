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
require(openxlsx)

#My functions 

source('Scripts/00_Functions.R')
source('Scripts/00_plot_scripts.R')
source('Scripts/00_iops_function.R')
source('Scripts/00_Functions_filter.R')
source("Scripts/00_SateliteValidation.R")


# Load data 

DATA = fread('Data/ACOLITE/oli_acolite_Maciel_v3.csv') %>% na.omit()
DATA = index_calc_bind(df = DATA, blue = DATA$blue, green = DATA$green, red = DATA$red)


qaa_RGB = read.xlsx('Data/QAA_RGB/landsat_OLI_Matchups_qaa_rgb.xlsx')

RF = readRDS('Outputs/ML_Models/random_forest_oli.R')
SVM = readRDS('Outputs/ML_Models/SVM_oli.R')
XGB = readRDS("Outputs/ML_Models/XGB_oli.R")

FILENAME = 'Outputs/Satellite_Application/comparison_OLI.jpeg'


xgb_test  = xgboost::xgb.DMatrix(data = as.matrix(DATA[,c('blue', 'green', 'red', 'green_red', 'blue_green','blue_red' ,'LH')], label = valid$secchi))


DATA$RF  = predict(RF, DATA)
DATA$QAA_RGB = qaa_RGB[qaa_RGB$uid %in% DATA$uid, 'Predicted']
DATA$XGBOOTS = predict(XGB, xgb_test)
DATA$SVM = predict(SVM, DATA)



MDN = satelite_validation_new(secchi_real = DATA$secchi, 
                              secchi_estimated = DATA$predicted,
                              NAME = '', 
                              Provider = DATA$Provider,
                              filter_days = T, 
                              DATE = DATA$date,
                              cloud_QA = DATA$V1,
                              blue_sat = DATA$blue,
                              green_sat = DATA$green, red_sat = DATA$red, nir_sat = DATA$nir,
                              site_id = DATA$uid,
                              image_id = DATA$meta.3,
                              TIME = DATA$time,
                              Model = 'MDN',
                              MAX_ZSD = 20,
                              index = DATA$index,
                              lat = DATA$lat,
                              separador = 'MDN',
                              filter_hour = T,
                              lon = DATA$lon,
                              save = FALSE, 
                              sensor = 'oli', 
                              DAYS = 3, 
                              method = 'ACOLITE')


RF = satelite_validation_new(secchi_real = DATA$secchi, 
                             secchi_estimated = DATA$RF,
                             NAME = '', 
                             Provider = DATA$Provider,
                             filter_days = T, 
                             DATE = DATA$date,
                             cloud_QA = DATA$V1,
                             blue_sat = DATA$blue, green_sat = DATA$green, red_sat = DATA$red, nir_sat = DATA$nir,
                             site_id = DATA$uid,
                             image_id = DATA$meta.3,
                             TIME = DATA$time,
                             Model = 'MDN',
                             MAX_ZSD = 20,
                             index = DATA$index,
                             lat = DATA$lat,
                             separador = 'Random Forest',
                             filter_hour = T,
                             lon = DATA$lon,
                             save = FALSE, 
                             sensor = 'oli', 
                             DAYS = 3, 
                             method = 'ACOLITE')


QAARGB = satelite_validation_new(secchi_real = DATA$secchi, 
                                 secchi_estimated = DATA$QAA_RGB,
                                 NAME = '', 
                                 Provider = DATA$Provider,
                                 filter_days = T, 
                                 DATE = DATA$date,
                                 cloud_QA = DATA$V1,
                                 blue_sat = DATA$blue, green_sat = DATA$green, red_sat = DATA$red, nir_sat = DATA$nir,
                                 site_id = DATA$uid,
                                 image_id = DATA$meta.3,
                                 TIME = DATA$time,
                                 Model = 'MDN',
                                 MAX_ZSD = 20,
                                 index = DATA$index,
                                 lat = DATA$lat,
                                 separador = 'QAA-RGB',
                                 filter_hour = T,
                                 lon = DATA$lon,
                                 save = FALSE, 
                                 sensor = 'oli', 
                                 DAYS = 3, 
                                 method = 'ACOLITE')

xgb = satelite_validation_new(secchi_real = DATA$secchi, 
                              secchi_estimated = DATA$XGBOOTS,
                              NAME = '', 
                              Provider = DATA$Provider,
                              filter_days = T, 
                              DATE = DATA$date,
                              cloud_QA = DATA$V1,
                              blue_sat = DATA$blue, green_sat = DATA$green, red_sat = DATA$red, nir_sat = DATA$nir,
                              site_id = DATA$uid,
                              image_id = DATA$meta.3,
                              TIME = DATA$time,
                              Model = 'MDN',
                              MAX_ZSD = 20,
                              index = DATA$index,
                              lat = DATA$lat,
                              separador = 'XGBoost',
                              filter_hour = T,
                              lon = DATA$lon,
                              save = FALSE, 
                              sensor = 'oli', 
                              DAYS = 3, 
                              method = 'ACOLITE')


SVM = satelite_validation_new(secchi_real = DATA$secchi, 
                              secchi_estimated = DATA$SVM,
                              NAME = '', 
                              Provider = DATA$Provider,
                              filter_days = T, 
                              DATE = DATA$date,
                              cloud_QA = DATA$V1,
                              blue_sat = DATA$blue, green_sat = DATA$green, red_sat = DATA$red, nir_sat = DATA$nir,
                              site_id = DATA$uid,
                              image_id = DATA$meta.3,
                              TIME = DATA$time,
                              Model = 'MDN',
                              MAX_ZSD = 20,
                              index = DATA$index,
                              lat = DATA$lat,
                              separador = 'SVM',
                              filter_hour = T,
                              lon = DATA$lon,
                              save = FALSE, 
                              sensor = 'oli', 
                              DAYS = 3, 
                              method = 'ACOLITE')

top_row = ggarrange(MDN$graph.log, 
                    RF$graph.log, ncol = 2, labels = c("", ""))

finnal = ggarrange(MDN$graph.log, xgb$graph.log, SVM$graph.log, QAARGB$graph.log, RF$graph.log) + bgcolor("white") 


bottom_row = ggarrange(NULL, QAARGB$graph.log, NULL, ncol = 3, labels = c("", "", ""), widths = c(1,2,1))
final_plot = ggarrange(top_row, bottom_row, ncol = 1) + bgcolor("white") 

ggsave(finnal, 
       filename = FILENAME,  width = 23, height = 17, units = 'in', dpi =300)



