## Run QAA From Zin et al. (2021) https://www.sciencedirect.com/science/article/pii/S0303243421001641
## Run QAA From Lee et al. (2016) - Only works (THEORETYCALLY FOR LANDSAT 8)

require(dplyr)
require(data.table)
require(tidyr)
require(sf)
require(sp)
require(ggplot2)
require(scales)
require(caret)
require(caTools)
require(e1071)
require(randomForest)
require(xgboost)
require(GeoLight)
require(caTools)
require(rgdal)
require(openxlsx)
require(ggpubr)

source('Scripts/Functions.R')
source('Scripts/MDNs e model validation/plot_scripts.R')
source('Scripts/MDNs e model validation/iops_function.R')


## Load dataset for creaint benchmark ML models

data = fread('MDN/DatasetFiltered/dataset_oli_v3.csv')[,-1]
#data = fread('MDN/train_algorithms_oli.csv')[,-1]
#data = fread('MDN/train_algorithms_.csv')[,-1]

#For TM and ETM create b1

data$B1_DEEP_BLUE = -5.631e-05  +  1.077e+00 * data$B1  -2.705e-01 *data$B2  + 1.103e-01  * data$B3 

names(data)[c(3:6, 20)] = c('B2', 'B3', 'B4', 'B5', 'B1')

#names(data)[14:18] = c('B2', 'B3', 'B4', 'B5', 'B1')
## QAA From Lee et al. (2016)

qaa_v5.pred = iops_qaav5(data[, c( 'B1', 'B2', 'B3', 'B4')])
qaa_v6.pred = iops_qaav6(data[, c( 'B1', 'B2', 'B3', 'B4')])


ABS_final = qaa_v5.pred$ABS
ABS_final[data$B4 > 0.0015 & is.na(qaa_v5.pred$ABS$B1) == FALSE,] = qaa_v6.pred$ABS[data$B4 > 0.0015 & is.na(qaa_v5.pred$ABS$B1) == FALSE, ]


BB_final = qaa_v5.pred$BB
BB_final[data$B4 > 0.0015 & is.na(qaa_v5.pred$BB$B1) == FALSE,] = qaa_v6.pred$BB[data$B4 > 0.0015 & is.na(qaa_v5.pred$ABS$B1) == FALSE, ]


require(GeoLight)

lon <- data$long
lat <- data$lat


## Solar zenith angle for noon on the first of May 2000
## at the Sydney Harbour Bridge

data$date[is.na(data$date)] = '2000-01-01'
s <- solar(as.POSIXct(paste(data$date,"12:00:00","EST")))
zenith_angle = zenith(s,lon,lat)

yin.secchi = secchi_calculation_yin(ABS = ABS_final, 
                                    bbp = BB_final, 
                                    teta_s = zenith_angle, 
                                    rrs.t = data[, c('B1', 'B2', 'B3', 'B4')])




data$predicted_yin = yin.secchi$SECCHI

write.csv(data, 'Data/Models/Yin et al 2021/yin_secchi_oli.csv')

plot(yin.secchi$SECCHI, data$secchi, xlim = c(0,50), ylim = c(0,50))
abline(0,1)### QAA FROM ZIN ET AL. 2021








