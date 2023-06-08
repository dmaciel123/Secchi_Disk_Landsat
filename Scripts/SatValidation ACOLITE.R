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



## Final validaiton samples








## Count sample size

tm$Secchi$Provider[tm$Secchi$site_id %like% 'Daniel_TM'] = 'AQUASAT'
etm$Secchi$Provider[etm$Secchi$site_id %like% 'Daniel_ETM'] = 'AQUASAT'
oli$Secchi$Provider[oli$Secchi$site_id %like% 'Daniel_OLI'] = 'AQUASAT'



tm$Secchi  %>% group_by(Provider) %>% summarise(N = length(Provider))
etm$Secchi %>% group_by(Provider) %>% summarise(N = length(Provider))
oli$Secchi %>% group_by(Provider) %>% summarise(N = length(Provider))


all = rbind(tm$Secchi, etm$Secchi, oli$Secchi)


require(terra)


SHP = vect(all, geom = c('lon', 'lat'), crs = 'EPSG:4326')

writeVector(SHP, filename = 'Outputs/ModelApplication/ACOLITE/acolite_matchups_v3.shp', overwrite = T)

require(plotly)

PREDICTED = tm$Secchi

TM = plots_secchi_validation_sep_log_density(estimado = PREDICTED$Predicted, 
                                             medido = PREDICTED$Measured, 
                                             separador = PREDICTED$Provider, 
                                             color = 'black', MAX_ZSD = 100,
                                             METODO = 'Sensor: TM', campanha = 'A', size_axis = 10, size_title = 10, size_txt = 3)

ggsave(TM, filename = 'Outputs/ModelApplication/ACOLITE/acolite_provider_tm_v3.jpeg', width = 15, height = 10, units = 'in', dpi =300)


PREDICTED = etm$Secchi

ETM = plots_secchi_validation_sep_log_density(estimado = PREDICTED$Predicted, medido = PREDICTED$Measured, 
                                              separador = PREDICTED$Provider, color = 'black', MAX_ZSD = 100,
                                              METODO = 'Sensor: ETM', campanha = 'A', size_axis = 10, size_title = 10, size_txt = 3)

ggsave(ETM, filename = 'Outputs/ModelApplication/ACOLITE/acolite_provider_etm_v3.jpeg', width = 15, height = 10, units = 'in', dpi =300)

PREDICTED = oli$Secchi

OLI = plots_secchi_validation_sep_log_density(estimado = PREDICTED$Predicted, medido = PREDICTED$Measured, 
                                              separador = PREDICTED$Provider, color = 'black', MAX_ZSD = 100,
                                              METODO = 'Sensor: OLI', campanha = 'A', size_axis = 10, size_title = 10, size_txt = 3)

ggsave(OLI, filename = 'Outputs/ModelApplication/ACOLITE//acolite_provider_oli_v3.jpeg', width = 15, height = 10, units = 'in', dpi =300)


mapview(st_as_sf(SHP), zcol= "Provider", 
        col.regions = c('blue', 'green', 'red', 'yellow', 'orange', 'brown', 'magenta', 'black', 'white'))



##Diferenca de locais

require(sf)

SHP$Error = abs(SHP$Measured-SHP$Predicted)/(SHP$Measured)*100

SHP50 = st_as_sf(SHP[SHP$Error < 50, ])



mapview(SHP50, zcol = 'Error')

mapview(st_as_sf(SHP[SHP$Error < 50 & SHP$Error > 0, ]), zcol = 'Error', col.regions = 'red', layer.name = "Error Less 50") +
  mapview(st_as_sf(SHP[SHP$Error < 200 & SHP$Error > 50, ]), zcol = 'Error',col.regions = 'blue', layer.name = "Error H 50 L 200%") +
  mapview(st_as_sf(SHP[SHP$Error > 200 & SHP$Error < 1000, ]), zcol = 'Error',col.regions = 'black', layer.name = "Error H 200%") 










id_plots = function(ID) { 
  
  
  MAPVIEW = subset(SHP, SHP$Index %in% ID) %>% st_as_sf() %>% mapview()
  
  plots_rrs = etm.sat %>% filter(index %in% ID) %>% select(c('index', 'blue', 'green', 'red','nir')) 
  names(plots_rrs) = c('index', 490, 560, 660, 850)
  
  plots_rrs = plots_rrs %>% melt('index')
  
  res.rrs = ggplot(plots_rrs, aes(x = as.numeric(variable), y = value, color = as.factor(index))) +
    geom_point(aes(color = as.factor(index))) +
    geom_line()
  
  
  return(list(map = MAPVIEW, spectra = res.rrs))
}

a = id_plots(ID = c(2252, 1173, 2780, 1686, 1087,1130))


a$map
## Comparing the plots



etm.sat.sel = select(etm.sat, c('secchi', 'blue', 'green', 'red','nir', 'green_blue', 'green_red')) %>% mutate(TYPE = 'SAT')
etm.situ.sel = select(etm.insitu, c('secchi', 'blue', 'green', 'red','nir', 'green_blue', 'green_red')) %>% mutate(TYPE = 'FIELD')

merged = rbind(etm.sat.sel, etm.situ.sel) %>% melt(c('secchi', 'TYPE'))
merged$band_type = paste(merged$variable, merged$TYPE)


band_names = c('blue', 'green', 'red','nir', 'green_blue', 'green_red')

res.list = list()

for(i in band_names) {
  res = merged %>% filter(variable == i) %>% ggplot(aes(x = secchi, y = value)) +
    geom_point(aes(color = TYPE),alpha = 0.5) + 
    facet_wrap(~TYPE, scale = 'fixed') +
    ggtitle(label = i) +
    theme_bw()
  
  res.list[[i]] = res
}

teste = ggarrange(res.list$blue, res.list$green, res.list$red, 
                  res.list$green_blue, res.list$green_red,ncol = 1)

ggsave(plot = teste, filename = 'relation_dataetm.jpeg',
       width = 10, height = 20, units = 'in', dpi = 200)


ggplot(etm.insitu, aes(x = secchi, y = green)) +
  geom_point(aes(color = secchi))


matplot(etm.sat$red, etm.sat$predicted)


### Tried to find some same loc



tm.sat
oli.sat


etm$Secchi$site_name_date = paste(etm$Secchi$site_id, etm$Secchi$date)
tm$Secchi$site_name_date = paste(tm$Secchi$site_id,  tm$Secchi$date)
oli$Secchi$site_name_date = paste(oli$Secchi$site_id, oli$Secchi$date)


etm_tm = merge(tm$Secchi[,-1], etm$Secchi[,-1], by = 'site_name_date')
etm_oli = merge(oli$Secchi[,-1], etm$Secchi[,-1], by = 'site_name_date')


wgsproj4 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

high_secchi.shp = SpatialPointsDataFrame(coords = etm_tm[,c('lon.x', 'lat.x')],
                                         data = etm_tm[,], proj4string = wgsproj4) 



mapview(high_secchi.shp[1,])


for(i in 1:nrow(etm_tm)) {
  
  point = etm_tm[i,]
  
  rrs.tm = point[,c('blue_sat.x', 'green_sat.x', 'red_sat.x', 'nir_sat.x')]
  rrs.etm = point[,c('blue_sat.y', 'green_sat.y', 'red_sat.y', 'nir_sat.y')]
  
  #MAX = max(c(max(rrs.tm), max(rrs.etm)))+0.02
  MAX = 0.05
  title = paste(point$Provider.y,point$site_id.x, point$date.x) %>% gsub(pattern = '/', replacement = '-')
  
  jpeg(paste('Outputs/Figures_ComparisonLandsats_ACOLITE/', title, '.jpeg', sep = ''), height = 5, width = 5, units = 'in', res = 200)
  
  matplot(y = t(rrs.tm), x = c(490, 560, 660, 860), type = 'l', ylim = c(-0.01,MAX), col = 'red', lwd = 2,
          xlab = 'Wavelength', ylab = 'Rrs')
  
  par(new=T)
  
  matplot(y = t(rrs.etm), x = c(490, 560, 660, 860), type = 'l', ylim = c(-0.01,MAX), col = 'black', lwd = 2,
          xlab = 'Wavelength', ylab = 'Rrs', main = title)
  
  legend('topleft', legend = c('TM','ETM'), lty = c(1,1), col = c('red', 'black'))
  legend('topright', legend = c(paste('Secchi TM:', round(point$Predicted.x,3)),
                                paste('Secchi ETM:', round(point$Predicted.y,3)),
                                paste("Measured:", point$Measured.x)), lty = c(1,1), col = c('red', 'black'))
  
  legend('bottomleft', legend = c(paste('TM:', point$Image_name.x), 
                                  paste('ETM+:', point$Image_name.y)))
  
  
  dev.off()
  
  print(i)
  
}

par(mfrow=c(2,3))

matplot(x = etm_tm$blue_sat.x, y = etm_tm$blue_sat.y, xlim = c(0,0.05), ylim = c(0,0.05),
        xlab = 'TM', ylab = 'ETM', pch = 20, main = 'Blue')
abline(0,1)


matplot(x = etm_tm$green_sat.x, y = etm_tm$green_sat.y, xlim = c(0,0.05), ylim = c(0,0.05),
        xlab = 'TM', ylab = 'ETM', pch = 20, main = 'Green')
abline(0,1)
matplot(x = etm_tm$red_sat.x, y = etm_tm$red_sat.y, xlim = c(0,0.05), ylim = c(0,0.05),
        xlab = 'TM', ylab = 'ETM', pch = 20, main = 'Red')
abline(0,1)
matplot(x = etm_tm$nir_sat.x, y = etm_tm$nir_sat.y, xlim = c(0,0.05), ylim = c(0,0.05),
        xlab = 'TM', ylab = 'ETM', pch = 20, main = 'NIR')
abline(0,1)


matplot(x = etm_tm$Predicted.x, y = etm_tm$Predicted.y, xlim = c(0,10), ylim = c(0,10),
        xlab = 'TM', ylab = 'ETM', pch = 20, main = 'Secchi Predicted')
abline(0,1)




### MAP GENERATION


world <- map_data("world")

dataset = oli$Secchi

dataset$Error = (dataset$Measured-dataset$Predicted)/dataset$Measured*100


ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) + geom_point(
    data = dataset,
    aes(lon, lat, color = Error),
    alpha = 0.7
  ) 


tm_metrics = NULL
etm_metrics = NULL
oli_metrics = NULL


datas = seq(from = 0.3, to = 5, by = 0.1)
data2 = seq(from = 5, to = 100, by = 4)
aa = c(datas, data2)



tm = satelite_validation_new(secchi_real = tm.sat$secchi, 
                             secchi_estimated = tm.sat$predicted, NAME = '', 
                             Provider = tm.sat$Provider,filter_days = T, 
                             DATE = tm.sat$date,site_id = tm.sat$uid,
                             blue_sat = tm.sat$blue, green_sat = tm.sat$green, red_sat = tm.sat$red, nir_sat = tm.sat$nir,
                             filter_hour = T,
                             image_id = tm.sat$Image_Name,TIME = tm.sat$time,
                             Model = 'MDN', MAX_ZSD = 100,index = tm.sat$index,
                             lat = tm.sat$lat, separador = 'TM',cloud_QA = tm.sat$V1,
                             lon = tm.sat$lon,
                             save = FALSE, sensor = 'tm', DAYS = 2, method = 'ACOLITE')


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
                              MAX_ZSD = 100,
                              index = oli.sat$index,
                              lat = oli.sat$lat,
                              separador = 'OLI',
                              filter_hour = T,
                              lon = oli.sat$lon,
                              save = FALSE, 
                              sensor = 'oli', 
                              DAYS = 2, 
                              method = 'ACOLITE')

etm = satelite_validation_new(secchi_real = etm.sat$secchi, 
                              secchi_estimated = etm.sat$predicted, NAME = '', 
                              Provider = etm.sat$Provider,filter_days = T, filter_hour = T,
                              blue_sat = etm.sat$blue, green_sat = etm.sat$green, red_sat = etm.sat$red, nir_sat = etm.sat$nir,
                              site_id = etm.sat$uid,
                              DATE = etm.sat$date,cloud_QA = etm.sat$V1,
                              image_id = etm.sat$Image_name,TIME = etm.sat$time,
                              Model = 'MDN', MAX_ZSD = 100,index = etm.sat$index,
                              lat = etm.sat$lat, separador = 'ETM+',
                              lon = etm.sat$lon,
                              save = FALSE, sensor = 'etm', DAYS = 2, method = 'ACOLITE')


datas = seq(from = 0, to = 10, by = 1)

DT = 1:length(datas)

tm_metrics.i = data.frame(e = DT, RMSLE = DT, mAE = DT, N = DT, MAPE = DT)
etm_metrics.i = data.frame(e = DT, RMSLE = DT, mAE = DT, N = DT, MAPE = DT)
oli_metrics.i = data.frame(e = DT, RMSLE = DT, mAE = DT, N = DT, MAPE = DT)



for(K in 1:length(datas)) {
  
  fim = 100#datas[K+1]
  ini = datas[K]
  
  
  
  a = tm$graph.log$data %>% filter(measured > ini & measured < fim) %>% summarise(E = 100*(10^(median(abs(Y)))-1), 
                                                                                  MAE = mean(abs(measured-est)), 
                                                                                  MAE = mean(abs(measured-est)), 
                                                                                  MAPE = Metrics::mape(actual = measured, predicted = est ),
                                                                                  RMSLE = rmsle(measured, est))
  
  tm_metrics.i$e[K] = a$E
  tm_metrics.i$RMSLE[K] = a$RMSLE
  tm_metrics.i$mAE[K] = a$MAE
  tm_metrics.i$MAPE[K] = a$MAPE
  
  tm_metrics.i$N[K] = nrow(tm$graph.log$data %>% filter(measured > ini & measured < fim))
  
  
  a = oli$graph.log$data %>% filter(measured > ini & measured < fim) %>% summarise(E = 100*(10^(median(abs(Y)))-1), 
                                                                                   MAE = mean(abs(measured-est)), 
                                                                                   MAE = mean(abs(measured-est)), 
                                                                                   MAPE = Metrics::mape(actual = measured, predicted = est ),
                                                                                   RMSLE = rmsle(actual = measured, predicted = est))
  
  oli_metrics.i$e[K] = a$E
  oli_metrics.i$RMSLE[K] = a$RMSLE
  oli_metrics.i$mAE[K] = a$MAE
  oli_metrics.i$MAPE[K] = a$MAPE
  
  oli_metrics.i$N[K] = nrow(oli$graph.log$data %>% filter(measured > ini & measured < fim))
  
  
  a  = etm$graph.log$data %>% filter(measured > ini & measured < fim) %>% summarise(E = 100*(10^(median(abs(Y)))-1), 
                                                                                    MAE = mean(abs(measured-est)), 
                                                                                    MAE = mean(abs(measured-est)), 
                                                                                    MAPE = Metrics::mape(actual = measured, predicted = est ),
                                                                                    RMSLE = rmsle(actual = measured, predicted = est))
  
  
  etm_metrics.i$e[K] = a$E
  etm_metrics.i$RMSLE[K] = a$RMSLE
  etm_metrics.i$mAE[K] = a$MAE
  etm_metrics.i$MAPE[K] = a$MAPE
  
  etm_metrics.i$N[K] = nrow(etm$graph.log$data %>% filter(measured > ini & measured < fim))
  
}


df = data.frame(secchi = c(datas), tm = tm_metrics.i, etm = etm_metrics.i, oli = oli_metrics.i)

names(df) = c("Secchi", 'e (TM)', 'RMSLE (TM)', 'mAE (TM)','N.TM' , 'MAPE (TM)','e (ETM+)', 'RMSLE (ETM+)', 
              'mAE (ETM+)', 'N.ETM+', 'MAPE (ETM+)', "e (OLI)", "RMSLE (OLI)", "mAE (OLI)", 'oli.n', 'MAPE (OLI)')

df.melt = melt(df, id = 'Secchi')

df.melt$Secchi = df.melt$Secchi+1
RMSLE = df.melt[df.melt$variable %like% 'RMSLE', ] %>% ggplot( aes(x = Secchi, y = value)) +
  geom_point(aes(color = variable)) +
  facet_wrap(~variable, scale = 'fixed', ncol = 3) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0,10)) +
  
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        #plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
        axis.title = element_text(face="bold", size = size_axis),
        axis.text.x = element_text(colour="black", size = size_axis),
        axis.text.y = element_text(colour="black", size = size_axis),
        axis.line = element_line(size=2, colour = "black"),
        strip.text = element_text(size=size_title, face='bold')) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))

mAE = df.melt[df.melt$variable %like% 'mAE', ] %>% ggplot( aes(x = Secchi, y = value)) +
  geom_point(aes(color = variable)) +
  facet_wrap(~variable, scale = 'fixed', ncol = 3) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0,10)) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        panel.background = element_blank(),
        legend.position = "none",
        #plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
        axis.title = element_text(face="bold", size = size_axis),
        axis.text.x = element_text(colour="black", size = size_axis),
        axis.text.y = element_text(colour="black", size = size_axis),
        axis.line = element_line(size=2, colour = "black"),
        strip.text = element_text(size=size_title, face='bold')) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))


E = df.melt[df.melt$variable %in% c("e (ETM+)", 'e (TM)', 'e (OLI)'), ] %>% ggplot( aes(x = Secchi, y = value)) +
  geom_point(aes(color = variable)) +
  facet_wrap(~variable, scale = 'fixed', ncol = 3) +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(limits = c(0,10)) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        #plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
        axis.title = element_text(face="bold", size = size_axis),
        axis.text.x = element_text(colour="black", size = size_axis),
        axis.text.y = element_text(colour="black", size = size_axis),
        axis.line = element_line(size=2, colour = "black"),
        strip.text = element_text(size=size_title, face='bold')) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))


MAPE = df.melt[df.melt$variable %like% 'MAPE', ] %>% ggplot( aes(x = Secchi, y = value)) +
  geom_point(aes(color = variable)) +
  facet_wrap(~variable, scale = 'fixed', ncol = 3) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(limits = c(0,10)) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        #plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
        axis.title = element_text(face="bold", size = size_axis),
        axis.text.x = element_text(colour="black", size = size_axis),
        axis.text.y = element_text(colour="black", size = size_axis),
        axis.line = element_line(size=2, colour = "black"),
        strip.text = element_text(size=size_title, face='bold')) +
  theme(plot.margin = unit(c(2,2,2,2), "lines"))

ggarrange(E, mAE, RMSLE, ncol = 1)




## 

estatisicas(real = 0.1, estimado = 0.2)


etm.train = etm.sat

etm.train$blue_green = etm.train$blue/etm.train$green

etm.train$green_red = etm.train$green/etm.train$red


samples = sample(x = 1:nrow(etm.train), size = nrow(etm.train)*0.7, replace = F)

train = etm.train[samples,]
valid = etm.train[-samples,]



RF = randomForest(secchi~blue+red+green+blue_green+green_red, ntree = 20, mtry = 2, data = train)


valid$predicted_RF = predict(RF, valid)

etm = satelite_validation_new(secchi_real = valid$secchi, 
                              secchi_estimated = valid$predicted_RF, NAME = '', 
                              Provider = valid$Provider,filter_days = T, filter_hour = T,
                              blue_sat = valid$blue, green_sat = valid$green, red_sat = valid$red,
                              nir_sat = valid$nir,
                              site_id = valid$uid,
                              DATE = valid$date,cloud_QA = valid$V1,
                              image_id = valid$Image_name,TIME = valid$time,
                              Model = 'MDN', MAX_ZSD = 100,index = valid$index,
                              lat = valid$lat, separador = 'ETM+',
                              lon = valid$lon,
                              save = FALSE, sensor = 'etm', DAYS = 2, method = 'ACOLITE')


etm$graph.log
