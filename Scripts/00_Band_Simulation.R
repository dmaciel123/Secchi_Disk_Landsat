## Band simulation based on Rrs hyperspectral data ##

## Sensors simulation

require(dplyr)
require(data.table)
require(openxlsx)
require(bandSimulation)


source("Scripts/00_SAM_OWT.R")

## Loading dataset

data = fread("Data/rrs_hyper_v4.csv")



#References from Lianwei Wei et al. 2022
references = fread('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Rrs_validation/Data/OWT_allwaters_mean_standardised.csv')

references = references[is.na(references$`Pahlevan et. al. 2021`)==F, -2]
references.t = t(references[,-1])

res = OWT_pahlevan(rrs = data, reference = references.t, id = data$station_id)

# Atribuiting OWT
data$OWT = res$Class

# Band simulation

data_for_sim = dplyr::select(data, paste("Rrs_", 400:900, sep = ''))


OLI = oli_simulation(spectra = t(data_for_sim), point_name = data$station_id)
ETM = etm_simulation(spectra = t(data_for_sim), point_name = data$station_id)
TM = tm_simulation(spectra = t(data_for_sim), point_name = data$station_id)


#Back to the correct directory


OLI.t = t(OLI[,-1]) %>% data.frame()
ETM.t = t(ETM[,-1]) %>% data.frame()
TM.t = t(TM[,-1]) %>% data.frame()


names(OLI.t)  =  c('B1', 'B2', 'B3', 'B4', 'B5')
names(ETM.t)  =  c('B1', 'B2', 'B3', 'B4')
names(TM.t)  =  c('B1', 'B2', 'B3', 'B4')


#Creating lake year month dataset

data.sep = data %>% separate(date, into = c('year', 'month', 'day'))

data$local_year_month = paste(data.sep$local, 
                                    data.sep$year, 
                                    data.sep$month, sep = "_")


OLI.merge    = cbind(dplyr::select(data, -paste("Rrs_", 400:900, sep = '')), OLI.t)
ETM.merge    = cbind(dplyr::select(data, -paste("Rrs_", 400:900, sep = '')), ETM.t)
TM.merge     = cbind(dplyr::select(data, -paste("Rrs_", 400:900, sep = '')), TM.t)



write.csv(OLI.merge, "Data/Simulated dataset/rrs_sim_OLI_v4.csv")
write.csv(ETM.merge, "Data/Simulated dataset/rrs_sim_ETM_v4.csv")
write.csv(TM.merge,  "Data/Simulated dataset/rrs_sim_TM_v4.csv")

