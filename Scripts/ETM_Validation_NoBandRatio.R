## Comparison of other algorithms against MDN

## I can use the scripts from my model

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
require(ggpointdensity)
require(viridis)

source('Scripts/Functions.R')
source('Scripts/plot_scripts.R')
source('Scripts/iops_function.R')

# Path to MDN results .csv 

path = 'MDN/MonteCarlo/ETM/'

#Load files
files = list.files(path = path, pattern = '.csv', full.names = T)

#Create a list to store the .csv results from MDN

mdn.list = list()


for(i in 1:length(files)) {
  
  
  mdn.list[[i]] = read.delim(files[i], header=T, sep = ',')
  
  print(i)
  
}


## Load dataset for creaint benchmark ML models

data = fread('Data/rrs_etm_v3.csv')

### Loading SAA algorithms

qaa_rgb = read.xlsx('Data/QAA_RGB//landsat_etm_qaa_rgb.xlsx', sheet = 1)
qaa_lin = fread('Data/QAA_Yin//yin_secchi_etm.csv')

### 


dados = data.frame(campanha = data$local_year_month, 
                   data = data$date, 
                   local = data$local, 
                   station_id = data$station_id, 
                   local_year_month = data$local_year_month,
                   secchi = data$secchi,
                   organization = data$local,
                   blue = data$`blue (sr-1)`, green = data$`green (sr-1)`, red = data$`red (sr-1)`)

dados = cbind(dados, index_calc(blue = dados$blue, green = dados$green, red = dados$red))

## Fazer para o QAA RGB e QAA YIN
dados = merge(dados, qaa_rgb[, c('station_id', 'Predicted')], by = 'station_id')
dados = rename(dados, Predicted_QAARGB = Predicted)


## Fazer para o QAA RGB e QAA YIN
dados = merge(dados, qaa_lin[, c('station_id', 'predicted_yin')], by = 'station_id')
dados = rename(dados, Predicted_YIN = predicted_yin)




dados = do.call(data.frame,lapply(dados, function(x) replace(x, is.infinite(x),NA)))


dados = dados[duplicated(dados$station_id) == F,]

K = length(mdn.list)

RF = data.frame(1:K)
SVM = data.frame(1:K)
XGB = data.frame(1:K)
MDN = data.frame(1:K)
QAA_RGB = data.frame(1:K)
QAA_YIN = data.frame(1:K)



df.join = dados %>% na.omit()








df.join = dados[is.na(dados$blue) == FALSE, ]
df.join = dados[is.na(df.join$green) == FALSE, ]
df.join = dados[is.na(df.join$red) == FALSE, ]
df.join = dados[is.na(df.join$secchi) == FALSE, ]
df.join = dados[is.na(df.join$green_red) == FALSE, ]
df.join = dados[is.na(df.join$blue_green) == FALSE, ]
df.join = dados[is.na(df.join$blue_red) == FALSE, ]
df.join = dados[is.na(df.join$LH) == FALSE, ]





for(i in 1:K) {
  
  set.seed(i)
  samples_select = mdn.list[[i]]$local_year_month
  
  valid = mdn.list[[i]] %>% select(c('station_id','predicted', 'secchi', 'B1', 'B2', 'B3'))
  names(valid) = c('station_id','MDN_pred', 'secchi', 'blue', 'green', 'red')
  
  valid = cbind(valid, index_calc(blue = valid$blue, green = valid$green, red = valid$red))
  
  #Merge with SAA
  valid = df.join %>% select(station_id,
                             Predicted_QAARGB,
                             Predicted_YIN
  ) %>% merge(valid, by = 'station_id')
  
  valid = valid[duplicated(valid) == FALSE, ]
  
  #Creating train dataset for data without this validation set
  train = df.join[df.join$local_year_month %in% samples_select == F,] %>% na.omit()
  
  
  
  #RF Algorithm
  
  set.seed(i)
  RF.MOD =  randomForest(secchi~blue+
                           green+
                           red+green_red+blue_green+LH+blue_red,
                         data = train, ntree = 62, mtry = 2, importance = T)
  set.seed(i)
  
  #SVM algorithm
  SVM.MOD =  svm(secchi~blue+
                   green+
                   red+green_red+blue_green+LH+blue_red,
                 kernel = 'radial', data = train, cost = 4)
  
  #Xgboost algorithm
  
  
  xgb_train = xgb.DMatrix(data = as.matrix(train[,c('blue', 'green', 'red', 'green_red', 'blue_green', "blue_red", 'LH')], label = train$secchi))
  xgb_test  = xgb.DMatrix(data = as.matrix(valid[,c('blue', 'green', 'red', 'green_red', 'blue_green', "blue_red", 'LH')], label = valid$secchi))
  
  bstSparse <- xgboost(data = as.matrix(train[,c('blue', 'green', 'red', 'green_red' ,'blue_green',"blue_red",'LH')]), 
                       label = train$secchi,
                       max.depth = 10, 
                       eta = 0.3, 
                       gama = 0.3,
                       nthread = 3, 
                       min_child_weight = 1, 
                       nrounds = 20, 
                       objective = "reg:squaredlogerror", verbosa = 5)
  
  valid$XGBOOTS_SECCHI = predict(bstSparse, xgb_test)
  valid$SECCHI_RF = predict(RF.MOD, valid)
  valid$SECCHI_SVM = predict(SVM.MOD, valid[,6:12])
  
  
  RF[i,1:9] = estatisicas(real = valid$secchi, estimado = valid$SECCHI_RF)
  SVM[i,1:9] = estatisicas(real = valid$secchi, estimado = valid$SECCHI_SVM)
  XGB[i,1:9] = estatisicas(real = valid$secchi, estimado = valid$XGBOOTS_SECCHI)
  MDN[i, 1:9] = estatisicas(real = valid$secchi, estimado = valid$MDN_pred)
  QAA_RGB[i, 1:9] = estatisicas(real = valid$secchi, estimado = valid$Predicted_QAARGB)
  QAA_YIN[i, 1:9] = estatisicas(real = valid$secchi, estimado = valid$Predicted_YIN)
  
  print(i)
  
  
  
}


resmean = t(data.frame(
  RF = RF %>% na.omit() %>% apply(MARGIN = 2, FUN = mean),
  SVM = SVM %>% na.omit() %>% apply(MARGIN = 2, FUN = mean),
  XGB = XGB %>% na.omit() %>% apply(MARGIN = 2, FUN = mean),
  MDN = MDN %>% filter(MAPE < 1000)%>% filter(MAPE < 500) %>% na.omit() %>% apply(MARGIN = 2, FUN = mean),
  QAA_YIN = QAA_YIN %>% filter(MAPE < 1000) %>% na.omit() %>% apply(MARGIN = 2, FUN = mean),
  QAA_RGB = QAA_RGB %>% na.omit() %>% apply(MARGIN = 2, FUN = mean)))


ressd = t(data.frame(
  RF = RF %>% na.omit() %>% apply(MARGIN = 2, FUN = sd),
  SVM = SVM %>% na.omit() %>% apply(MARGIN = 2, FUN = sd),
  XGB = XGB %>% na.omit() %>% apply(MARGIN = 2, FUN = sd),
  MDN = MDN %>% filter(MAPE < 1000)%>% na.omit() %>% apply(MARGIN = 2, FUN = sd),
  QAA_YIN = QAA_YIN %>% filter(MAPE < 1000) %>% na.omit() %>% apply(MARGIN = 2, FUN = sd),
  QAA_RGB = QAA_RGB %>% na.omit() %>% apply(MARGIN = 2, FUN = sd)))


matrix_res = matrix(paste(round(resmean,2), "±", round(ressd,2)),
                    nrow = nrow(ressd), ncol = ncol(ressd)) 

rownames(matrix_res) = rownames(resmean)
colnames(matrix_res) = colnames(resmean)

View(matrix_res)

write.table(matrix_res, 'Outputs/MonteCarlo_Results/etm_mc_results.csv')


RF.pt = plots_secchi_validation_sep_log_density(estimado = valid$SECCHI_RF, 
                                             medido = valid$secchi, 
                                             METODO = 'Random Forest', campanha = 'GLORIA + LabISA', 
                                             size_axis = 10, 
                                             color = 'black',MAX_ZSD = 50,
                                             separador = 'a',
                                             size_txt = 5, 
                                             size_title = 10)

SVM.pt = plots_secchi_validation_sep_log_density(estimado = valid$SECCHI_SVM, 
                                              medido = valid$secchi, 
                                              METODO = 'SVM', campanha = 'GLORIA + LabISA', 
                                              size_axis = 10,MAX_ZSD = 50,
                                              color = 'black',
                                              separador = 'a',
                                              size_txt = 5, 
                                              size_title = 10)


XGB.pt = plots_secchi_validation_sep_log_density(estimado = valid$XGBOOTS_SECCHI, 
                                              medido = valid$secchi, 
                                              METODO = 'XGBoost',
                                              campanha = 'GLORIA + LabISA', 
                                              size_axis = 10, 
                                              color = 'black',MAX_ZSD = 50,
                                              separador = 'a',
                                              size_txt = 5, 
                                              size_title = 10)


MDN.pt = plots_secchi_validation_sep_log_density(estimado = valid$MDN_pred, 
                                              medido = valid$secchi, 
                                              METODO = 'MDN', campanha = 'GLORIA LabISA', 
                                              size_axis = 10, 
                                              color = 'black',MAX_ZSD = 50,
                                              separador = 'a',
                                              size_txt = 5, 
                                              size_title = 10)


QAA_RGB.pt = plots_secchi_validation_sep_log_density(estimado = valid$Predicted_QAARGB, 
                                medido = valid$secchi, 
                                METODO = 'QAA-RGB', campanha = 'GLORIA LabISA', 
                                size_axis = 10, separador = 'qaa',color = 'black',
                                size_txt = 5, 
                                size_title = 10,
                                MAX_ZSD = 15)


QAA_LIn.pt = plots_secchi_validation_sep_log_density(estimado = valid$Predicted_YIN, 
                                medido = valid$secchi, separador = "QAA-Yin",
                                color = '',MAX_ZSD = 15,
                                METODO = 'QAA Lin et al. (2021)', campanha = 'GLORIA LabISA', 
                                size_axis = 10, 
                                size_txt = 5, 
                                size_title = 10)

result = ggarrange(MDN.pt, XGB.pt, RF.pt, SVM.pt, QAA_RGB.pt,QAA_LIn.pt)

ggsave(device = 'jpeg', plot = result,filename =  'Outputs/MonteCarlo_Results/etm.jpeg', width = 20, height = 15,dpi = 300, units = 'in')

##Full models


#RF Algorithm


set.seed(i)
RF.MOD =  randomForest(secchi~blue+
                         green+
                         red+blue_green+green_red+blue_red+LH,
                       data = df.join, ntree = 62, mtry = 4, importance = T)


varImpPlot(RF.MOD, pch = 20, main = 'Random Forest - Landsat-8/etm')

saveRDS(RF.MOD, 'Outputs//ML_Models/random_forest_etm.R')

#SVM
SVM.MOD =  svm(secchi~blue+
                 green+
                 red+blue_green+green_red+blue_red+LH,
               kernel = 'radial', data = df.join, cost = 4)

saveRDS(SVM.MOD, 'Outputs//ML_Models/SVM_etm.R')


xgb_train = xgb.DMatrix(data = as.matrix(df.join[,c('blue', 'green', 'red', 'green_red', 'blue_green','blue_red', 'LH')], label = train$secchi))
xgb_test  = xgb.DMatrix(data = as.matrix(valid[,c('blue', 'green', 'red', 'green_red', 'blue_green','blue_red' ,'LH')], label = valid$secchi))


set.seed(2)
bstSparse <- xgboost(data = as.matrix(df.join[,c('blue', 'green', 'red', 'green_red', 'blue_green','blue_red', 'LH')]), 
                     label = df.join$secchi,
                     max.depth = 10, 
                     eta = 0.3, 
                     gama = 0.3,
                     nthread = 3, 
                     min_child_weight = 1, 
                     nrounds = 20, 
                     objective = "reg:squaredlogerror", verbosa = 5)

saveRDS(bstSparse, 'Outputs/ML_Models/XGB_etm.R')

