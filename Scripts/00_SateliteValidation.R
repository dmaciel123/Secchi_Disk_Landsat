satelite_validation = function(satellite, train, NAME, PATH, BANDS, save, sensor, DAYS,
                               local_year_month_remove,org_remove, method) {
  
  #Filtering
  train = train[!(train$local_year_month %in% local_year_month_remove),]
  train = train[!(train$organization %in% org_remove),]
  
  if(sensor == 'OLI'){
  
    dados = data.frame(secchi = train$secchi, coastal = train$B1,
                     blue = train$B2, green = train$B3, red = train$B4, nir = train$B5)

  }
  
  if(sensor == 'OLI2'){
    
    dados = data.frame(secchi = train$secchi, coastal = train$B1,
                       blue = train$B2, green = train$B3, red = train$B4, nir = train$B5)

  }
  
  if(sensor == 'ETM'){
    dados = data.frame(secchi = train$secchi, 
                       blue = train$B1, green = train$B2, red = train$B3, nir = train$B4)
    
    
  }
  
  if(sensor == 'TM'){
    dados = data.frame(secchi = train$secchi, 
                       blue = train$B1, green = train$B2, red = train$B3, nir = train$B4)
    
    
  }
  
  dados$ndci = (dados$nir-dados$red)/(dados$nir+dados$red)
  dados$green_blue = (dados$green/dados$blue)
  dados$green_red = (dados$green/dados$red)
  dados$green_nir = (dados$green/dados$nir)
  
  train = select(dados,BANDS)
  
  
  
  train = do.call(data.frame,lapply(train, function(x) replace(x, is.infinite(x),NA))) %>% na.omit()
  
  
  RF = randomForest(secchi~., ntree = 62, mtry = 3, data = train, importance = T)
  SVM = svm(secchi~., kernel = 'radial', data = train, cost = 4)
  
  if(method == 'ACOLITE') {
  
  res = secchi_filter_global_acolite(dataset = satellite[,-1],  difference_days = DAYS, filter_hour_cheseapeake = T)
  
  } else{
    
    res = secchi_filter_global(dataset = satellite[,-1],  difference_days = DAYS, filter_hour_cheseapeake = T)
    
  }
  
  res$sensor = sensor
  pred = select(res,BANDS)
  
  
  res$predict_RF = predict(RF, pred)
  res$predict_SVM = predict(SVM, pred)
  
  
  MDN = select(res, c('sensor', 'secchi','predicted_oli','dif_days', 'Provider'))
  RF = select(res, c('sensor', 'secchi','predict_RF','dif_days', 'Provider')) 
  SVM = select(res, c('sensor', 'secchi','predict_SVM','dif_days', 'Provider')) 
  
  MDN$model = 'MDN'
  RF$model = 'RF'
  SVM$model = 'SVM'
  
  names(MDN)[3] = 'Predicted'
  names(RF)[3] = 'Predicted'
  names(SVM)[3] = 'Predicted'
  
  merged = rbind(MDN,
                 RF,
                 SVM) 
  
  merged$model_sensor = paste(merged$model, merged$sensor)
  merged$model_provider = paste(merged$Provider, merged$model)
  
  merged = filter(merged, sensor != 'NA')
  
  
  results.log = plots_secchi_validation_sep_log_density(estimado = merged$Predicted, 
                                                        medido = merged$secchi,
                                                        color = merged$sensor,
                                                        separador = merged$model,
                                                        METODO = NAME,
                                                        campanha = 'GLORIA + LabISA', 
                                                        size_axis = 20, 
                                                        size_txt = 5, size_title = 20)
  
  
  results = plots_secchi_validation_sep_density(estimado = merged$Predicted, 
                                                medido = merged$secchi,
                                                color = merged$model,
                                                separador = merged$model,
                                                METODO = 'Model Application - ETM+',
                                                campanha = 'GLORIA + LabISA', 
                                                size_axis = 20, 
                                                size_txt = 5, size_title = 20, max = 5)
  
  
  if(save == TRUE) {
    
    ggsave(plot = results.log, filename = paste(PATH, NAME, '_log.jpeg', sep = ''),
           width = 15, height = 10, units = 'in', dpi = 300)
    
    
    ggsave(plot = results, filename = paste(PATH, NAME, '.jpeg', sep = ''), 
           width = 15, height = 10, units = 'in', dpi = 300)
    
  }
  
  final.results = list(Secchi = merged, graph.log = results.log, graph = results)
}

satelite_validation_new = function(secchi_real,
                                   secchi_estimated,
                                   NAME, 
                                   Provider,
                                   site_id,
                                   filter_days, 
                                   blue_sat,
                                   green_sat, 
                                   red_sat, 
                                   nir_sat,
                                   DATE,
                                   Model,
                                   save, 
                                   lat, 
                                   lon,
                                   filter_hour,
                                   sensor,
                                   MAX_ZSD,
                                   DAYS, method, image_id, TIME, index, separador, cloud_QA) {
  

  
  res = data.frame(Index = index, 
                   site_id,
                   Measured = secchi_real, 
                   Predicted = secchi_estimated, 
                   Sensor = sensor,
                   blue_sat = blue_sat,
                   green_sat = green_sat, 
                   red_sat = red_sat, 
                   nir_sat = nir_sat,
                   lat = lat, 
                   lon = lon,
                   Provider = Provider, 
                   Model = Model, 
                   Image_name = image_id,
                   date = DATE, time = TIME, separador = separador, cloud_QA)
  
  
  if(filter_days == TRUE) {
    
    if(method == 'ACOLITE') {
      
      res2 = secchi_filter_global_acolite(dataset = res,  difference_days = DAYS, filter_hour_cheseapeake = filter_hour)
      
      print('Days Filtered')
    } else{
      
      res2 = secchi_filter_global(dataset = res,  difference_days = DAYS, filter_hour_cheseapeake = filter_hour)
      
    }
    
  } 
  
  if(filter_days == FALSE) { res2 = res}

  res = res2
  res$model_provider = paste(res$Provider, res$Model)
  
  
  results.log = plots_secchi_validation_sep_log_density(estimado = res$Predicted, 
                                                        medido = res$Measured,
                                                        color = res$Sensor,
                                                        separador = separador,
                                                        METODO = NAME,
                                                        campanha = 'GLORIA + LabISA', 
                                                        size_axis = 25,  MAX_ZSD,
                                                        size_txt = 10, size_title = 25)
  
  if(MAX_ZSD == FALSE) { MAX_SD = max(max(res$Measured), max(res$Predicted))}
  
  results = plots_secchi_validation_sep_density(estimado = res$Predicted, 
                                                        medido = res$Measured,
                                                        color = res$Sensor,
                                                        separador = separador,
                                                        METODO = NAME,
                                                        campanha = 'GLORIA + LabISA', 
                                                        size_axis = 25, MAX_ZSD = MAX_ZSD,
                                                        size_txt = 10, size_title = 25)
  

  
  if(save == TRUE) {
    
    ggsave(plot = results.log, filename = paste(PATH, NAME, '_log.jpeg', sep = ''),
           width = 15, height = 10, units = 'in', dpi = 300)
    
    
    ggsave(plot = results, filename = paste(PATH, NAME, '.jpeg', sep = ''), 
           width = 15, height = 10, units = 'in', dpi = 300)
    
  }
  
  final.results = list(Secchi = res, graph.log = results.log, graph = results)
}
