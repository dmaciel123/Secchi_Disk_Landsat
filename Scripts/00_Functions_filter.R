### This script will compare Rrs values in situ with LaSRC or LEDAPS algorithsm

require(data.table)
require(dplyr)
require(ggplot2)

secchi_filter = function(dataset, difference_days) {

  require(data.table)
  require(dplyr)
  require(ggplot2)
  
  #Corrige as colunas
  dataset[dataset$orgnztn != 'LabISA-INPE', 'station_id'] = dataset[dataset$orgnztn != 'LabISA-INPE', 'id']

  #NDWI calculation
  dataset$NDWI = (dataset$green-dataset$swir22)/(dataset$green+dataset$swir22)
  
  #Remove NDWI > 0
  dataset = filter(dataset, NDWI > 0)
  

  #Remove negative data
  dataset = filter(dataset, blue > 0)
  dataset = filter(dataset, green > 0)
  dataset = filter(dataset, red > 0)
  dataset = filter(dataset, nir > 0)
  
  
  dataset$sensor = 'NA' 
  
  dataset[grepl("LT05",dataset$Image_Name), 'sensor'] = 'TM' 
  dataset[grepl("LE07",dataset$Image_Name), 'sensor'] = 'ETM' 
  dataset[grepl("LC08",dataset$Image_Name), 'sensor'] = 'OLI' 
  dataset[grepl("LC09",dataset$Image_Name), 'sensor'] = 'OLI2'
  
  #satelite = filter(satelite, blue < 0.1)
  
  dataset$dif_days = 999

  for(i in 1:nrow(dataset)) {
    
    data = dataset$Image_Name[i] %>% strsplit('_')
    data = data[[1]][4]
    data = as.Date(x = data, format = "%Y%m%d")
    
    dataset$dif_days[i] = abs(data-as.Date(dataset$date[i]))
    print(i) 
  }
  
  satelite.filter = filter(dataset, dif_days < difference_days)
  
  satelite.filter = filter(satelite.filter, QA == 5504 | QA == 21952)
  

  
    
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'AMZ', replacement = 'Brazil')
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'RJ', replacement = 'Brazil')
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'MG', replacement = 'Brazil')
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'SP', replacement = 'Brazil')
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'PR', replacement = 'Brazil')
    
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'United States of America [:punct:(]the)', replacement = 'US')
    satelite.filter$region = gsub(satelite.filter$region, pattern = "United Kingdom of Great Britain and Northern Ireland [:punct:(]the)", replacement = 'UK')
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'Bahamas [:punct:(]the)', replacement = 'Bahamas')
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'Netherlands [:punct:(]the)', replacement = 'Netherlands')
    satelite.filter$region = gsub(satelite.filter$region, pattern = 'Korea [:punct:(]the Republic of)', replacement = 'Netherlands')

    
    return(satelite.filter)
    
}


secchi_filter_global = function(dataset, difference_days, filter_hour_cheseapeake) {
  
  require(data.table)
  require(dplyr)
  require(ggplot2)
  
  
  
  
  ## Filter Cheseapeak bay with +- 1 hour
  
  #Correcting hour
  


  #Selecting sensors
  
  dataset[grepl("LT05",dataset$Image_Name), 'sensor'] = 'TM' 
  dataset[grepl("LE07",dataset$Image_Name), 'sensor'] = 'ETM' 
  dataset[grepl("LC08",dataset$Image_Name), 'sensor'] = 'OLI' 
  dataset[grepl("LC09",dataset$Image_Name), 'sensor'] = 'OLI2'
  
  #satelite = filter(satelite, blue < 0.1)
  
  dataset$dif_days = 999
  
  NAME = data.frame(dataset$Image_name)
  #Calculate the date in text format
    res =  apply(NAME, MARGIN = 1, FUN = function(x) {
    res = strsplit(x, split = '_')
    res = res[[1]][4]
 
    }
  )
  
    #subtract the dates
  dataset$dif_days = abs(as.Date(res, format = '%Y%m%d')-as.Date(dataset$date, format = '%d/%m/%y'))

  
  if(filter_hour_cheseapeake == T) {
    
    dataset.hour = dataset
    dataset.hour$hour = hms(dataset$time)
    
    dataset.hour$time = as.numeric(hour(dataset.hour$hour))
    
    dataset1 = dataset.hour %>% filter(!(Provider == 'CHESEAPEK_BAY' & time < 10))
    dataset1 = dataset1 %>% filter(!(Provider == 'CHESEAPEK_BAY' & time > 13))
    dataset1 = dataset1 %>% filter(!(Provider == 'CHESEAPEK_BAY' & dif_days == 1))
    
    dataset1 = dataset1 %>% filter(!(Provider == 'EOTB' & time < 10))
    dataset1 = dataset1 %>% filter(!(Provider == 'EOTB' & time > 13))
    dataset2 = dataset1 %>% filter(!(Provider == 'EOTB' & dif_days == 1))
    
    
  }
  
  if(exists('dataset2') == FALSE) {  
    
    satelite.filter = filter(dataset, dif_days < difference_days)
  }
  
  if(exists('dataset2') == TRUE) {
  
    satelite.filter = filter(dataset2, dif_days < difference_days)
  
  }
  
  satelite.filter = filter(satelite.filter, cloud_QA == 5504 | cloud_QA == 21952)
  
  
  
  # 
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'AMZ', replacement = 'Brazil')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'RJ', replacement = 'Brazil')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'MG', replacement = 'Brazil')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'SP', replacement = 'Brazil')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'PR', replacement = 'Brazil')
  # 
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'United States of America [:punct:(]the)', replacement = 'US')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = "United Kingdom of Great Britain and Northern Ireland [:punct:(]the)", replacement = 'UK')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'Bahamas [:punct:(]the)', replacement = 'Bahamas')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'Netherlands [:punct:(]the)', replacement = 'Netherlands')
  # satelite.filter$region = gsub(satelite.filter$region, pattern = 'Korea [:punct:(]the Republic of)', replacement = 'Netherlands')
  # 
  
  return(satelite.filter)
  
}


secchi_filter_global_acolite = function(dataset, difference_days, filter_hour_cheseapeake) {
  
  require(data.table)
  require(dplyr)
  require(ggplot2)
  
  
  ## Filter Cheseapeak bay with +- 1 hour
  
  #Correcting hour
  
  
  
  #Selecting sensors
  #satelite = filter(satelite, blue < 0.1)
  
  dataset$dif_days = 999
  
  NAME = data.frame(dataset$Image_name)
  #Calculate the date in text format
  res =  apply(NAME, MARGIN = 1, FUN = function(x) {
    res = strsplit(x, split = '_')
    res = res[[1]][4]
    
  }
  )
  
  #subtract the dates
  dataset$dif_days = abs(as.Date(res, format = '%Y%m%d')-as.Date(dataset$date, format = '%d/%m/%y'))
  
  
  if(filter_hour_cheseapeake == T) {
    
    dataset.hour = dataset
    dataset.hour$hour = hms(dataset$time)
    
    dataset.hour$time = as.numeric(hour(dataset.hour$hour))
    
    dataset1 = dataset.hour %>% filter(!(Provider == 'CHESEAPEK_BAY' & time < 10))
    dataset1 = dataset1 %>% filter(!(Provider == 'CHESEAPEK_BAY' & time > 13))
    dataset1 = dataset1 %>% filter(!(Provider == 'CHESEAPEK_BAY' & dif_days == 1))
    
    dataset1 = dataset1 %>% filter(!(Provider == 'EOTB' & time < 10))
    dataset1 = dataset1 %>% filter(!(Provider == 'EOTB' & time > 13))
    dataset2 = dataset1 %>% filter(!(Provider == 'EOTB' & dif_days == 1))
    
    
  }
  
  if(filter_hour_cheseapeake == F) {dataset2 = dataset}
  
  satelite.filter = filter(dataset2, dif_days < difference_days)
  

  
  return(satelite.filter)
  
}

  