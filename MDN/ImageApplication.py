#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 18 13:07:42 2022

@author: fnincao
"""

from osgeo import gdal 
import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.preprocessing import StandardScaler


coastal = gdal.Open('//Users/danielmaciel/Downloads/LC08_L1TP_228061_20220925_20221004_02_T1/L8_OLI_2022_09_25_13_54_38_228061_L2W_Rrs_443.tif', gdal.GA_ReadOnly)
blue = gdal.Open('//Users/danielmaciel/Downloads/LC08_L1TP_228061_20220925_20221004_02_T1/L8_OLI_2022_09_25_13_54_38_228061_L2W_Rrs_483.tif', gdal.GA_ReadOnly)
green = gdal.Open('//Users/danielmaciel/Downloads/LC08_L1TP_228061_20220925_20221004_02_T1/L8_OLI_2022_09_25_13_54_38_228061_L2W_Rrs_561.tif', gdal.GA_ReadOnly)
red = gdal.Open('//Users/danielmaciel/Downloads/LC08_L1TP_228061_20220925_20221004_02_T1/L8_OLI_2022_09_25_13_54_38_228061_L2W_Rrs_655.tif', gdal.GA_ReadOnly)


#%%

coastal_array = coastal.GetRasterBand(1).ReadAsArray()
blue_array = blue.GetRasterBand(1).ReadAsArray()
green_array = green.GetRasterBand(1).ReadAsArray()
red_array = red.GetRasterBand(1).ReadAsArray()
green_blue = green_array/blue_array
green_red = green_array/red_array






#%%




#stack array
array_rf = np.stack((blue_array, green_array, red_array, green_blue, green_red))

import os

os.chdir('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi')


from MDN import image_estimates, get_sensor_bands
import numpy as np
import pandas as pd


sensor = "OLI-NoCoastal"
kwargs={'product':'secchi-Maciel',
        'use_retio': True}


a = np.reshape(array_rf, (, 4500783, 3))

res, idxs  = image_estimates(a, sensor=sensor,**kwargs)



img_prediction = res.reshape((1393, 3231))


#%%

gt = green.GetGeoTransform()

proj = green.GetProjection()

driver = gdal.GetDriverByName("Gtiff")

driver.Register()

outds = driver.Create('img_classificada_l8_log2.tif', xsize = img_prediction.shape[1], 
                      ysize=img_prediction.shape[0], bands=1, eType = gdal.GDT_Float32)

outds.SetGeoTransform(gt)

outds.SetProjection(proj)

outband = outds.GetRasterBand(1)

outband.WriteArray(img_prediction)

outband.SetNoDataValue(np.nan)

outband.FlushCache()

outband = None

outds = None

green_band = None

red_band = None

nir_band = None

orange_band = None

mascara_band = None














































   
   
   
   
   
   
   
   
   
   
   
   
   
   

