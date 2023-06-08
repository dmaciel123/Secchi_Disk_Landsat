# -*- coding: utf-8 -*-
"""
@author: Daniel Maciel

This script will apply the MDN to Landsat data. It was aplpying for all data in the .CSV file but it will be filtered after that. 
"""

from MDN.parameters import get_args
from MDN.utils import split_data
from MDN import get_sensor_bands
from MDN.product_estimation import get_estimates
import pandas as pd
import matplotlib.pyplot as plt
import MDN.plot_utils
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_absolute_error, mean_squared_error, mean_absolute_percentage_error
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error
import math  
import random
import tensorflow as tf
import math  


#Data for model creation
dataset = pd.read_csv(r"/Users/danielmaciel/MDN Daniel/Results Validation - Providers global/SimulatedDataset/rrs_sim_ETM.csv", sep = ',')

#Validation dataset

validation = pd.read_csv('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi/Data/In_situ_secchi/Gathered/ETM/acolite.csv')

Glint_correction = False 


#Removing negative values
dataset = dataset[(dataset['B1']> 0)]
dataset = dataset[(dataset['B2']> 0)]
dataset = dataset[(dataset['B3']> 0)]
dataset = dataset[(dataset['B4']> 0)]
dataset = dataset[(dataset['secchi']> 0)]

dataset['ndci'] = (dataset['B4']-dataset['B3'])/(dataset['B4']+dataset['B3'])
dataset['green_blue'] = (dataset['B2']/dataset['B1'])
dataset['green_red'] = (dataset['B2']/dataset['B3'])
dataset['green_nir'] = (dataset['B2']/dataset['B4'])

#Select data to be used in training

#Before running the algorithm we need to exclude dataset that are flagged in GLORIA. 

url='https://drive.google.com/file/d/1z-kattHR0YSWFn_-yyZoQOrde1McoZ5E/view?usp=sharing'
url='https://drive.google.com/uc?id=' + url.split('/')[-2]
df = pd.read_csv(url)

#Remove flags with #Suspect and #Baseline Shift


baseline_shift = df[df.Baseline_shift == 1]
suspect = df[df.Suspect == 1]

dataset.set_index("station_id",  inplace = True)

dataset_filter1 = dataset.drop(baseline_shift.GLORIA_ID, errors='ignore')
dataset_filter1 = dataset_filter1.drop(suspect.GLORIA_ID, errors='ignore')

#Removing organization with wrong results.
dataset_filter1.set_index("organization",  inplace = True)
np.unique(dataset_filter1.index)

campaigns_remove = ['Creighton','NOAA ECSC','NSF GCE LTER','ECCC']


dataset_filter1 = dataset_filter1.drop(campaigns_remove)

#Removing campaings with wrong results
dataset = dataset_filter1
dataset_filtered = dataset


X_train_data = dataset_filtered

#Rename to match with landsat data names
X_train_data.rename(columns = { "B1":'blue', "B2":'green', "B3":'red', "B4":'nir'}, inplace = True)

#Select only secchi for train Y
y_train = dataset_filtered[['secchi']]

##### Load dataset for prediction


validation.sep = validation


# Apply glint correction?


if Glint_correction == True:
    validation.sep['blue'] = validation.sep['blue']-validation.sep['swir22']
    validation.sep['green'] = validation.sep['green']-validation.sep['swir22']
    validation.sep['red'] = validation.sep['red']-validation.sep['swir22']
    validation.sep['nir'] = validation.sep['nir']-validation.sep['swir22']



#Removing negative Rrs values

validation.sep = validation.sep[(validation.sep['blue']> 0)]
validation.sep = validation.sep[(validation.sep['green']> 0)]
validation.sep = validation.sep[(validation.sep['red']> 0)]
validation.sep = validation.sep[(validation.sep['nir']> 0)]
validation.sep = validation.sep[(validation.sep['swir22']> 0)]
validation.sep = validation.sep[(validation.sep['secchi']> 0)]


#Calculating band ratios
validation.sep['ndci'] = (validation.sep['nir']-validation.sep['red'])/(validation.sep['nir']+validation.sep['red'])
validation.sep['green_blue'] = (validation.sep['green']/validation.sep['blue'])
validation.sep['green_red'] = (validation.sep['green']/validation.sep['red'])
validation.sep['green_nir'] = (validation.sep['green']/validation.sep['nir'])



## Args of model (don't mind if we use OLI-FULL)
args = get_args( no_load=True,sensor='ETM-Daniel',product='secchi',   
                n_rounds = 10, benchmark=False, use_ratio = False)



# get_estimates trains model on x_train/y_train data and returns estimates predicted from x_test
tf.random.set_seed(1)

BANDS = ('blue', 'green', 'red')#,  'green_blue', 'green_red')#,'nir', 'ndci', 'green_nir')

print(X_train_data)
estimates, est_slice = get_estimates(args, X_train_data.loc[:,BANDS],
                                     y_train, 
                                     validation.sep.loc[:,BANDS],
                                     validation.sep[['secchi']])


#Median of estimates
estimates_MDN = np.median(estimates,  0) 

#Add preddicted values collumn
validation.sep[['predicted_oli']]  = estimates_MDN


#Results
# 
if Glint_correction == True:
    pd.DataFrame(validation.sep).to_csv("Results/ACOLITE_Secchi_Landsat7_validation.csv")

if Glint_correction == False:
    pd.DataFrame(validation.sep).to_csv("/Users/danielmaciel/MDN Daniel/Results Validation - Providers global/Results/ACOLITE_Secchi_Landsat7_validation.csv")
