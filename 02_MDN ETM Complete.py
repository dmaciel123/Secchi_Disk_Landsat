# -*- coding: utf-8 -*-
"""
@author: Daniel Maciel

This script will apply the MDN to Landsat data. It was aplpying for all data in the .CSV file but it will be filtered after that. 
"""

from MDN.parameters import get_args
from MDN.product_estimation import get_estimates
import pandas as pd
import matplotlib.pyplot as plt
import MDN.plot_utils
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error
import math  
import random
import tensorflow as tf
import math  


#Data for model creation
dataset = pd.read_csv(r"Data/Simulated Dataset/rrs_sim_ETM_v3.csv", sep = ',')

#Validation datase


BANDS = ('blue', 'green', 'red')

dataset.rename(columns = {'B1':'blue',  "B2":'green', "B3":'red', "B4":'nir'}, inplace = True)

X_train =  dataset.loc[:,BANDS]

print(X_train,  'Train Data')

#Select only secchi for train Y
y_train = dataset[['secchi']]

print(y_train, 'Secchi Train Data')

#Generate the MDN model

args = get_args(no_load=False,sensor='ETM',product='secchi-Maciel', 
                n_rounds = 10,
                benchmark=False, 
                use_ratio = True)


#Saving the MDN model
tf.random.set_seed(1)

get_estimates(args, X_train, y_train)

 



