# -*- coding: utf-8 -*-
"""
Created on Thu Jun 23 09:10:06 2022

@author: damac
"""


import os

os.chdir('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi')


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
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error
import math  
import random
import tensorflow as tf

# Dataset creation and separation into train / test (70% / 30%)
dataset = pd.read_csv(r"/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi/MDN/DatasetFiltered/dataset_etm_v3.csv", sep = ',')

dataset.set_index("local_year_month",  inplace = True)

local_unique = np.unique(dataset.index)

#Removing some datase

  
for i in range(2,50):

    lake_train, lake_test = train_test_split(local_unique, test_size = 0.3, random_state=i)


    train = dataset.loc[lake_train]
    test = dataset.drop(lake_train, errors='ignore')

    X_train = train[["B1", "B2", "B3"]]
    X_test = test[["B1",  "B2", "B3"]]

    y_train = train[['secchi']]
    y_test = test[['secchi']]


    ## MDN Training and validation 
    #get_args pulls default arguments from parameters.py unless alternative values are given

    args = get_args(no_load=True,sensor='ETM',product='secchi_maciel', 
                n_rounds = 10, benchmark=False, use_ratio = True,  n_mix = 5)




    # get_estimates trains model on x_train/y_train data and returns estimates predicted from x_test
    tf.random.set_seed(1)

    estimates, est_slice = get_estimates(args, X_train,
                                     y_train, 
                                     X_test,
                                     y_test)

    # We take the median over the number of MDN models trained (args.n_rounds = 10 by default)

    estimates_MDN = np.median(estimates,  0) 


    test['predicted']  = estimates_MDN

    a = f"/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi/MDN/MonteCarlo/ETM/MDN_run_number_{i}.csv"
    pd.DataFrame(test).to_csv(a)

    print(a)




