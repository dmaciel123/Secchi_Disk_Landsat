#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct  6 16:36:15 2022

@author: Daniel Andrade Maciel

"""

from MDN import image_estimates, get_sensor_bands
import numpy as np
import pandas as pd

#Generate Secchi estimates using MDN for Landsat-8/9/OLI

Data = pd.read_csv(r"Data//ACOLITE/oli_acolite_maciel_v3.csv")

print(Data)

apply_secchi = Data

GC = False

# Remove possible negative values

apply_secchi = apply_secchi[(apply_secchi['blue']> 0)]
apply_secchi = apply_secchi[(apply_secchi['green']> 0)]
apply_secchi = apply_secchi[(apply_secchi['red']> 0)]

# Creating a vector to predict
validation = apply_secchi.loc[:, ('blue', 'green', 'red')].to_numpy()
validation_reshape = np.reshape(validation, (1, len(validation), 3))


# MDN arguments

sensor = "OLI-NoCoastal"
kwargs={'product':'secchi-Maciel', 'use_ratio':True}
res, idxs  = image_estimates(validation_reshape, sensor=sensor,**kwargs)

# Change structure
results = res.transpose(2,0,1).reshape(1,-1).transpose()
results = pd.DataFrame(results)
results.columns = ['Secchi']

# Creating and exporting the final results in a new appended collumn

final_df = apply_secchi.reset_index()
final_df['predicted'] = results['Secchi']

print(final_df)
final_df.to_csv(r"Outputs/Zsd_predicted_OLI_v3.csv")
