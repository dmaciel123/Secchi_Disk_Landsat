#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct  6 16:36:15 2022

@author: danielmaciel
"""

import os

os.chdir('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi')


from MDN import image_estimates, get_sensor_bands
import numpy as np
import pandas as pd

#Generate Secchi estimates using MDN for OLI


Data = pd.read_csv('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi/Data/In_situ_secchi/Gathered/OLI/acolite.csv')
#Data = pd.read_csv('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/TemporalTrendsAmazon/Data/GLORIA_LAbisa Validation/Secchi_Landsat8_LARSC_FINAL_validation.csv')

apply_secchi = Data

apply_secchi = apply_secchi[(apply_secchi['blue']> 0)]
apply_secchi = apply_secchi[(apply_secchi['secchi']> 0)]


apply_secchi['green_blue'] = (apply_secchi['green']/apply_secchi['blue'])
apply_secchi['green_red'] = (apply_secchi['green']/apply_secchi['red'])


validation = apply_secchi.loc[:, ('coastal', 'blue', 'green', 'red')].to_numpy()



validation_reshape = np.reshape(validation, (1, len(validation), 4))



sensor = "OLI"
kwargs={'product':'secchi-Maciel', 'use_ratio': True}



res, idxs  = image_estimates(validation_reshape, sensor=sensor,**kwargs)

results = res.transpose(2,0,1).reshape(1,-1).transpose()
results = pd.DataFrame(results)
results.columns = ['Secchi']

results

final_df = apply_secchi.reset_index()

final_df['predicted'] = results['Secchi']


final_df.to_csv('MDN/ModelAplication/oli_results_newratio.csv')




