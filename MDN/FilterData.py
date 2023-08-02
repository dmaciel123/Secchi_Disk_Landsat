#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 11 10:56:41 2022

@author: danielmaciel
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Jun 23 09:10:06 2022

@author: damac
"""


import os

os.chdir('/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Global Secchi')


import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Dataset creation and separation into train / test (70% / 30%)
dataset = pd.read_csv(r"/Users/danielmaciel/Library/CloudStorage/OneDrive-inpe.br/Doutorado/Artigos/Rrs_interpolation/Data/Simulated dataset/rrs_sim_tm.csv", sep = ',')
pd.DataFrame(dataset).to_csv('MDN/DatasetFiltered/dataset_tm.csv')


dataset['station_id']


#Remove NA
#dataset = dataset.dropna(axis = 0)
#Remove negative 
dataset = dataset[(dataset['B1']> 0)]
dataset = dataset[(dataset['B2']> 0)]
dataset = dataset[(dataset['B3']> 0)]
#dataset = dataset[(dataset['B4']> 0)]


dataset = dataset[(dataset['secchi']> 0)]

#ETM 4397
#OLI 4827



#Removing duplicates entries (It happens in some cases for Amazon region)

dataset = dataset.drop_duplicates(subset = 'station_id', keep =  'first') #ETM 4397 OLI 4764



#Before running the algorithm we need to exclude dataset that are flagged in GLORIA. 

url='https://drive.google.com/file/d/1z-kattHR0YSWFn_-yyZoQOrde1McoZ5E/view?usp=sharing'
url='https://drive.google.com/uc?id=' + url.split('/')[-2]
df = pd.read_csv(url)

#Remove flags with #Suspect and #Baseline Shift


baseline_shift = df[df.Baseline_shift == 1]
suspect = df[df.Suspect == 1]

dataset.set_index("station_id",  inplace = True)

dataset['station_id'] = dataset.index

dataset_filter1 = dataset.drop(baseline_shift.GLORIA_ID, errors='ignore')
dataset_filter1 = dataset_filter1.drop(suspect.GLORIA_ID, errors='ignore')


GLORIA_REMOVE = [
  'GID_3380',
  'GID_3366',
  'GID_3362',
  'GID_3368',
  'GID_545',
  'GID_3369',
  'GID_5103',
  'GID_5411',
  'GID_5512',
  'GID_6700',
  'GID_4010',
  'GID_6700',
  'GID_6701',
  'GID_7139',
  'GID_7138',
  'GID_6695',
  'GID_3425',
  'GID_5149',
  'GID_7296',
  'GID_7320',
  'GID_7351',
  'GID_4018',
  'GID_5412',
  'GID_7095',
  'GID_7295',
  'GID_557',
  'GID_7121',
  'GID_7283',
  'GID_7312',
  'GID_3370',
  'GID_4016',
  'GID_3422',
  'GID_5494',
  'Geoma_09_2009_P48a',
  'Geoma_09_2009_P48b',
  'GID_5139',
  'GID_5129',
  'GID_5107',
  'GID_4643',
  'GID_5105',
  'Itaipu_03_2013_Ponto_08',
  'Tucurui_11_2021_Ponto_04',
  'GID_3463',
  'Tucurui_11_2021_Ponto_08',
  'GID_3423',
  'GID_610',
  'GID_3973']



dataset_filter1 = dataset_filter1.drop(GLORIA_REMOVE, errors='ignore')

#Filter by organization to remove wrong results

dataset_filter1.set_index("organization",  inplace = True)

dataset_filter1['organization'] = dataset_filter1.index

remove_orgs = ['Creighton', 'NOAA ECSC', 'NSF GCE LTER']

dataset_filter1 = dataset_filter1.drop(remove_orgs)

dataset_filter1


pd.DataFrame(dataset_filter1).to_csv('MDN/DatasetFiltered/filtered_TM.csv')
