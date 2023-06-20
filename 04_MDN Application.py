import os

from MDN import image_estimates, get_tile_data, get_sensor_bands
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
from osgeo import gdal 
import numpy as np
import pandas as pd
import numpy as np
from Image_Application import image_MDN_application
import glob 


## We need:
## The path to the .NC file derived from ACOLITE
## The path for a reference .TIF file derived from ACOLITE
## Saving path

IMAGE_PATH_NC = 'ACOLITE/L9_OLI_2023_02_13_15_41_09_014034_L2W.nc'
IMAGE_PATH_TIF_REF = 'ACOLITE/L9_OLI_2023_02_13_15_41_09_014034_L2W_Rrs_561.tif'
SAVE_PATH = 'ACOLITE'

image_MDN_application(IMAGE_PATH_NC, IMAGE_PATH_TIF_REF, SAVE_PATH, 'OLI-NoCoastal', False)

 