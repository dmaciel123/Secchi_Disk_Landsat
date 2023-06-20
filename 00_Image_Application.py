

def image_MDN_application(image_NC, image_ref, SAVE_FOLDER, sensor, useRatio):
        
    from MDN import image_estimates, get_tile_data, get_sensor_bands
    import matplotlib.pyplot as plt
    from matplotlib.colors import LogNorm
    from osgeo import gdal 
    import numpy as np
    import pandas as pd
    import numpy as np

    sensor = sensor

    IMAGE_PATH_NC = image_NC
    IMAGE_PATH_TIF_REF = image_ref

    SAVE_FOLDER = SAVE_FOLDER+'/secchi.tif'


    bands, Rrs = get_tile_data(IMAGE_PATH_NC, sensor, allow_neg=False)

    kwargs={'product':'secchi-Maciel', 'use_ratio':useRatio}

    Secchi_Est, idxs2 = image_estimates(Rrs, sensor=sensor,**kwargs)





    print(Secchi_Est, type(Secchi_Est), Secchi_Est.shape, idxs2)

    Secchi = Secchi_Est[:,:,slice(None, None, None)]


    ## Apply and convert to tif

    img_prediction = Secchi_Est.reshape((Rrs.shape[0], Rrs.shape[1]))

    reference_tif = gdal.Open(IMAGE_PATH_TIF_REF, gdal.GA_ReadOnly)


    #Save
    gt = reference_tif.GetGeoTransform()
    proj = reference_tif.GetProjection()
    driver = gdal.GetDriverByName("Gtiff")

    driver.Register()

    outds = driver.Create(SAVE_FOLDER, xsize = img_prediction.shape[1], 
                        ysize=img_prediction.shape[0], bands=1, eType = gdal.GDT_Float32)

    outds.SetGeoTransform(gt)

    outds.SetProjection(proj)

    outband = outds.GetRasterBand(1)

    outband.WriteArray(img_prediction)

    outband.SetNoDataValue(np.nan)

    outband.FlushCache()
    outband = None

    outds = None

    reference_tif = None




