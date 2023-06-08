# Global Secchi Landsat
 
This respository contain all the necessary code to reproduce the findings obtained in the paper: "Global Long-term Water Transparency Products from the Landsat Archive".

The code contain Python and R scripts to apply Mixture Density Networks, Random Forest, Support Vector Regression, and XGBoost to simulated and satellite-retrieval Remote Sensing Reflectance (Rrs) data based on Landsat-family sensors. 

Python scripts applies the Mixture Density Network (MDN) algorithm to Landsat atmospherically corrected or in situ measured Remote Sensing Reflectance to predict Secchi Disk Depth (Zsd). 

To run the code, clone the GitHub repository and install the required packages (requirements.txt) in a new Python (3.6 or 3.9) environment. 

# Three examples are provided:

1) Running the MDN for TM, ETM+, and OLI simulated Rrs based on a subset of GLORIA (Lehmann et al. 2023) dataset

2) Running the MDN for OLI-2 Chesapeake Bay Rrs data to spatialize the Zsd.
 
3) RSTUDIO project with other Machine Learning methods implementation, QAA's analyzes, and some examples of figures generation.

## Instalation 

First, clone the repository using the following command 

```sh
git clone https://github.com/dmaciel123/GlobalSecchiLandsat
```

After that, cd to your MDN folder and install the requirements based on the requirements.txt file. 

``` sh
pip install -r .\requirements.txt
```

## Running the MDN algorithm for in situ data

Three Python scripts are provided to the user to run the MDN algorithm to Landsat-5/TM, Landsat-7/ETM+ and Landsat-8-9/OLI in situ simulated Remote Sensing Reflectance. To run these scripts, open the selected one (e.g., MDN Application TM.py) and change the input data to your dataset. See the examples provided for each sensor. 

## Running the MDN algorithm for Landsat satellite data.

An example is provided to run the MDN algorithm for Landsat-9/OLI data from Chesapeake Bay (Maryland, United States). Input data for this script is the .NETCDF file obtained from the ACOLITE atmospheric correction method, and the .TIF Rrs file obtained as output from ACOLITE. TIF file is used to convert the .NETCDF to a more GIS familar .TIF format. 

## Running the Random Forest, Support Vector Regression and XGBoost

An example is provided in the folder "Scripts" that demonstrates how to apply these algorithms (train and validation process) using Landsat in situ reflectance and also satellite retrieved. 

## Data availability

All the data that supports the findings of the study is available in the folder "Data". 
