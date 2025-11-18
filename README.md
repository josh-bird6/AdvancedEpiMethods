# Re-Examining Associations between Four Major PFAS and Non-Communicable Disease Mortality: A U.S. Population-Based NHANES Study

This repository contains all the code necessary to reproduce the all analysis in the above-named manuscript (Huerne, Rincón and Bird, 2026)

The very first step is downloading mortality data from the CDC website for the eight survey cycles relevant to this project, 2003-04 to 2017-18 (https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/). These are not called directly to ensure this analysis will run if the URL above is altered or the mortality files are moved to a different location. ```Masterdoc.R``` specifies the survey cycles where our exposure data are drawn from.

After cloning the repository to your local machine, opening the ```.Rproj``` file and running the ```.Rmd``` files in the ```Manuscript``` directory reproduces the analysis at each stage of the project - first the statistical analysis plan (SAP), then the methods and results section, and finally the entire manuscript. 

If you wish to reproduce the analysis step by step, go into the ```Code``` subfolder and run the scripts in this order:
  - ```Functions.R``` defines the functions which are used to extract and format data presented throughout the document.
  - In ```Base_wrangling.R```, these user-defined functions are deployed for data across the eight survey cycles which are then concatenated into the base dataset.
  - Next, the survey design object is completed in ```Surveydesign.R``` script
  - Following which Table 1 (stratified by outcome) is produced and outputted to your local machine in the ```Tables.R```. After this data preparation and descriptive analyses is complete, the statistical analysis begins
  - First, all the bivarlabe analysis is conducted in ```Table2_bivariable.R```
  - Then, the multivariable analysis (complete case) is conducted in ```Table3_adjusted_COMPLETECASE.R```
  - To handle missingness, multiple imputation by chained equations (MICE) is performed. After exploring the data to confirm MAR, the missing data is imputed in ```Miceimputation.R```, and a new survey design is also created here to account for the newly-imputed data
  - The actual MICE analysis is performed in ```Table3_adjusted_MICE.R```
  - All the main results from the complete case and MICE analysis are output in ```Forestplot.R```
  - Subgroup analysis for age and sex are performed in ```Subgroup.R```
  - Miscellaneous visualisations are performed in ```Vis.R```

Contact: Joshua Bird (joshua.bird@bccsu.ubc.ca)
Following the production of all these models, they are then assessed in the ```diagnostics``` code chunk. And at last they are pulled together into readable tables in ```forestplot```. 
