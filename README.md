# TBD_MAUP

## This is a repository that provides all necessary code to recreate the analysis in a publication currently submitted to the Journal of Medical Entomology using publically available data. Specifically:

- Tick sampling data is aggregated to the county level, thus, kriging interpolations use county centroids.
- Case data uses counts of anaplasmosis aggregated to the county level, which are then randomly placed within the county.

## The data used in the published manuscript is protected data and may be made available upon reasonable request.

### Other notes:

#### Scripts are numbered according to what must be run prior to running the simulation script (Script 8).

#### Certain analyses do not require all scripts before them to be run.

#### Script 5, which loads the Gridded Population of the World (GPW) dataset, requires a manual download and USGS login, thus, sourcing Script 5 will not work without first changing the file path to the dataset.

#### All Figures are recreatable using the code within the "Figure_creation" folder. Code in this folder outputs figures to the "Figures" folder.
