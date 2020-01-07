
# El Nino-Southern oscillation and under-5 diarrhea in Botswana
## Alexandra Heaney, Jeffrey Shaman, Kathleen Alexander
## Nature Communication 10, 5798 (2019) doi:10.1038/s41467-019-13584-6

This repository includes code and an example dataset to reproduce the results in the publication "El Nino-Southern oscillation and under-5 diarrhea in Botswana" published in Nature Communication in 2019.

### Data Availability 
The El Nino-Southern Oscillation datasets analyzed during the current study are available in the NOAA National Climatic Data Center repository, https://www.ncdc.noaa.gov/teleconnections/enso/index.php. TRMM rainfall data analyzed during the current study are available in the NOAA TRMM data repository, https://pmm.nasa.gov/data-access/downloads/trmm. Meteorological data from Chobe District, Botswana are available from the corresponding author by reasonable request. The under-5 diarrhea data that support the findings of this study are available from the Botswana Government Ministry of Health but restrictions apply to the availability of these data, which were used under license for the current study, and so are not publicly available. Researchers may request access to this data directly from the Botswana Government Ministry of Health at http://www.moh.gov.bw/.

### Files 

#### Data_empty.xlsx 
This file includes a data set that includes all publically available data (ENSO indices) and empty columns for the diarrhea and environmental data that is not publically available.

#### Codebook.docx
This file contains descriptions of each variable in the data set. 

#### Replication_main.R
This file contains R code that was used to run all analyses and generate all figures in the analyses. The only input to this code is the data_empty.xlsx file. Unfortunately, because diarrhea and environmental data is not publically available, readers will not be able to directly reproduce figures and results. Any questions about the code can be directed to Dr. Alexandra Heaney at akheaney@berkeley.edu. 
