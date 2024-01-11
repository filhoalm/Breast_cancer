
## Breast cancer research
# SEER - Breast cancer: invasive vs in situ
## Data source

Institute NC. Surveillance, Epidemiology, and End Results (SEER 8, and 13,) Program Populations (1975-2020). (www.seer.cancer.gov/popdata), National Cancer Institute, DCCPS, Surveillance Research Program, released May 2023. February 2022S ed.: National Cancer Institute, 2022.

#### Morphology

##### DCIS

'8201/2: Cribriform carcinoma in situ','8500/2: Intraductal carcinoma, noninfiltrating, NOS','8501/2: Comedocarcinoma, noninfiltrating','8503/2: Noninfiltrating intraductal papillary adenocarcinoma','8507/2: Intraductal micropapillary carcinoma','8523/2: Intraductal with other types of carcinoma in situ' AND {Extent of Disease.ER Status Recode Breast Cancer (1990+)} = 'Positive','Negative','Borderline/Unknown','Recode not available'

##### IDC

'8500/3: Infiltrating duct carcinoma, NOS','8523/3: Infiltrating duct mixed with other types of carcinoma' AND {Extent of Disease.ER Status Recode Breast Cancer (1990+)} = 'Positive','Negative','Borderline/Unknown','Recode not available'


### Dataset available
### Incidence

#### SEER8
SEER8 Research plus - IDC and DCIS by, age, year, ER status, from 1979 to 2020:
DCIS:https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/dataCheck/dcis.csv"
IDC: https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/dataCheck/idc.csv

#### SEER13
SEER13 Research plus - Malignant breast cancer by race and ethnicity, year, age, ER status, HER status from 1992 to 2018: https://github.com/filhoalm/Breast_cancer/blob/main/forecasting/data/breast_er_her_11072023.csv

SEER13 Research plus - Malignant breast cancer by year, age, ER status, HER status, PR status from 1992 to 2018: 
https://github.com/filhoalm/Breast_cancer/blob/main/forecasting/data/breast_er_her_pr_1182023.csv

### SEER Updates - 2022
Starting with the 1975-2019 SEER Research Data (November 2021 submission), Detroit is no longer included in any databases, and Illinois and Texas are included in the new 22-registry database (https://seer.cancer.gov/registries/terms.html).

#### SEER12 (ER and HER2) by race and ethnicity, age and year
Malignant breast cancer
https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/forecasting/data/seer12_1102024.csv

#### SEER12 (ER, HER2, and PR status) by age and year
Malignant breast cancer 
https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/forecasting/data/seer12_1102024.csv

### Survival
Files available in a .zip files: (https://github.com/filhoalm/Breast_cancer/blob/main/Survival.zip)

#### Case listing:
Malignant breast cancer in females, by age, race an ethnicity, ER status, PR, status, HER2 status, 1992-2020.
1-year censored follow-up - case_listing_1y.csv
5-year censored follow-up - case_listing_5y.csv

#### Net survival Pohar-Perme
5-year censored follow-up - net_survival_5y_er_pr_her_seer12.csv




### Results


Figure 1 shows the trends in the incidence of Infiltrating Ductal Cancer (IDC) and Ductal Carcinoma In Situ (DCIS) breast cancers relative to 1990, when ER status started to be recorded. A) depicts overall trends; B) depicts trends in younger women; and C) depicts trends in older women. The dashed line corresponds to a value of 1.

### Quarto version
https://filhoalm.github.io/Breast_cancer/SEER8/gitfiles/seer8.html

### WerR live version
https://filhoalm.github.io/Breast_cancer/test.html

