#### ICD10 Comorbidity table
####
#### This table maps ICD10 diagnosis codes to one or more of 39 comorbidities,
#### according to the US-AHRQ version 2021.1 definitions. Each row of the table
#### has one diagnosis code and an indicator variable for each of the comorbidities.
#### Altogether, the columns are:
####  diagnosis_title:  Title/short description of the diagnosis
####  diagnosis: Diagnosis code in dotless format
####  39 comorbidity columns: One for each of the comorbidities in the comorb_description
####            table. The value in the column is a 1 if the diagnosis indicated
####            the comorbidity is present, 0 if it does not.
####
#### Source: US Agency for Healthcare Research and Quality
####         https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp



library('dplyr')
library('readr')
library('stringr')
library('here')

icd10_comorb <- read_csv(here::here('data-raw', 'Com-ICD10CM-ReferncFile-v2021-1.csv'), skip=1,
                         col_types = 'c-iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii--')

## Filter the diagnosis columns down to just the codes
icd10_comorb$diagnosis_title <- icd10_comorb$Diagnosis
m <- str_match(icd10_comorb$Diagnosis, '^([A-Z0-9]+):')
icd10_comorb$Diagnosis <- m[,2]
icd10_comorb <- icd10_comorb[unique(c('diagnosis_title', 'Diagnosis', names(icd10_comorb)))]
names(icd10_comorb) <- tolower(names(icd10_comorb))

## Make the CBVD code consistent with the abbreviation used in the description table
ncbvd <- which(names(icd10_comorb) == 'cbvd_poa')
names(icd10_comorb)[ncbvd] <- 'cbvd'

usethis::use_data(icd10_comorb, overwrite = TRUE, internal = TRUE)
