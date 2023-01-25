#### ICD10 Comorbidity table
####
#### This table maps ICD10 diagnosis codes to one or more of 39 comorbidities,
#### according to the US-AHRQ version 2023.1 definitions. Each row of the table
#### has one diagnosis code and an indicator variable for each of the comorbidities.
#### Altogether, the columns are:
####  diagnosis_title:  Title/short description of the diagnosis
####  diagnosis: Diagnosis code in dotless format
####  39 comorbidity columns: One for each of the comorbidities in the comorb_description
####            table. The value in the column is a 1 if the diagnosis indicated
####            the comorbidity is present, 0 if it does not.
####
#### Source: US Agency for Healthcare Research and Quality
####         https://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp#down



library('dplyr')
library('readr')
library('stringr')
library('here')
icd10_comorb <- read_csv(here::here('data-raw', 'dx-to-comorb-mapping-v2023-1.csv'), skip=1,
                         col_types = 'cc-iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii--')

## Rename the columns with the codes and titles and Reorder to put the title in
## the first column -- in comorb.R we use this positioning to drop the unneeded
## title column
names(icd10_comorb)[c(1,2)] <- c('diagnosis', 'diagnosis_title')
icd10_comorb <- icd10_comorb[unique(c('diagnosis_title', 'diagnosis', names(icd10_comorb)))]
names(icd10_comorb) <- tolower(names(icd10_comorb))

## Make the CBVD code consistent with the abbreviation used in the description table
ncbvd <- which(names(icd10_comorb) == 'cbvd_poa')
names(icd10_comorb)[ncbvd] <- 'cbvd'

## Check to ensure that the names and ordering of the comorbidities match up with
## the description table


usethis::use_data(icd10_comorb, overwrite = TRUE, internal = TRUE)
