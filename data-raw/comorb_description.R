library('dplyr')
library('readr')
library('stringr')
library('here')

#### Change Log
#### v1.0 ####
####  * Update to v2023-1 of AHRQ comorbidity definitions
####  * Drop the "CMR_" prefix that was added to comorbidity names

comorb_description <- read_csv(here('data-raw', 'comorbidity-measures-v2023-1.csv'), skip=1,
                               col_types = 'ccc')
names(comorb_description) <- c('comorbidity', 'description', 'poa')
comorb_description$poa <- comorb_description$poa == 'Yes'
comorb_description$comorbidity <- sub('CMR_', '', comorb_description$comorbidity)
comorb_description$comorbidity <- tolower(comorb_description$comorbidity)

## Add the CBVD_SQLA comorbidity, which is not included as a separate entry.
sqla <- tibble::tibble(comorbidity = 'cbvd_sqla', description = 'Sequelae of cerebrovascular diseases',
                       poa = TRUE)
comorb_description <- arrange(bind_rows(comorb_description, sqla), comorbidity)

usethis::use_data(comorb_description, overwrite = TRUE)
