library('dplyr')
library('readr')
library('stringr')
library('here')

comorb_description <- read_csv(here('data-raw', 'comorbidity-measures-v2021-1.csv'), skip=1,
                               col_types = 'ccc')
names(comorb_description) <- c('comorbidity', 'description', 'poa')
comorb_description$poa <- comorb_description$poa == 'Yes'
comorb_description$comorbidity <- tolower(comorb_description$comorbidity)

## Fix apparent typo in ANEMDEF comorbidity
nanem <- which(comorb_description$comorbidity == 'anemdf')
comorb_description$comorbidity[nanem] <- 'anemdef'

## Add the CBVD_SQLA comorbidity, which is not included as a separate entry.
sqla <- tibble::tibble(comorbidity = 'cbvd_sqla', description = 'Sequelae of cerebrovascular diseases',
                       poa = TRUE)
comorb_description <- arrange(bind_rows(comorb_description, sqla), comorbidity)

usethis::use_data(comorb_description, overwrite = TRUE)
