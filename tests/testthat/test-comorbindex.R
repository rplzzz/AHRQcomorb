#### Test the index function

## Create some test data.  We are deliberately going to put these a little
## out of order to test that they are properly rearranged

t1 <- diag(nrow(comorb_description))
colnames(t1) <-
  c('aids', 'alcohol', 'anemdef', 'autoimmune', 'bldloss', 'cancer_leuk',
    'cancer_lymph', 'cancer_mets', 'cancer_nsitu', 'cancer_solid', 'cbvd',
    'hf', 'coag', 'dementia', 'depress', 'diab_cx', 'diab_uncx', 'drug_abuse',
    'htn_cx', 'htn_uncx', 'liver_mld', 'liver_sev', 'lung_chronic',
    'neuro_movt', 'neuro_oth', 'neuro_seiz', 'obese', 'paralysis', 'perivasc',
    'psychoses', 'pulmcirc', 'renlfl_mod', 'renlfl_sev', 'thyroid_hypo',
    'thyroid_oth', 'ulcer_peptic', 'valve', 'wghtloss', 'cbvd_sqla')

## add a couple of cases that have several comorbs
t2 <- matrix(0, nrow=3, ncol=nrow(comorb_description))
colnames(t2) <- colnames(t1)
t2[1, c('cancer_mets', 'htn_cx', 'alcohol', 'liver_mld')] <- 1
t2[2, c('neuro_oth', 'depress', 'cancer_nsitu')] <- 1
t2[3, c('cancer_leuk', 'psychoses')] <- 1

testdata <- as.data.frame(rbind(t1,t2))
mort_index_vals <-
  c(-4, -1, -3, -1, -4, 9, 6, 23, 0, 10, 5, 15, 15, 5, -9, -2, 0, -7,
    1, 0, 2, 17, 2, -1, 23, 2, -7, 4, 3, -9, 4, 3, 8, -3, -8, 0, 0, 14, 0,
    23 + 1 - 1 + 2,
    23 - 9,
    9 - 9)

readm_index_vals <-
  c(5, 3, 5, 2, 2, 10, 7, 11, 0, 7, 0, 7, 3, 1, 2, 4, 0,
    6, 0, 0, 3, 10, 4, 1, 2, 5, -2, 3, 1, 6, 3, 4, 8, 0, 0, 2, 0, 6, 0,
    11 + 0 + 3 + 3,
    2 + 2 + 0,
    10 + 6)

test_that('Comorbidity index is correct', {
  cm <- comorbindex(testdata, 'mort')
  expect_equal(cm, mort_index_vals)

  cr <- comorbindex(testdata, 'readm')
  expect_equal(cr, readm_index_vals)
})
