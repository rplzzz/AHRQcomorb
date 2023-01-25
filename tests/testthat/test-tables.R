
test_that('Comorbidity translation table has the correct columns', {
  expect_equal(names(icd10_comorb),
               c('diagnosis_title', 'diagnosis', comorb_description$comorbidity))
})
