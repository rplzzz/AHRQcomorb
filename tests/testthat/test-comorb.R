## patient with several unrelated comorbidities
p1 <- data.frame(id=1,
                 diagnosis=c('F10120', 'F0631', 'E441'),  # alcohol, depress, wghtloss
                 poa=1)
## same, except none are poa
p2 <- data.frame(id=2,
                 diagnosis=c('F10120', 'F0631', 'E441'), # alcohol, depress, NO wghtloss
                 poa=0)
## Test some multiple comorbidity conditions, as well as some overlapping conditions
p3 <- data.frame(id=3,
                 diagnosis=c('I132', 'K7010', 'I119', 'B189'),
                 # chf, htn_cx, renlfl_sev, alcohol, liver_mld (check duplicates are handled)
                 poa=1)
## test clinically similar conditions
p4 <- data.frame(id=4,
                 diagnosis=c('B189', 'B190', 'E0821', 'E0810', 'I10', 'I119', 'I120',
                             'N183', 'C770', 'C000', 'D0000'),
                 # liver_sev, diab_cx, htn_cx, renlfl_sev, cancer_mets
                 poa=1)
## clinically similar conditions: cancer_mets + nsitu
p5 <- data.frame(id=5,
                 diagnosis=c('C770', 'D0000'), poa=1)  # cancer_mets
## cancer_mets + cancer_solid
p6 <- data.frame(id=6,
                 diagnosis=c('C770', 'C000'), poa=1) # cancer_mets
## cancer_solid + cancer_nsitu
p7 <- data.frame(id=7,
                 diagnosis=c('C000', 'D0000'), poa=1) # cancer_solid
## only the less severe versions of similar conditions
p8 <- data.frame(id=8,
                 diagnosis=c('B189', 'E0810', 'I10', 'N183', 'D0000'),
                 # liver_mld, diab_uncx, htn_uncx, renlfl_mod, cancer_nsitu
                 poa=1)

pall <- dplyr::bind_rows(p1,p2,p3,p4,p5,p6,p7,p8)

## empty vector for creating results
empty <- rep(0L, nrow(comorb_description))
names(empty) <- comorb_description$comorbidity

## result vectors for the test patients
r1 <- empty ; r1[c('alcohol','depress','wghtloss')] <- 1L
r2 <- empty ; r2[c('alcohol','depress')] <- 1L
r3 <- empty ; r3[c('chf','htn_cx','renlfl_sev','alcohol','liver_mld')] <- 1L
r4 <- empty ; r4[c('liver_sev','diab_cx','htn_cx','renlfl_sev','cancer_mets')] <- 1L
r5 <- empty ; r5['cancer_mets'] <- 1L
r6 <- r5
r7 <- empty ; r7['cancer_solid'] <- 1L
r8 <- empty ; r8[c('liver_mld', 'diab_uncx', 'htn_uncx', 'renlfl_mod', 'cancer_nsitu')] <- 1L

rall <- empty; rall[c('alcohol', 'depress', 'wghtloss', 'chf', 'htn_cx', 'renlfl_sev', 'liver_sev',
                      'diab_cx', 'cancer_mets')] <- 1L


test_that('Comorbidities are assigned correctly in table with id', {
  rslt <- comorb(pall, idcol='id')

  cs <- comorb_description$comorbidity

  expect_equal(simplify2array(rslt[1,cs]), r1)
  expect_equal(simplify2array(rslt[2,cs]), r2)
  expect_equal(simplify2array(rslt[3,cs]), r3)
  expect_equal(simplify2array(rslt[4,cs]), r4)
  expect_equal(simplify2array(rslt[5,cs]), r5)
  expect_equal(simplify2array(rslt[6,cs]), r6)
  expect_equal(simplify2array(rslt[7,cs]), r7)
  expect_equal(simplify2array(rslt[8,cs]), r8)

  ## Check that dotless flag gives the same result
  expect_equal(comorb(pall, idcol='id', dotless=TRUE), rslt)

  ## Check that you get the same results if you name the columns differently
  pall_rename <- pall
  names(pall_rename) <- c('clm', 'dgns', 'pa')
  rslt2 <- comorb(pall_rename, diagcol = 'dgns', poacol = 'pa', idcol='clm')
  names(rslt)[names(rslt)=='id'] <- 'clm'
  expect_equal(rslt2, rslt)
})

test_that('Comorbidities are assigned correctly in table with no id', {
  rslt <- comorb(pall, idcol=NULL)

  expect_equal(simplify2array(rslt[1,]), rall)
})
