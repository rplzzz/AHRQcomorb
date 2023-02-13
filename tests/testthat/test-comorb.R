## No comorbidity conditions at all
p0 <- data.frame(id=0,
                 diagnosis=c('THX1138'),   # guaranteed not to be in the table
                 poa=1
                 )
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
                 # hf, htn_cx, renlfl_sev, alcohol, liver_mld (check duplicates are handled)
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
r0 <- empty
r1 <- empty ; r1[c('alcohol','depress','wghtloss')] <- 1L
r2 <- empty ; r2[c('alcohol','depress')] <- 1L
r3 <- empty ; r3[c('hf','htn_cx','renlfl_sev','alcohol','liver_mld')] <- 1L
r4 <- empty ; r4[c('liver_sev','diab_cx','htn_cx','renlfl_sev','cancer_mets')] <- 1L
r5 <- empty ; r5['cancer_mets'] <- 1L
r6 <- r5
r7 <- empty ; r7['cancer_solid'] <- 1L
r8 <- empty ; r8[c('liver_mld', 'diab_uncx', 'htn_uncx', 'renlfl_mod', 'cancer_nsitu')] <- 1L

rall <- empty; rall[c('alcohol', 'depress', 'wghtloss', 'hf', 'htn_cx', 'renlfl_sev', 'liver_sev',
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

test_that('Cases with no comorbidities are handled correctly', {
  p <- dplyr::bind_rows(p1, p0, p3)
  rslt <- comorb(p, idcol='id')

  cs <- comorb_description$comorbidity
  expect_equal(simplify2array(rslt[1,cs]), r1)
  expect_equal(simplify2array(rslt[2,cs]), r0)
  expect_equal(simplify2array(rslt[3,cs]), r3)

})

#### check diagnoses that were removed from comorbidities in recent updates
px1 <- data.frame(id = 1,
                  diagnosis = c('O9081', 'O9902', 'O9903'),
                  poa = 1)
px2 <- data.frame(id = 2,
                  diagnosis = c(paste0('M469', seq(4,9)),
                                paste0('M498', seq(0,9))),
                  poa = 1)
pxall <- dplyr::bind_rows(px1, px2)
test_that('Removed diagnoses do not get classified', {
  rslt <- comorb(pxall, idcol='id')
  cs <- comorb_description$comorbidity

  expect_equal(simplify2array(rslt[1,cs]), empty)
  expect_equal(simplify2array(rslt[2,cs]), empty)
})

#### check diagnoses that were added to comorbidities in recent updates.
#### There quite a few of these, so we will just spot check some of them.
## new coag dx in v2023
d1 <- c(paste0('D680', c(0,1,20,21,22,23,29,3,4,9)),
        paste0('D758', c(21,22,28,29,4)))
id1 <- seq_along(d1)
pd1 <- data.frame(id=id1,
                  diagnosis=d1,
                  poa=1)

## new dementia dx in v2023
d2 <- c(paste0('F015', c(11,18,2,3,4)),
        paste0('F01A', c(0,11,18,2,3,4)),
        paste0('F01B', c(0,11,18,2,3,4)),
        paste0('F01C', c(0,11,18,2,3,4)),
        paste0('F028', c(11,18,2,3,4)),
        paste0('F02A', c(0,11,18,2,3,4)),
        paste0('F02B', c(0,11,18,2,3,4)),
        paste0('F02C', c(0,11,18,2,3,4)),
        paste0('F039', c(11,18,2,3,4)),
        paste0('F03A', c(0,11,18,2,3,4)),
        paste0('F03B', c(0,11,18,2,3,4)),
        paste0('F03C', c(0,11,18,2,3,4)),
        paste0('F067', c(0,1))
        )
id2 <- seq_along(d2)
pd2 <- data.frame(id=id2,
                  diagnosis=d2,
                  poa=0)

## valvular disease additions for v.2023.1
d3 <- c('I3481', 'I3489')
id3 <- seq_along(d3)
pd3 <- data.frame(id=id3,
                  diagnosis=d3,
                  poa=1)

## peripheral vascular disease additions for v.2023.1
d4 <- c(
  paste0('I7101', c(0,1,2,9)),
  paste0('I711', c(0,1,2,3)),
  paste0('I712', c(0,1,2,3)),
  paste0('I713', c(0,1,2,3)),
  paste0('I714', c(0,1,2,3)),
  paste0('I715', c(0,1,2)),   #sic
  paste0('I716', c(0,1,2)),
  'I7782'
)
id4 <- seq_along(d4)
pd4 <- data.frame(id=id4,
                  diagnosis=d4,
                  poa=1)   # could also have been poa=0.

## One code was added as both liver_mld and neuro_oth in v.2023.1
pd5 <- data.frame(id=1,
                 diagnosis='K7682',
                 poa=1)

## Autoimmune disorders added in v2022.1
d6 <- c(
  paste0('M311', c(0,1,9)),
  paste0('M350', c(5,6,7,8, 'A', 'B', 'C')),
  paste0('M45A', c(seq(0,8), 'B')),
  'M3211', 'M3581', 'M3589'
)
id6 <- seq_along(d6)
pd6 <- data.frame(id=id6,
                  diagnosis=d6,
                  poa=0)

pd7 <- data.frame(id=1,
                  diagnosis='F32A',
                  poa=0)

pd8 <- data.frame(id=1,
                  diagnosis='C847A',
                  poa=1)
pd9 <- data.frame(id=1,
                  diagnosis='C7963',
                  poa=1)
pd10 <- data.frame(id=1,
                   diagnosis='C563',
                   poa=1)

test_that('Added codes are classified as comorbidities', {
  cs <- comorb_description$comorbidity
  create_rslt <- function(conds, n) {
    r <- empty
    for(c in conds) {
      r[[c]] <- 1
    }
    rr <- as.data.frame(dplyr::bind_rows(rep(list(r), n)))
    row.names(rr) <- as.character(row.names(rr))
    rr
  }

  rslt1 <- comorb(pd1, idcol='id')
  nr1 <- max(pd1$id)
  er1 <- create_rslt('coag', nr1)
  expect_equal(rslt1[cs], er1)

  rslt2 <- comorb(pd2, idcol='id')
  nr2 <- max(pd2$id)
  er2 <- create_rslt('dementia', nr2)
  expect_equal(rslt2[cs], er2)

  rslt3 <- comorb(pd3, idcol='id')
  nr3 <- max(pd3$id)
  er3 <- create_rslt('valve', nr3)
  expect_equal(rslt3[cs], er3)

  rslt4 <- comorb(pd4, idcol='id')
  nr4 <- max(pd4$id)
  er4 <- create_rslt('perivasc', nr4)
  expect_equal(rslt4[cs], er4)

  rslt5 <- comorb(pd5, idcol='id')
  er5 <- create_rslt(c('liver_mld', 'neuro_oth'), 1)
  expect_equal(rslt5[cs], er5)

  rslt6 <- comorb(pd6, idcol='id')
  nr6 <- max(pd6$id)
  er6 <- create_rslt('autoimmune', nr6)
  expect_equal(rslt6[cs], er6)

  rslt7 <- comorb(pd7, idcol='id')
  er7 <- create_rslt('depress', max(pd7$id))
  expect_equal(rslt7[cs], er7)

  rslt8 <- comorb(pd8, idcol='id')
  er8 <- create_rslt('cancer_lymph', max(pd8$id))
  expect_equal(rslt8[cs], er8)

  rslt9 <- comorb(pd9, idcol='id')
  er9 <- create_rslt('cancer_mets', max(pd9$id))
  expect_equal(rslt9[cs], er9)

  rslt10 <- comorb(pd10, idcol='id')
  er10 <- create_rslt('cancer_solid', max(pd10$id))
  expect_equal(rslt10[cs], er10)

})
