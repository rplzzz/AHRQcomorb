
clmall <- readRDS(here::here('tests','testthat','wide-test-data.rds'))
cllall <- readRDS(here::here('tests','testthat','long-test-data.rds'))

test_that('Single claim pivot works correctly', {
  clm1 <- clmall[1,]
  cl1_l <- claimpivot(clm1, c('pid','cid'))

  cl1_gold <- cllall[cllall$id == 1,]
  cl1_gold$id <- paste(clm1$pid, clm1$cid, sep=intToUtf8(31))

  expect_equal(cl1_l, cl1_gold)

  ## Check that we get the same thing with na.fail
  cl1_l2 <- claimpivot(clm1, c('pid','cid'), nahandle = na.fail)
  expect_equal(cl1_l2, cl1_l)
})


test_that('Multiple claim pivot works correctly', {
  expect_equal(claimpivot(clmall, 'cid'), dplyr::rename(cllall, cid='id'))

})

test_that('NA handling in POA works correctly', {
  clm1 <- clmall[1,]
  clm1$CLM_POA_IND_SW10 <- NULL
  cl1_l1 <- expect_silent(claimpivot(clm1, 'cid'))
  cl1_gold <- dplyr::rename(cllall[cllall$id == 1,], cid='id')
  cl1_gold$poa[cl1_gold$diagnosis=='Z6825'] <- NA
  expect_equal(cl1_l1, cl1_gold)

  expect_error(claimpivot(clm1, 'cid', nahandle = na.fail))

  cl1_l2 <- expect_silent(claimpivot(clm1, 'cid', nahandle = na.omit))
  expect_equal(cl1_l2, cl1_gold[cl1_gold$diagnosis != 'Z6825',], ignore_attr=TRUE)
})
