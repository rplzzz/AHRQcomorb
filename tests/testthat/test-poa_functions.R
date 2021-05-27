poatest <- c('Y', 'N', '1', '0', 'U', 'W', 'Z', 'X',  # values found in CMS
             'T', 'F', 'true', 'false',               # variants of true and false
             'fnord', '', ' ')                        # garbage values

test_that('poa_cms works', {
  expect_equal(poa_cms(poatest), c(1L, rep(0L, length(poatest)-1)))
})

test_that('poa_cms2 works', {
  expect_equal(poa_cms2(poatest),
               c(1L, 0L, rep(NA_integer_, length(poatest)-2)))
})

test_that('poa_natural works', {
  expect_equal(poa_natural(poatest),
               as.integer(c(1, 0, 1, 0, NA, NA, NA, NA,
                            1, 0, 1, 0,
                            0, NA, NA)))   # "fnord" gets detected as "false". Oh, well.
})
