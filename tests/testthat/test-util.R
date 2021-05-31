test_that("intor works", {
  expect_equal(intor(rep(1,5)), 1L)
  expect_equal(intor(rep(0,5)), 0L)
  expect_equal(intor(c(0,1,0,1,0)), 1L)
  expect_equal(intor(numeric()), 0L)
})

test_that("pintand works", {
  x <- c(1,1,0,0)
  y <- c(1,0,1,0)
  expect_equal(pintand(x,y), c(1L,0L,0L,0L))

  x <- as.integer(rnorm(10) > 0)
  expect_equal(pintand(x,1), as.integer(x))
  expect_equal(pintand(x,0), rep(0L, length(x)))
})
