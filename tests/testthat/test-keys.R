
id1 <- c('a','b','c')
id2 <- c('d','e','f')
x <- c(1,2,3)
y <- c(13,23,42)
synthid <- c('a\037d', 'b\037e', 'c\037f')

separate <- data.frame(id1=id1, id2=id2, x=x, y=y, stringsAsFactors = FALSE)
merged <- data.frame(id=synthid, x=x, y=y, stringsAsFactors = FALSE)

nodrop <- data.frame(id=synthid, id1=id1, id2=id2, x=x, y=y, stringsAsFactors = FALSE)

test_that('Merging keys works', {
  expect_equal(mergekeys(separate, c('id1','id2')), merged)
  expect_equal(mergekeys(separate, c('id1','id2'), drop=FALSE), nodrop)
})

test_that('Separating keys works', {
  expect_equal(splitkeys(merged, c('id1','id2')), separate)
  expect_equal(splitkeys(merged, c('id1','id2'), drop=FALSE), nodrop)
})
