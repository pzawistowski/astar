context("Priority queue")

test_that("poping an empty queue returns NA", {
  pq <- NaivePriorityQueue$new()

  expect_equal(pq$pop(), NA)
})


test_that("poping a queue with a single element returns that element and removes it from the queue", {
  pq <- NaivePriorityQueue$new()
  pq$push(1,2)

  expect_equal(pq$pop(), 1)
  expect_equal(pq$pop(), NA)
})



test_that("the queue pops the elements according to ther increasing value", {
  pq <- NaivePriorityQueue$new()

  pq$push('B',2)
  pq$push('E',5)
  pq$push('A',1)
  pq$push('D',4)
  pq$push('C',3)
  res <- sapply(1:5, function(i) pq$pop())

  expect_equal(res, c('A','B','C','D','E'))
})


test_that("handles duplicates", {
  pq <- NaivePriorityQueue$new()

  pq$push('A',2)
  pq$push('A',2)
  pq$push('B',4)
  pq$push('C',4)

  expect_equal(c(pq$pop(),pq$pop()), c('A','A'))
  expect_equal(c(pq$pop(),pq$pop()), c('B','C'))
})



test_that("handles complex values", {
  pq <- NaivePriorityQueue$new()

  pq$push(1,10)
  pq$push(c('A','B'),1)
  pq$push(list(1,2,3),3)
  pq$push(list(A=1,B=2),2)

  res <- pq$pop()
  expect_equal(res, c('A','B'))
  expect_equal(pq$pop(), list(A=1,B=2))
  expect_equal(pq$pop(), list(1,2,3))
  expect_equal(pq$pop(), 1)
})
