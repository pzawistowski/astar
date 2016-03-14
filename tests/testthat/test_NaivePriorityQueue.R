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

test_that("supports finding elements", {
  pq <- NaivePriorityQueue$new()

  pq$push(list(n='A',idx=1),1)
  pq$push(list(n='B',idx=1),2)
  pq$push(list(n='C',idx=1),3)

  filter <- function(el) el$n == 'B'

  expect_equal(pq$find(filter), 2)
})



test_that("supports finding elements - multidimensional case", {
  pq <- NaivePriorityQueue$new()

  pq$push(c(1,0), 1)
  pq$push(c(0,1),2)
  pq$push(c(1,1),3)

  filter <- function(el) identical(el, c(0,1))

  expect_equal(pq$find(filter), 2)
})


test_that("return NA is element is not found", {
  pq <- NaivePriorityQueue$new()

  filter <- function(el) el == 'B'

  expect_equal(pq$find(filter), NA)

})



test_that("supports getting elements", {
  pq <- NaivePriorityQueue$new()

  pq$push('A',10)
  pq$push('B',20)
  pq$push('C',30)


  expect_equal(pq$get(3), 'C')
  expect_equal(pq$getValue(3), 30)

})



test_that("supports removing elements", {
  pq <- NaivePriorityQueue$new()

  pq$push('A',10)
  pq$push('B',20)
  pq$push('C',30)

  pq$remove(2)
  expect_equal(pq$get(2), 'C')
  expect_equal(pq$getValue(2), 30)

  pq$remove(1)
  expect_equal(pq$get(1), 'C')
  expect_equal(pq$getValue(1), 30)
})
