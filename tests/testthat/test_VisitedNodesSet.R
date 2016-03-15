context("Visited nodes set")

test_that("allows adding nodes and filtering neighbours", {
  fn <- VisitedNodesSet$new(2)

  fn$add(1)
  fn$add(2)
  fn$add(3)

  expect_equal(fn$filterNeighbours(1:10), 4:10)
})



test_that("allows getting last node visited", {
  fn <- VisitedNodesSet$new(2)

  fn$add(1)
  fn$add(2)

  expect_equal(fn$last(), 2)

  fn$add(3)

  expect_equal(fn$last(), 3)
})
