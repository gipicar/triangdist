test_that("dtriang returns correct densities", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_equal(dtriang(1, 0, 1, 0.5), 0)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)  # fuera del rango
  expect_error(dtriang(0.5, 1, 0, 0.5))    # min >= max
  expect_error(dtriang(0.5, 0, 1, 2))      # mode fuera de rango
})

test_that("ptriang returns correct probabilities", {
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
  expect_true(all(ptriang(c(0.2, 0.8), 0, 1, 0.5) >= 0))
  expect_error(ptriang(0.5, 1, 0, 0.5))
  expect_error(ptriang(0.5, 0, 1, 2))
})

test_that("qtriang returns correct quantiles", {
  expect_equal(qtriang(0, 0, 1, 0.5), 0)
  expect_equal(qtriang(1, 0, 1, 0.5), 1)
  expect_error(qtriang(1.5, 0, 1, 0.5))   # p fuera de [0,1]
  expect_error(qtriang(0.5, 1, 0, 0.5))
})

test_that("rtriang generates correct number of values", {
  set.seed(42)
  r <- rtriang(100, 0, 1, 0.5)
  expect_length(r, 100)
  expect_true(all(r >= 0 & r <= 1))
  expect_error(rtriang(10, 1, 0, 0.5))
})

test_that("edge case: mode == min", {
  expect_no_error(dtriang(0, 0, 1, 0))
  expect_no_error(ptriang(0.5, 0, 1, 0))
})

test_that("qtriang validates mode", {
  expect_error(qtriang(0.5, 0, 1, 2))
})

test_that("rtriang validates mode", {
  expect_error(rtriang(10, 0, 1, 2))
})
