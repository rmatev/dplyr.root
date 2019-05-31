context("tbl-root")
library(dplyr)

filename <- system.file("examples/exampleEs.root", package = "RootTreeToR")
data <- tbl_rootchain(filename, 'exampleEs')

test_that("filter works", {
  expect_identical(
    filter(collect(data), eCh > 0, px > 0),
    collect(filter(data, eCh > 0, px > 0))
  )
})

test_that("globals don't mask data variables", {
  pz <- function() { stop("This should never be evaluated") }
  data <- collect(filter(data, pz > 180))
  expect_true(T)
})

test_that("empty collect works", {
  res <- collect(filter(data, pz > 1000))
  expect_equal(nrow(res), 0)
})

test_that("select works", {
  expect_named(
    collect(select(data, pz, px)),
    c("pz", "px")
  )
})

test_that("mutate works", {
  expect_identical(
    collect(mutate(data, pt = sqrt(px^2 + py^2))),
    mutate(collect(data), pt = sqrt(px^2 + py^2))
  )
})

test_that("nse works", {
  cuts <- list(eCh=0, px=50)
  expect_identical(
    filter(collect(data), eCh > cuts$eCh, px > cuts$px),
    collect(filter(data, eCh > cuts$eCh, px > cuts$px))
  )
})
