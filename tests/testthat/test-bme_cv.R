# testing the prob_zk function (dimensions)

# data
data("utsnowload")
ch <- utsnowload[2:10, c("latitude", "longitude")]
cs <- utsnowload[68:232, c("latitude", "longitude")]
zh <- utsnowload[2:10, c("hard")]
a <- utsnowload[68:232, c("lower")]
b <- utsnowload[68:232, c("upper")]

# test for bme_cv mean
test_that("bme CV function works", {

  data_object <- bme_map(ch, cs, zh, a , b)

  k1 <- bme_cv(data_object = data_object, model = "exp",
               nugget = 0.0953, sill = 0.3639, range = 1.0787,
               zk_range = c(-1.5, 2.5), type = "mean")

  k2 <- c(nrow(ch), 7)

  expect_equal(dim(k1), k2)
})


# test for bme_cv mode
test_that("bme CV function works", {

  data_object <- bme_map(ch, cs, zh, a , b)

  k3 <- bme_cv(data_object = data_object, model = "exp",
               nugget = 0.0953, sill = 0.3639, range = 1.0787,
               zk_range = c(-1.5, 2.5), type = "mode")

  k4 <- c(nrow(ch), 6)

  expect_equal(dim(k3), k4)
})


