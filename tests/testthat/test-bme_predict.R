# testing the bme_predict function

# data
data("utsnowload")
x <- utsnowload[1, c("latitude", "longitude")]
ch <- utsnowload[2:67, c("latitude", "longitude")]
cs <- utsnowload[68:232, c("latitude", "longitude")]
zh <- utsnowload[2:67, c("hard")]
a <- utsnowload[68:232, c("lower")]
b <- utsnowload[68:232, c("upper")]

# test for posterior mode
test_that("posterior mode function works", {

  data_object <- bme_map(ch, cs, zh, a , b)

  k1 <- bme_estimate(x, data_object = data_object, model = "exp",
                     nugget = 0.0953, sill = 0.3639, range = 1.0787,
                     zk_range = c(-1.5, 2.5))[1]

  k2 <- bme_predict(x, data_object = data_object, model = "exp",
                    nugget = 0.0953, sill = 0.3639, range = 1.0787,
                    zk_range = c(-1.5, 2.5), type = "mode")[[3]]

  expect_equal(round(k1,2), round(k2,2))
})


# test for posterior mean
test_that("posterior mean function works", {

  data_object <- bme_map(ch, cs, zh, a , b)

  k1 <- bme_estimate(x, data_object = data_object, model = "exp",
                     nugget = 0.0953, sill = 0.3639, range = 1.0787,
                     zk_range = c(-1.5, 2.5))[2]

  k2 <- bme_predict(x, data_object = data_object, model = "exp",
                    nugget = 0.0953, sill = 0.3639, range = 1.0787,
                    zk_range = c(-1.5, 2.5), type = "mean")[[3]]

  expect_equal(round(k1,2), round(k2,2))
})
