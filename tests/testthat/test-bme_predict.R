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

  k1 <- bme_estimate(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                     sill = 0.3639, range = 1.0787)[1]

  k2 <- bme_predict(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                    sill = 0.3639, range = 1.0787, type = "mode")[[3]]

  expect_equal(round(k1,2), round(k2,2))
})


# test for posterior mean
test_that("posterior mean function works", {

  k1 <- bme_estimate(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                     sill = 0.3639, range = 1.0787)[2]

  k2 <- bme_predict(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                    sill = 0.3639, range = 1.0787, type = "mean")[[3]]

  expect_equal(round(k1,2), round(k2,2))
})
