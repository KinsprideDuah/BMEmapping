# testing the bme_predict function

# data
data("utsnowload")
x <- data.matrix(utsnowload[1, c("latitude", "longitude")])
ch <- data.matrix(utsnowload[2:67, c("latitude", "longitude")])
cs <- data.matrix(utsnowload[68:232, c("latitude", "longitude")])
zh <- c(utsnowload[2:67, c("hard")])
a <- c(utsnowload[68:232, c("lower")])
b <- c(utsnowload[68:232, c("upper")])
bme_predict(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
            sill = 0.3639, range = 1.0787, type = "mean")


# test for posterior mode
test_that("posterior mode function works", {

  k1 <- bme_estimate(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                     sill = 0.3639, range = 1.0787)[1]

  k2 <- bme_predict(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                    sill = 0.3639, range = 1.0787, type = "mode")[3]

  k1 <- data.frame(mode = k1)

  expect_equal(round(k1,3), round(k2,3))
})


# test for posterior mean
test_that("posterior mean function works", {

  k1 <- bme_estimate(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                     sill = 0.3639, range = 1.0787)[2]

  k2 <- bme_predict(x, ch, cs, zh, a, b, model = "exp", nugget = 0.0953,
                    sill = 0.3639, range = 1.0787, type = "mean")[3]

  k1 <- data.frame(mean = k1)

  expect_equal(round(k1,3), round(k2,3))
})
