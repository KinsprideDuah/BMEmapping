# testing the prob_zk function (dimensions)

# data
data("utsnowload")
x <- utsnowload[1, c("latitude", "longitude")]
ch <- utsnowload[2:67, c("latitude", "longitude")]
cs <- utsnowload[68:232, c("latitude", "longitude")]
zh <- utsnowload[2:67, c("hard")]
a <- utsnowload[68:232, c("lower")]
b <- utsnowload[68:232, c("upper")]

# test for prob_zk function (dimensions)
test_that("prob_zk function (dimensions)", {

  data_object <- bme_map(ch, cs, zh, a , b)

  k1 <- prob_zk(x, data_object = data_object, model = "exp",
                nugget = 0.0953, sill = 0.3639, range = 1.0787,
                zk_range = c(-1.5, 2.5))

  k2 <- c(50, 2)

  expect_equal(dim(k1), k2)
})


