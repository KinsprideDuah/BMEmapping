# testing the bme_predict function

# data
data("utah")

x <- data.matrix(utah[1, c("lat", "lon")])
ch <- data.matrix(utah[2:67, c("lat", "lon")])
cs <- data.matrix(utah[68:232, c("lat", "lon")])
zh <- c(utah[2:67, c("center")])
a <- c(utah[68:232, c("lower")])
b <- c(utah[68:232, c("upper")])

# variogram model and parameters
model <- "sph"
nugget <- 0.1184
sill <- 0.3474
range <- 119197

# additional parameters
nsmax <- 5
nhmax <- 10

# test for posterior mode
test_that("posterior mode function works", {

  k1 <- bme_estimate(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax,
                     nhmax)[1]

  k2 <- bme_predict(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax,
                    nhmax, type = "mode")[3]

  k1 <- data.frame(mode = k1)

  expect_equal(k1, k2)
})


# test for posterior mean
test_that("posterior mean function works", {

  k1 <- bme_estimate(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax,
                     nhmax)[2]

  k2 <- bme_predict(x, ch, cs, zh, a, b, model, nugget, sill, range, nsmax,
                    nhmax, type = "mean")[3]

  k1 <- data.frame(mean = k1)

  expect_equal(round(k1, 5), round(k2, 5))
})
