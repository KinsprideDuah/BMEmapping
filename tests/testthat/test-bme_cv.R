# testing the CV function

# data
data("utah")

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


# test for posterior mean
test_that("bme_cv works", {

  k1 <- bme_mean_loocv(ch, cs, zh, a, b, model, nugget, sill, range, nsmax,
                       nhmax)[,4]

  k2 <- bme_cv(ch, cs, zh, a, b, model, nugget, sill, range, nsmax, nhmax,
               type = "mean")[,4]

  expect_equal(round(k1, 2), round(k2, 2))
})


