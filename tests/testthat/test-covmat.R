# testing the covariance matrix function

# data
data("utah")

c1 <- data.matrix(utah[1:3, 1:2])
c2 <- data.matrix(utah[4:5, 1:2])

# variogram model and parameters
model <- "sph"
nugget <- 0.1184
sill <- 0.3474
range <- 119197

# test for exponential models
test_that("covariance matrix function works for exponential models", {

  k_exp <- exponential(dmatrix = distant(c1, c2), nugget, sill, range)

  k_cov <- covmat(c1, c2, model = "exp", nugget, sill, range)

  expect_equal(round(k_exp, 6), k_cov)
})


# test for spherical models
test_that("covariance matrix function works for spherical models", {

  k_sph <- spherical(dmatrix = distant(c1, c2), nugget, sill, range)

  k_cov <- covmat(c1, c2, model = "sph", nugget, sill, range)

  expect_equal(round(k_sph, 6), k_cov)
})


# test for Gaussian models
test_that("covariance matrix function works for gaussian models", {

  k_gau <- gausian(dmatrix = distant(c1, c2), nugget, sill, range)

  k_cov <- covmat(c1, c2, model = "gau", nugget, sill, range)

  expect_equal(round(k_gau, 6), k_cov)
})
