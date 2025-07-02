# testing the covariance matrix function

# data
data("utsnowload")
c1 <- utsnowload[1:3, c("latitude", "longitude")]
c2 <- utsnowload[4:5, c("latitude", "longitude")]

# variogram model and parameters
model <- "exp"
nugget <- 0.0953
sill <- 0.3639
range <- 1.0787


# test for exponential models
test_that("covariance matrix function works for exponential models", {

  k_exp <- exponential(dmatrix = distant(c1, c2), nugget, sill, range)

  k_cov <- covmat(c1, c2, model = "exp", nugget, sill, range)

  expect_equal(round(k_exp, 4), k_cov)
})


# test for spherical models
test_that("covariance matrix function works for spherical models", {

  k_sph <- spherical(dmatrix = distant(c1, c2), nugget, sill, range)

  k_cov <- covmat(c1, c2, model = "sph", nugget, sill, range)

  expect_equal(round(k_sph, 4), k_cov)
})


# test for Gaussian models
test_that("covariance matrix function works for gaussian models", {

  k_gau <- gausian(dmatrix = distant(c1, c2), nugget, sill, range)

  k_cov <- covmat(c1, c2, model = "gau", nugget, sill, range)

  expect_equal(round(k_gau, 4), k_cov)
})
