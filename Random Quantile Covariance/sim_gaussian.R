library(devtools)
library(spmodel)
library(gstat)
library(sp)
library(BMEmapping)
load_all()

#-------------------------------------------------------------------#
# Set up simulation space #
#-------------------------------------------------------------------#
set.seed(123)
n <- 625
x_coords <- runif(n, 0, 100)
y_coords <- runif(n, 0, 100)
grid <- data.frame(x = x_coords, y = y_coords)
coords <- grid
coordinates(grid) <- ~x + y  # Convert to spatial object

#----------------------------------------------------------------------#
# Define spatial model setup #
#----------------------------------------------------------------------#
vgm_model <- vgm(psill = 1, model = "Sph", range = 30, nugget = 0)

# Simulate 1 realization of a spatial field
g.dummy <- gstat(formula = z ~ 1, locations = ~x + y, dummy = TRUE,
                 beta = 0, model = vgm_model, nmax = 50)
sim <- predict(g.dummy, newdata = grid, nsim = 20)
sim_count <- ncol(sim)

#---------------------------------------------------------------------------#
# Simulation config and functions #
#---------------------------------------------------------------------------#

summarize_results <- function(results_list) {
  avg_matrix <- Reduce("+", lapply(results_list, function(df) {
    df_num <- as.data.frame(lapply(df[, c("bias", "MAE", "RMSE")], as.numeric))
    matrix(as.vector(unlist(df_num)), ncol = 3)
  })) / length(results_list)

  df <- data.frame(
    method    = c("old", "", "", "new", "", "", "variance"),
    pred_type = c("mode", "mean", "", "mode", "mean", "", ""),
    rbind(round(avg_matrix[1:2, ], 4), "", round(avg_matrix[3:4, ], 4),
          "", round(avg_matrix[5,], 2))
  )
  colnames(df) <- c("method", "pred_type", "bias", "MAE", "MSE")
  df[is.na(df)] <- ""
  return(df)
}

#---------------------------------------------------------------#
# Main simulation run #
#---------------------------------------------------------------#
q_list <- list(c(0.05, 0.25), c(0.4, 0.6), c(0.75, 0.95))
l_values <- c(1, 3, 5)
results_all <- list()

for (l in l_values) {
  results_l <- list()

  for (q in q_list) {
    results_q <- list()

    for (j in 1:sim_count) {
      mu <- 0; beta <- 1

      # Apply t-quantile function to standard normal
      z_gaussian <- sim@data[,j]
      gaussian_data <- cbind(coords, z = z_gaussian)

      # Partition the spatial data
      nh <- 1:125; ns <- 126:525; nk <- 526:625
      ch <- gaussian_data[nh, c("x", "y")]; zh <- gaussian_data[nh, "z"]
      cs <- gaussian_data[ns, c("x", "y")]; zs <- gaussian_data[ns, "z"]
      ck <- gaussian_data[nk, c("x", "y")]; zk <- gaussian_data[nk, "z"]

      # Fit variogram to training data
      df_vg <- gaussian_data[nh, ]
      coordinates(df_vg) <- ~x + y
      vg <- gstat::variogram(z ~ 1, data = df_vg)
      vg_model_fit <- fit.variogram(vg, model = vgm(c("Exp", "Sph")))

      # Extract model parameters
      model_type <- as.character(vg_model_fit[2, 1])
      nugget <- vg_model_fit[1, 2]
      sill <- vg_model_fit[2, 2]
      range <- vg_model_fit[2, 3]

      # Generate prediction intervals
      p <- runif(length(zs), q[1], q[2])
      #ln <- abs(rnorm(length(zs), mean = l, sd = 0.4))
      ln <- runif(length(zs), l - 0.5, l + 0.5)
      a <- zs - ln * p
      b <- zs + ln * (1 - p)

      # report variance of zs and mean of ln
      vs <- var(zs)
      m_ln <- mean(ln)

      # Perform BME estimation
      zk_bme <- bme_estimate(x = ck, ch, cs, zh, a, b, model_type, nugget,
                             sill, range)[, 1:2]
      zk_new <- new_bme_estimate(x = ck, ch, cs, zh, a, b, nq = 8)[, 1:2]

      # Define performance metrics
      metrics <- function(pred) {
        res <- zk - pred
        c(bias = mean(res), MAE = mean(abs(res)), MSE = mean(res^2))
      }

      DF1 <- t(sapply(split(zk_bme, col(zk_bme)), metrics))
      DF2 <- t(sapply(split(zk_new, col(zk_new)), metrics))
      DF <- rbind(DF1, DF2, c(0, vs, m_ln))

      # Store for this simulation
      results_q[[j]] <- DF
    }

    # Summarize across simulations for each q
    q_label <- paste0("q", paste(q, collapse = "_"))
    results_l[[q_label]] <- summarize_results(results_q)
  }

  # Store per l
  l_label <- paste0("l", l)
  results_all[[l_label]] <- results_l
}

#-----------------------------------------------------------------------------#
# Access summarized result example: #
#-----------------------------------------------------------------------------#
print(results_all)


# Create a directory to save results (optional)
dir.create("Gaussian_Sph_results_unif", showWarnings = FALSE)

# Loop through and write each summary to CSV
for (l_name in names(results_all)) {
  for (q_name in names(results_all[[l_name]])) {
    df <- results_all[[l_name]][[q_name]]
    file_name <- paste0("Gaussian_Sph_results_unif/", l_name, "_", q_name, ".csv")
    write.csv(df, file = file_name, row.names = FALSE)
  }
}



