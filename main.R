# Title     : GLM Benchmarks
# Objective : Benchmark the time R takes to estimate GLM models of various size and obs count
# Created by: jonlachmann
# Created on: 2021-01-29

# Set up grids for number of variables and observations to examine
obs_grid <- seq(100,10000,100)
var_grid <- seq(2,100,5)

# Generate dummy data
data <- as.data.frame(matrix(rnorm(max(obs_grid)*max(var_grid)), max(obs_grid), max(var_grid)))

# Create a matrix to store the results in
time_results <- matrix(NA, length(obs_grid), length(var_grid))

# Do benchmark
for (obs in 1:length(obs_grid)) {
  for (var in 1:length(var_grid)) {
    time_results[obs, var] <- system.time(
      glm(V1 ~ ., family = gaussian(), data=data[1:obs_grid[obs], 1:var_grid[var]])
    )[3]
    # Print something so we know if we are getting there or not
    print(paste("obs:", obs, "var:", var))
  }
}

# Setup column names for the results matrix
colnames(time_results) <- var_grid
rownames(time_results) <- obs_grid

# Create a plot of the benchmark
library(plotly)
fig <- plot_ly(z = time_results, x=var_grid ,y=obs_grid)
fig <- fig %>% add_surface()
fig <- fig %>% layout(
    title = "Computation time for a GLM",
    scene = list(
      xaxis = list(title = "Variables"),
      yaxis = list(title = "Observations"),
      zaxis = list(title = "Time")
    ))

fig

# Create observations count mean plot
time_results.obs <- rowMeans(time_results)

plot(y=time_results.obs, x=names(time_results.obs), type="l", ylim=c(0,0.15))
abline(0, 0.000012)

