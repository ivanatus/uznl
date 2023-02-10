rm(list = ls())
setwd("~/Desktop/UZnL")
library(trajr)
library(mapview)
dz10data <- read.csv('dz10data.csv',header = TRUE, sep = ';')
coords <- data.frame(x = c(dz10data$LOCATION.Latitude..), y = c(dz10data$LOCATION.Longitude..), times = c(dz10data$YYYY.MO.DD.HH.MI.SS_SSS))
trj <- trajr::TrajFromCoords(coords)
mapview(coords, xcol = "x", ycol = "y", crs = 4269, grid = FALSE)
plot(trj)
trj <- TrajGenerate(200, random = TRUE, angularErrorSd = .25)
# Plot original trajectory
plot(trj, lwd = 1, lty = 1)
# Create a smoothed trajectory, filter order 3, length 31
smoothed <- TrajSmoothSG(trj, p = 3, n = 31)
# Plot it in slightly transparent red
lines(smoothed, col = "#FF0000A0", lwd = 2)
legend("topright", c("Original", "Smoothed"), lwd = c(1, 2), lty = c(1, 1), col = c("black", "red"), inset = 0.01)
# Resampling trajectories
# trajr provides functions to resample a trajectory
# to a fixed step length or a fixed step time.
trj <- TrajGenerate(10, stepLength = 2)
# Plot original trajectory with dots at trajectory coordinates
plot(trj, lwd = 2)
points(trj, draw.start.pt = FALSE, pch = 16, col = "black", cex = 1.2)
# Resample to step length 1
resampled <- TrajRediscretize(trj, 1)
# Plot rediscretized trajectory in red
lines(resampled, col = "#FF0000A0", lwd = 2)
points(resampled, type = 'p', col = "#FF0000A0", pch = 16)
legend("topright", c("Original", "Rediscretized"), col = c("black", "red"),lwd = 2, inset = c(0.01, 0.02))
# Generate trajectory with a point every 2 hours and
# highly variable speed (which equates to
# step length)
trj <- TrajGenerate(10, stepLength = 1, fps = .5, timeUnits = "hours", linearErrorSd = .8)
# Plot original trajectory with dots at trajectory
# coordinates
plot(trj, lwd = 2)
points(trj, draw.start.pt = FALSE, pch = 16, col = "black", cex = 1.2)
# Resample to 1 hourly steps
resampled <- TrajResampleTime(trj, 1)
# Plot rediscretized trajectory in red
lines(resampled, col = "#FF0000A0", lwd = 2)
points(resampled, type = 'p', col = "#FF0000A0", pch = 16)
legend("topright", c("Original", "Resampled"), col = c("black", "red"), lwd = 2, inset = c(0.01, 0.02))
## Trajectory analysis
# Velocity analysis
trj <- TrajGenerate()
# Smooth before calculating derivatives
smoothed <- TrajSmoothSG(trj, 3, 101)
# Calculate speed and acceleration
derivs <- TrajDerivatives(smoothed)
# Plot acceleration and speed
plot(derivs$acceleration ~ derivs$accelerationTimes, type = 'l', col = 'red', yaxt = 'n', xlab = 'Time (s)', ylab = expression(paste('Acceleration (', m/s^2, ')')))
axis(side = 2, col = "red")
lines(derivs$speed ~ derivs$speedTimes, col = 'blue')
axis(side = 4, col = "blue")
mtext('Speed (m/s)', side = 4, line = 3)
abline(h = 0, col = 'lightGrey')
# The flying bee simulation
# Simulate a flying bee
trj <- TrajGenerate(100, angularErrorSd = 0.3, stepLength = 0.001)
# Smooth the trajectory
trj <- TrajSmoothSG(trj, 3, 51)
# Calculate hovering intervals
intervals <- TrajSpeedIntervals(trj, slowerThan = 2)
print(intervals)
# Plot speed over time with hovering intervals highlighted
plot(intervals)
# Analysing straightness
# Measure of efficiency of a direct walk.
# The simplest is D/L , where D is the distance
# from the start to the end of the trajectory,
# and L is the length of the trajectory.
# The straightness index is calculated by the function
# TrajStraightness, and is a number ranging from 0 to 1,
# where 1 indicates a straight line.
# Generate some trajectories for use in examples
n <- 100
# Random magnitude of angular errors
angularErrorSd <- runif(n, 0, 2)
# Generate some trajectories with varying angular errors
trjs <- lapply(1:n, function(i) TrajGenerate(500, stepLength = 2, angularErrorSd = angularErrorSd[i]))
# Rediscretize each trajectory to a range of step sizes
stepSizes <- c(1, 2, 10)
reds <- lapply(stepSizes, function(ss) lapply(1:n, function(i) TrajRediscretize(trjs[[i]], ss)))
# Calculate straightness (D/L) for all of the rediscretised trajectories
ds <- sapply(reds, function(rtrjs) sapply(1:n, function(i) TrajStraightness(rtrjs[[i]])))
# Calculate alternate straightness (r) for all of the rediscretised trajectories
rs <- sapply(reds, function(rtrjs) sapply(1:n, function(i) Mod(TrajMeanVectorOfTurningAngles(rtrjs[[i]]))))
# Plot both indices on the same graph
plot(rep(angularErrorSd, 3), rs, pch = 16, cex = .8, col = c(rep('red', n), rep('blue', n), rep('darkgreen', n)), xlab = expression(sigma[Delta]), ylab = "Straightness", ylim = range(c(ds, rs)))
points(rep(angularErrorSd, 3), ds, pch = 3, cex = .8,  col = c(rep('red', n), rep('blue', n), rep('darkgreen', n)))
legend("bottomleft", c(expression(italic(r)), "D/L", paste("Step length", stepSizes)), pch = c(16, 3, 16, 16), col = c("black", "black", "red", "blue", "darkgreen"), inset = 0.01)
# Sinuosity
# The sinuosity index may be an appropriate measure of the tortuosity of a
# random search path. Sinuosity is a function of the mean cosine of turning angles.
# Calculate sinuosity for all of the rediscretised trajectories
sins <- sapply(reds, function(rtrjs) sapply(1:n, function(i) TrajSinuosity2(rtrjs[[i]])))
# Plot sinuosity vs angular error
plot(rep(angularErrorSd, 3), sins,
     pch = 16, cex = .8,
     col = c(rep('red', n), rep('blue', n), rep('darkgreen', n)),
     xlab = expression(sigma["angular error"]), ylab = "Sinuosity")
legend("bottomright", paste("Step length", stepSizes),
       pch = 16, col = c("red", "blue", "darkgreen"), inset = 0.01)
# Sinuosity
# The sinuosity index may be an appropriate measure of the tortuosity of a
# random search path. Sinuosity is a function of the mean cosine of turning angles.
# Calculate sinuosity for all of the rediscretised trajectories
sins <- sapply(reds, function(rtrjs) sapply(1:n, function(i) TrajSinuosity2(rtrjs[[i]])))
# Plot sinuosity vs angular error
plot(rep(angularErrorSd, 3), sins, pch = 16, cex = .8, col = c(rep('red', n), rep('blue', n), rep('darkgreen', n)), xlab = expression(sigma["angular error"]), ylab = "Sinuosity")
legend("bottomright", paste("Step length", stepSizes), pch = 16, col = c("red", "blue", "darkgreen"), inset = 0.01)
# Fractal dimension
# Use the same range of step sizes for all trajectories
stepSizes <- TrajLogSequence(.1, 7, 10)
# Fractal dimension is a slow calculation, so just plot a subset
# of trajectories from the previous example
fn <- n / 4
use <- sample.int(fn, n = length(angularErrorSd))
fangularErrorSd <- angularErrorSd[use]
# Calculate fractal dimension for all of the rediscretized trajectories
d <- sapply(reds, function(rtrjs) sapply(use, function(i) {TrajFractalDimension(rtrjs[[i]], stepSizes)}))
# Plot fractal dimension vs angular error
plot(rep(fangularErrorSd, 3), d, pch = 16, cex = .8, col = c(rep('red', fn), rep('blue', fn), rep('darkgreen', fn)), xlab = expression(sigma["angular error"]), ylab = "Fractal dimension")
legend("topleft", c("Step length 1", "Step length 2", "Step length 10"), pch = 16, col = c("red", "blue", "darkgreen"), inset = 0.01)