set.seed(1)
t = seq(0, 5, 0.05)
x1 = sort(sample(t, 12))
y1 = 1 + x1*0.4 + rnorm(12, 0, 0.8)
x2 = sort(sample(t, 12))
y2 = 1.8 + x2*0.6 + rnorm(12, 0, 0.8)
x3 = sort(sample(t, 12))
y3 = 2.6 + x3*0.8 + rnorm(12, 0, 0.8)
x4 = sort(sample(t, 12))
y4 = 3.2 + x4 + rnorm(12, 0, 0.8)
  

par(las = 1, yaxt = "n", mar = c(4, 1, 1, 1))

# random intercepts

plot(rbind(cbind(x1, y1), cbind(x2, y2), cbind(x3, y3),cbind(x4, y4)), 
     xlab = "t", ylab = "", type = "p", bty = "l",
     pch = rep(c(15:18), each = 12), col = rep(c("black", "red", "yellow3", "green4"), each = 12),
     lwd = 2)
abline(1.2, 0, lwd = 2.5, lty = 2, col = "black")
abline(3.5, 0, lwd = 2.5, lty = 2, col = "red")
abline(5, 0, lwd = 2.5, lty = 2, col = "yellow3")
abline(6, 0, lwd = 2.5, lty = 2, col = "green4")

# random intercepts, fixed slope
plot(rbind(cbind(x1, y1), cbind(x2, y2), cbind(x3, y3),cbind(x4, y4)), 
     xlab = "t", ylab = "", type = "p", bty = "l",
     pch = rep(c(15:18), each = 12), col = rep(c("black", "red", "yellow3", "green4"), each = 12),
     lwd = 2)
abline(0.6, 0.5, lwd = 2.5, lty = 2, col = "black")
abline(1.8, 0.5, lwd = 2.5, lty = 2, col = "red")
abline(2.8, 0.5, lwd = 2.5, lty = 2, col = "yellow3")
abline(4, 0.5, lwd = 2.5, lty = 2, col = "green4")


# random intercepts, random slope
plot(rbind(cbind(x1, y1), cbind(x2, y2), cbind(x3, y3),cbind(x4, y4)), 
     xlab = "t", ylab = "", type = "p", bty = "l",
     pch = rep(c(15:18), each = 12), col = rep(c("black", "red", "yellow3", "green4"), each = 12),
     lwd = 2)
abline(1, 0.4, lwd = 2.5, lty = 2, col = "black")
abline(1.8, 0.6, lwd = 2.5, lty = 2, col = "red")
abline(2.6, 0.8, lwd = 2.5, lty = 2, col = "yellow3")
abline(3.2, 1, lwd = 2.5, lty = 2, col = "green4")
