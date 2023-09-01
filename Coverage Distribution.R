setwd("C:/Users/39327/Desktop/Uni/Tesi/R scripts")

coverage_dist <- function(alpha, n) {
    x <- seq(0, 1, length = 100000)
    for (i in 1:length(n)) {
        l <- (n[i] + 1) * alpha
        shape1 <- n[i] + 1 - l
        shape2 <- l
        if (i == 1) {
            plot(x, dbeta(x, shape1, shape2), type = "l", lwd = 2, col = i,
                 xlim = c(0.83, 0.97), ylim = c(0, 150), yaxt = "n",
                 ylab = "", xlab = "")
        }
        else {
            lines(x, dbeta(x, shape1, shape2), type = "l", lwd = 2, col = i)
        }
    }
    abline(v = 1 - alpha, lwd = 2, lty = "dashed", col = "gray")
}
n <- c(100, 1000, 10000)
coverage_dist(0.1, n)
legend("topleft", legend = c("n = 100", "n = 1000", "n = 10000", "1 - α"),
       col = c(1:3, "gray"), lwd = 2, lty = c(rep(1, 3), 2), cex = 0.75,
       bty = "n")
