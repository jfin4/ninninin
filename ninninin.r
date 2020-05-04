rm(list = ls())
phi <- (1 + 5**0.5) / 2
psi <- (1 - 5**0.5) / 2
n <- 12
fib <- (phi**(1:n) - psi**(1:n)) / (phi  - psi)
fib <- as.integer(fib)
r <- seq(fib[2], fib[n]) - fib[2]
ncurve <- log(fib[n], base = phi)
curve <- phi**seq(0, ncurve, length.out = length(r))
theta <- r * (curve / max(curve)) * (pi / max(r)) 
L = -theta - (pi / 2)
R = L + 2 * theta
i <- rep(NA, length(r))
i[fib[2:n]] <- 2:n
tab <- data.frame(i, r, L, R) 
len <- nrow(tab)
par(mar = c(0, 0, 0, 0))
plot(c(r, -r), c(r, -r), type = "n")
for (i in 1:len) {
    seg <- tab[i:(i + 1), ]
    for (j in 3:ncol(tab)) {
        r <- seg[, 2]
        twig <- seg[, j]
        x <- r * cos(twig)
        y <- r * sin(twig)
        lines(x, y)
    }
}
