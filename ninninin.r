rm(list = ls())
phi <- (1 + 5**0.5) / 2
psi <- (1 - 5**0.5) / 2
n <- 12
fib <- (phi**(1:n) - psi**(1:n)) / (phi  - psi)
fib <- as.integer(fib)
r <- fib[2:n] - fib[2]
theta <- r * (pi / max(r)) 
L = -theta - (pi / 2)
R = L + 2 * theta
tab <- data.frame(r, L, R) 
len <- nrow(tab)
blank <- rep(NA, len)
maketab <- function(i, tab) {
    cols <- lapply((2:ncol(tab)), makebranch, i, tab)
    tab <- cbind(tab, data.frame(cols))
    if (i + 1 == len) {
        return(tab)
    } else {
        maketab(i + 1, tab)
    }
}
makebranch <- function(j, i, tab) {
    name <- substr(names(tab[j]), 1, 1)
    if (name == "L") {
        name <- "R"
        node <- i
        sign <- 1
    } else {
        name <- "L"
        node <- i + 1
        sign <- -1
    }
    blank[node:len] <- 
        tab[[j]][node:len] + sign * 2 * theta[1:(len - (node - 1))]
    blank <- data.frame(blank)
    names(blank) <- name
    return(blank)
}
tab <- maketab(2, tab)

par(mar = c(0, 0, 0, 0))
plot(c(r, -r), c(r, -r), type = "n")
for (col in tab[2:ncol(tab)]) {
    x <- tab$r * cos(col)
    y <- tab$r * sin(col)
    lines(x, y)
}
